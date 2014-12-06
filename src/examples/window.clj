(ns examples.window
  (:require [clojure.core.async :as async]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad :as quad])
            [datomic.api :as d]
            (flow-gl.gui [drawable :as drawable]
                         [transformer :as transformer]
                         [renderer :as renderer]
                         [window :as window]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            [flow-gl.debug :as debug])
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable]
           [java.nio ByteBuffer])
  (:use flow-gl.utils
        clojure.test))


(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))

(defn event-loop [initial-state app]
  (loop [state initial-state]
    (if (:close-requested state)
      (window/close (:window state))

      (let [events (csp/drain (window/event-channel (:window state))
                              (:sleep-time state))]

        (recur (app state
                    (if (empty? events)
                      [{:type :wake-up}]
                      events)))))))

(defn wrap-with-close-window-on-exception [app]
  (fn [state events]
    (try
      (app state events)
      (catch Exception e
        (window/close (:window state))
        (throw e)))))

(defn wrap-with-separate-events [app]
  (fn [state events]
    (reduce app
            state
            events)))

(defn add-window [state]
  (assoc state :window (jogl-window/create 300
                                           400
                                           :profile :gl3
                                           :init opengl/initialize
                                           :reshape opengl/resize)))

(defn close-when-requested-beforehand [app]
  (fn [state event]
    (if (= (:type event) :close-requested)
      (assoc state :close-requested true)
      (app state event))))

(defn close-control-channels-afterwards-when-close-requested [app]
  (fn [state event]
    (let [state (app state event)]
      (when (= (:type event) :close-requested)
        (gui/close-control-channels (:view-state state)))
      state)))

(defn drawables-for-layout
  ([layout]
     (drawables-for-layout layout 0 0 0 []))

  ([layout parent-x parent-y parent-z drawables]
     (if (:children layout)
       (let [parent-x (+ parent-x (:x layout))
             parent-y (+ parent-y (:y layout))
             parent-z (+ parent-z (or (:z layout) 0))]
         (loop [drawables drawables
                children (:children layout)]
           (if-let [child (first children)]
             (let [drawables (drawables-for-layout child parent-x parent-y parent-z drawables)]
               (recur drawables
                      (rest children)))
             drawables)))
       (conj drawables
             (assoc layout
               :x (+ parent-x (:x layout))
               :y (+ parent-y (:y layout))
               :z (+ parent-z (or (:z layout) 0)))))))

(defn transform-layout-to-drawables-afterwards [app]
  (fn [state events]
    (let [state (app state events)
          [transformer-states drawables] (let [render-trees (transformer/render-trees-for-layout (:layout state))]
                                           (window/with-gl (:window state) gl
                                             (transformer/transform-trees (or (:transformer-states state)
                                                                              {})
                                                                          render-trees
                                                                          gl)))]
      (assoc state
        :drawables drawables
        :transformer-states transformer-states))))

(defn add-drawables-for-layout-afterwards [app]
  (fn [state events]
    (let [state (app state events)]
      (assoc state :drawables (drawables-for-layout (:layout state))))))

(defn render-drawables-afterwards [app]
  (fn [state events]
    (let [state (if (:renderers state)
                  state
                  (assoc state :renderers (window/with-gl (:window state) gl
                                            [(renderer/create-quad-renderer gl)
                                             (renderer/create-quad-view-renderer gl)
                                             (renderer/create-nanovg-renderer)])))
          state (app state events)]

      (window/with-gl (:window state) gl
        (opengl/clear gl 0 0 0 1)
        (renderer/render-frame (:drawables state)
                               gl
                               (:renderers state)))
      (window/swap-buffers (:window state))

      state)))

(defn limit-frames-per-second-afterwards [app target-frames-per-second]
  (fn [state events]
    (let [previous-sleep-time (:sleep-time state)
          state (app state events)]

      (if (:sleep-time state)
        (let [minimum-sleep-time (/ 1000 target-frames-per-second)
              previous-frame-duration (- (System/currentTimeMillis)
                                         (or (:last-frame state)
                                             (System/currentTimeMillis)))
              sleep-time (max (:sleep-time state)
                              (- minimum-sleep-time
                                 (max 0
                                      (- previous-frame-duration
                                         (or previous-sleep-time
                                             0)))))]
          (assoc state
            :sleep-time sleep-time
            :last-frame (System/currentTimeMillis)))
        state))))

(defn add-layout-afterwards [app]
  (fn [state event]
    (let [state (app state event)
          width (window/width (:window state))
          height (window/height (:window state))
          [state layout] (layout/layout (:layoutable state)
                                        state
                                        width
                                        height)
          layout (-> layout
                     (assoc :x 0
                            :y 0
                            :width width
                            :height height)
                     (layout/add-out-of-layout-hints))]
      (assoc state :layout layout))))

(defn add-layout-paths-under-mouse-beforehand [app]
  (fn [state event]
    (if (and (:layout state)
             (= (:source event)
                :mouse))
      (-> state
          (assoc :layout-paths-under-mouse (reverse (layout/layout-paths-in-coordinates (:layout state) (:x event) (:y event))))
          (app event))
      (app state event))))

(defn apply-layout-event-handlers-beforehand [app]
  (fn [state event]
    (let [state (if (and (:layout state)
                         (= (:source event)
                            :mouse))
                  (gui/apply-layout-event-handlers-3 state (:layout state) (:layout-paths-under-mouse state) :handle-mouse-event-2 event)
                  state)]
      (app state event))))

(defn apply-keyboard-event-handlers-beforehand [app]
  (fn [state event]
    (let [state (if (and (:focused-state-paths state)
                         (= (:source event)
                            :keyboard))
                  (gui/apply-keyboard-event-handlers-2 state event)
                  state)]
      (app state event))))

(defn set-focus-by-mouse-click-beforehand [app]
  (fn [state event]
    (let [state (if (and (= (:source event)
                            :mouse)
                         (= (:type event)
                            :mouse-clicked))
                  (let [state-paths-under-mouse (gui/layout-path-to-state-paths (:layout state)
                                                                                (last (:layout-paths-under-mouse state)))]
                    (println "under mouse" state-paths-under-mouse)
                    (if (get-in state (concat (last state-paths-under-mouse)
                                              [:can-gain-focus]))
                      (gui/set-focus state state-paths-under-mouse)
                      state))
                  state)]
      (app state event))))

(defn start-app [app]
  (-> {}
      (add-window)
      (event-loop (-> app
                      (add-layout-afterwards)
                      (apply-layout-event-handlers-beforehand)
                      (apply-keyboard-event-handlers-beforehand)
                      (set-focus-by-mouse-click-beforehand)
                      (add-layout-paths-under-mouse-beforehand)
                      (close-when-requested-beforehand)
                      (close-control-channels-afterwards-when-close-requested)
                      (wrap-with-separate-events)
                      (limit-frames-per-second-afterwards 60)
                      #_(add-drawables-for-layout-afterwards)
                      (transform-layout-to-drawables-afterwards)
                      (render-drawables-afterwards)
                      (wrap-with-close-window-on-exception)))))


(def initial-counter-state {:count 0
                            :can-gain-focus true
                            :handle-keyboard-event (fn [state event]
                                                     (println (:count state) "got" event )
                                                     [(update-in state [:count] inc)
                                                      true])})
(gui/def-control counter-control
  ([view-context control-channel]
     initial-counter-state)

  ([view-context state]
     (text (:count state) (if (:has-focus state)
                            [255 255 255 255]
                            [100 100 100 255]))))

(gui/def-control app-control
  ([view-context control-channel]
     initial-counter-state)

  ([view-context state]
     (l/vertically (text (str "count " (:count state)))
                   (counter-control :child-1 {})
                   (counter-control :child-2 {})
                   #_(transformer/with-transformers
                       (transformer/->Filter :fade1
                                             quad/alpha-fragment-shader-source
                                             [:1f "alpha" 0.3])
                       (text (:focused-state-paths state))))))

(defn control-to-app [constructor view]
  (fn [app-state event]
    (let [app-state (if (:view-state app-state)
                  app-state
                  (assoc app-state :view-state
                         (let [control-channel (async/chan)
                               event-channel (window/event-channel (:window app-state))
                               root-view-context {:state-path [:view-state]
                                                  :event-channel (window/event-channel (:window app-state))}
                               initial-state (constructor root-view-context
                                                          control-channel)
                               initial-state (binding [gui/current-event-channel (window/event-channel (:window app-state))]
                                               (-> (view root-view-context
                                                         initial-state
                                                         nil)
                                                   :state))
                               
                               initial-state (assoc initial-state :control-channel control-channel)]

                           initial-state)))
          
          
          {:keys [state layoutable sleep-time]} (binding [gui/current-event-channel (window/event-channel (:window app-state))]
                                                       (view {:state-path [:view-state]
                                                              :event-channel (window/event-channel (:window app-state))}
                                                             (:view-state app-state)
                                                             nil))
          
          layoutable (assoc layoutable :state-path [:view-state])]
      
      (assoc app-state
        :view-state state
        :layoutable layoutable
        :sleep-time sleep-time))))


(defn counter-view [state]
  (text (:count state) (if (:has-focus state)
                         [255 255 255 255]
                         [100 100 100 255])))

(defn layout-app [state event]
  (let [state (if (:view-state state)
                state
                (assoc state :view-state
                       (merge initial-counter-state
                              {:child-view-states {:child-1 initial-counter-state
                                                   :child-2 initial-counter-state}})))
        state (if (:focused-state-paths state)
                state
                (assoc state :focused-state-paths [[:view-state] [:child-view-states :child-1]]))]
    (-> state
        (assoc :layoutable (-> (l/vertically (text (get-in state [:view-state :count]))
                                             (-> (counter-view (get-in state [:view-state :child-view-states :child-1]))
                                                 (assoc :state-path [:view-state :child-view-states :child-1]))
                                             (-> (counter-view (get-in state [:view-state :child-view-states :child-2]))
                                                 (assoc :state-path [:view-state :child-view-states :child-2]))
                                             (transformer/with-transformers
                                               (transformer/->Filter :fade1
                                                                     quad/alpha-fragment-shader-source
                                                                     [:1f "alpha" 0.3])
                                               (text (:focused-state-paths state))))

                               (assoc :state-path [:view-state]))))))

(defn start []
  #_(start-app layout-app)
  (start-app (control-to-app create-app-control app-control-view)))
