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

;; Events

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

(defn wrap-with-separate-events [app]
  (fn [state events]
    (reduce app
            state
            events)))

;; Window

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

(defn wrap-with-close-window-on-exception [app]
  (fn [state events]
    (try
      (app state events)
      (catch Exception e
        (window/close (:window state))
        (throw e)))))

;; CSP

(defn close-control-channels-afterwards-when-close-requested [app]
  (fn [state event]
    (let [state (app state event)]
      (when (= (:type event) :close-requested)
        (gui/close-control-channels (:view-state state)))
      state)))

(defn apply-view-state-applications-beforehand [app]
  (fn [state event]
    (let [state (if (= (:type event)
                       :apply-to-view-state)
                  ((:function event) state)
                  state)]
      (app state event))))

;; Rendering

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

(defn add-drawables-for-layout-afterwards [app]
  (fn [state events]
    (let [state (app state events)]
      (assoc state :drawables (drawables-for-layout (:layout state))))))

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

;; Animation

(defn set-wake-up [view-context sleep-time]
  (swap! (get-in view-context [:application-state :sleep-time-atom])
         (fn [old-sleep-time]
           (if old-sleep-time
             (if sleep-time
               (min old-sleep-time sleep-time)
               old-sleep-time)
             sleep-time))))

(defn limit-frames-per-second-afterwards [app target-frames-per-second]
  (fn [state events]
    (let [previous-sleep-time (:sleep-time state)
          state (-> state
                    (assoc :frame-started (System/currentTimeMillis))
                    (dissoc :sleep-time)
                    (app events))]

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

(defn wrap-with-sleep-time-atom [app]
  (fn [state event]
    (let [sleep-time-atom (atom (:sleep-time state))
          state (app (assoc state
                       :sleep-time-atom sleep-time-atom)
                     event)]
      (assoc state :sleep-time @sleep-time-atom))))

;; Layout

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

;; Mouse

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

;; Keyboard

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

;; App

(defn start-app [app]
  (-> {}
      (add-window)
      (event-loop (-> app
                      (add-layout-afterwards)
                      (apply-view-state-applications-beforehand)
                      (apply-layout-event-handlers-beforehand)
                      (apply-keyboard-event-handlers-beforehand)
                      (set-focus-by-mouse-click-beforehand)
                      (add-layout-paths-under-mouse-beforehand)
                      (close-when-requested-beforehand)
                      (close-control-channels-afterwards-when-close-requested)
                      (wrap-with-separate-events)
                      (wrap-with-sleep-time-atom)
                      (limit-frames-per-second-afterwards 60)
                      #_(add-drawables-for-layout-afterwards)
                      (transform-layout-to-drawables-afterwards)
                      (render-drawables-afterwards)
                      (wrap-with-close-window-on-exception)))))

;; Controls

#_(defn add-control-channel [view]
  (fn [view-context state]
    (let [control-channel (async/chan)
          view-result (view (assoc view-context
                              :control-channel control-channel)
                            state)]
      (assoc-in view-result (concat [:state] (:state-path view-context) [:control-channel]) control-channel))))

(defn control-to-application [constructor view]
  (fn [application-state event]
    (let [control-channel (async/chan)
          root-view-context {:state-path [:view-state]
                             :application-state application-state
                             :control-channel control-channel
                             :event-channel (window/event-channel (:window application-state))}

          view-result (view root-view-context
                            (or (:view-state application-state)
                                (-> (constructor root-view-context)
                                    (assoc :control-channel control-channel))))]

      (assoc application-state
        :view-state (:state view-result)
        :layoutable (assoc (:layoutable view-result) :state-path [:view-state])))))

;; Control test

(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))

(def initial-counter-state {:count 0
                            :can-gain-focus true
                            :handle-keyboard-event (fn [state event]
                                                     [(update-in state [:count] inc)
                                                      true])})

(gui/def-control counter-control
  ([view-context]
     (assoc initial-counter-state
       :animation-started (get-in view-context [:application-state :frame-started])))

  ([view-context state]
     (let [duration (mod (get-in view-context [:application-state :frame-started])
                         (:pulse-rate state))]

       (set-wake-up view-context (- (/ (:pulse-rate state)
                                       2)
                                    duration))
       (text (str (:count state) (if (> (/ duration
                                           (:pulse-rate state))
                                        0.5)
                                   "x"
                                   ""))
             (if (:has-focus state)
               [255 255 255 255]
               [100 100 100 255])))))

(gui/def-control app-control
  ([view-context]
     (async/go-loop []
       (async/alt! (:control-channel view-context) ([_] (println "exiting counter process"))
                   (async/timeout 2000) ([_]
                                           (gui/apply-to-state view-context update-in [:count] inc)
                                           (recur))))

     initial-counter-state)

  ([view-context state]
     (l/vertically (text (str "count " (:count state)))
                   (counter-control view-context :child-1 {:pulse-rate 1000})
                   (counter-control view-context :child-2 {:pulse-rate 500})
                   #_(transformer/with-transformers
                       (transformer/->Filter :fade1
                                             quad/alpha-fragment-shader-source
                                             [:1f "alpha" 0.3])
                       (text (:focused-state-paths state))))))

;; App test

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
  (start-app (control-to-application create-app-control app-control-view)))
