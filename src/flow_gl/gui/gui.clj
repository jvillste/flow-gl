(ns flow-gl.gui.gui
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
                         [events :as events]
                         [layoutable :as layoutable])
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
        midje.sweet
        clojure.test))
;; utils

(defn update-or-apply-in [map path function & arguments]
  (if (seq path)
    (apply update-in map path function arguments)
    (apply function map arguments)))

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

(defn add-control-channel-to-view-state [constructor]
  (fn [view-context & parameters]
    (let [view-context (assoc view-context
                         :control-channel (async/chan))]
      (-> (apply constructor
                 view-context
                 parameters)
          (assoc :control-channel (:control-channel view-context))))))

(defn call-destructors [view-state destructor-decorator]
  (let [destructor (destructor-decorator
                    (or (:destructor view-state)
                        identity))]
    (destructor view-state)
    (doseq [child-view-state (vals (:child-states view-state))]
      (call-destructors child-view-state destructor-decorator))))

(defn call-destructors-when-close-requested [app]
  (fn [state event]
    (let [state (app state event)]
      (when (= (:type event) :close-requested)
        (call-destructors (:view-state state) (:destructor-decorator state)))
      state)))

(defn close-control-channel-beforehand [destructor]
  (fn [state]
    (when-let [control-channel (:control-channel state)]
      (async/close! control-channel))
    (destructor state)))

(def control-channel-feature {:constructor-decorator add-control-channel-to-view-state
                              :destructor-decorator close-control-channel-beforehand
                              :application-decorator call-destructors-when-close-requested})

(defn add-event-channel [view-context]
  (assoc view-context
    :event-channel (window/event-channel (-> view-context :application-state :window))))

(defn apply-view-state-applications-beforehand [app]
  (fn [state event]
    (let [state (if (= (:type event)
                       :apply-to-view-state)
                  ((:function event) state)
                  state)]
      (app state event))))

(def apply-to-view-state-feature {:view-context-decorator add-event-channel
                                  :application-decorator apply-view-state-applications-beforehand})

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

(defn path-prefixes [path]
  (loop [prefixes []
         prefix []
         xs path]
    (if-let [x (first xs)]
      (let [prefix (conj prefix x)]
        (recur (conj prefixes prefix)
               prefix
               (rest xs)))
      prefixes)))

(fact (path-prefixes [[1 2] [3] [4]])
      => [[[1 2]]
          [[1 2] [3]]
          [[1 2] [3] [4]]])

(defn layout-path-to-handlers [layout-path layout handler-key]
  (->> (map (fn [path]
              (when-let [handler (-> (get-in layout path)
                                     (handler-key))]
                handler))
            (path-prefixes layout-path))
       (filter identity)
       (reverse)))

(deftest layout-path-to-handlers-test
  (is (= '(:handler2 :handler1 :handler3)
         (mapcat #(layout-path-to-handlers %
                                           {:a
                                            {:b
                                             {:c
                                              {:handler :handler1
                                               :d
                                               {:handler :handler2}}
                                              :c2 {:handler :handler3}}}}

                                           :handler)
                 [[:a :b :c :d] [:a :b :c2]]))))


(defn apply-layout-event-handlers [state layout layout-paths handler-key & arguments]
  (if layout-paths
    (let [handlers (apply concat (mapcat #(layout-path-to-handlers % layout handler-key)
                                         layout-paths))]
      (if-let [handler (first handlers)]
        (apply handler
               state
               arguments)
        state))
    state))

(defn apply-layout-event-handlers-beforehand [app]
  (fn [state event]
    (let [state (if (and (:layout state)
                         (= (:source event)
                            :mouse))
                  (apply-layout-event-handlers state (:layout state) (:layout-paths-under-mouse state) :handle-mouse-event event)
                  state)]
      (app state event))))

;; mouse api

(defn add-mouse-event-handler [layoutable handler]
  (assoc layoutable
    :handle-mouse-event (conj (or (:handle-mouse-event layoutable)
                                  [])
                              handler)))

(defn add-mouse-event-handler-with-context [layoutable view-context handler]
  (add-mouse-event-handler layoutable (fn [state event]
                                        (update-or-apply-in state (:state-path view-context) handler event))))

(defn on-mouse-clicked [layoutable view-context handler & arguments]
  (add-mouse-event-handler-with-context layoutable view-context (fn [state event]
                                                                  (if (= :mouse-clicked (:type event))
                                                                    (apply handler state event arguments)
                                                                    state))))

(defn on-mouse-event [layoutable event-type view-context handler & arguments]
  (add-mouse-event-handler-with-context layoutable view-context (fn [state event]
                                                                  (if (= event-type (:type event))
                                                                    (apply handler state event arguments)
                                                                    state))))

;; Keyboard

(defn apply-keyboard-event-handlers [state event]
  (loop [state state
         focused-state-paths (:focused-state-paths state)]
    (if-let [focused-state-path (first focused-state-paths)]
      (if-let [keyboard-event-handler (get-in state (conj (vec focused-state-path) :handle-keyboard-event))]
        (if (seq focused-state-path)
          (let [[child-state continue] (keyboard-event-handler (get-in state focused-state-path)
                                                               event)
                state (assoc-in state focused-state-path child-state)]
            (if continue
              (recur state
                     (rest focused-state-paths))
              state))
          (let [[state continue] (keyboard-event-handler state event)]
            (if continue
              (recur state
                     (rest focused-state-paths))
              state)))
        (recur state
               (rest focused-state-paths)))
      state)))

(defn apply-keyboard-event-handlers-beforehand [app]
  (fn [state event]
    (let [state (if (and (:focused-state-paths state)
                         (= (:source event)
                            :keyboard))
                  (apply-keyboard-event-handlers state event)
                  state)]
      (app state event))))

(defn layout-path-to-state-paths [root-layout child-path]
  (loop [state-paths (if-let [state-path (:state-path root-layout)]
                       [state-path]
                       [])
         rest-of-child-path child-path
         child-path []]
    (if-let [child-path-part (first rest-of-child-path)]
      (let [child-path (conj child-path child-path-part)]
        (if-let [new-state-path (get-in root-layout (conj child-path :state-path))]
          (recur (conj state-paths new-state-path)
                 (rest rest-of-child-path)
                 child-path)
          (recur state-paths
                 (rest rest-of-child-path)
                 child-path)))
      state-paths)))

(defn set-hierarchical-state [state paths new-value child-state-key state-key state-gained-key state-lost-key]
  (if (seq paths)
    (loop [paths paths
           state state]
      (if (seq (rest paths))
        (recur (rest paths)
               (update-in state (first paths) assoc child-state-key new-value))
        (update-in state (first paths) (fn [state]
                                         (let [focus-handler-key (if new-value state-gained-key state-lost-key)]
                                           (-> (if-let [focus-handler (focus-handler-key state)]
                                                 (focus-handler state)
                                                 state)
                                               (assoc state-key new-value)))))))
    state))

(defn move-hierarchical-state [state paths previous-path-parts-key child-state-key state-key state-gained-key state-lost-key]
  (-> state
      (set-hierarchical-state (previous-path-parts-key state) false child-state-key state-key state-gained-key state-lost-key)
      (set-hierarchical-state paths true child-state-key state-key state-gained-key state-lost-key)
      (assoc previous-path-parts-key paths)))

(defn set-focus [state focus-paths]
  (move-hierarchical-state state focus-paths :focused-state-paths :child-has-focus :has-focus :on-focus-gained :on-focus-lost))

(defn set-focus-on-mouse-click-beforehand [app]
  (fn [state event]
    (let [state (if (and (= (:source event)
                            :mouse)
                         (= (:type event)
                            :mouse-clicked))
                  (let [state-paths-under-mouse (layout-path-to-state-paths (:layout state)
                                                                            (first (:layout-paths-under-mouse state)))]
                    (if (get-in state (concat (last state-paths-under-mouse)
                                              [:can-gain-focus]))
                      (set-focus state state-paths-under-mouse)
                      state))
                  state)]
      (app state event))))

;; moving focus

(defn seq-focus-handlers [child-seq-key]
  {:first-focusable-child (fn [_] [child-seq-key 0])
   :next-focusable-child (fn [this [_ child-index]]
                           (let [new-child-index (inc child-index)]
                             (if (= new-child-index
                                    (count (child-seq-key this)))
                               nil
                               [child-seq-key (inc child-index)])))})


(def child-focus-handlers
  {:first-focusable-child (fn [state]
                            [:child-states (first (:children state))])
   :next-focusable-child (fn [this currently-focused-path-part]
                           (let [child-index (first (positions #{currently-focused-path-part} (map #(conj [:child-states] %)
                                                                                                   (:children this))))]
                             (let [new-child-index (inc child-index)]
                               (if (= new-child-index
                                      (count (:children this)))
                                 nil
                                 [:child-states (get (:children this)
                                                     new-child-index)]))))})

(defn initial-focus-paths [state]
  (loop [state state
         focus-paths [[:view-state]]]
    (if-let [focus-path-part (when-let [first-focusable-child-function (:first-focusable-child state)]
                               (first-focusable-child-function state))]
      (recur (get-in state focus-path-part)
             (conj focus-paths focus-path-part))
      focus-paths)))

(fact (initial-focus-paths {:first-focusable-child (fn [_] [:children 1])
                            :children [{:first-focusable-child (fn [_] [:children 0])
                                        :children [{}]}
                                       {:first-focusable-child (fn [_] [:children 0])
                                        :children [{}]}]})
      => [[:children 1] [:children 0]])

(defn next-focus-path-parts [state previous-focus-path-parts]
  (when (seq previous-focus-path-parts)
    (let [parent-focus-path-parts (vec (drop-last previous-focus-path-parts))
          focus-parent (get-in state (apply concat parent-focus-path-parts))]
      (if-let [next-focus-path-part ((:next-focusable-child focus-parent) focus-parent (last previous-focus-path-parts))]
        (concat parent-focus-path-parts
                [next-focus-path-part]
                (initial-focus-paths (get-in state
                                             (apply concat
                                                    (conj parent-focus-path-parts
                                                          next-focus-path-part)))))
        (next-focus-path-parts state parent-focus-path-parts)))))

(fact (let [state (conj (seq-focus-handlers :children1)
                        {:children1 [(conj (seq-focus-handlers :children2)
                                           {:children2 [{}{}{}]})
                                     (conj (seq-focus-handlers :children2)
                                           {:children2 [{}{}(conj (seq-focus-handlers :children3)
                                                                  {:children3 [{}{}{}]})]})]})]

        (next-focus-path-parts state [[:children1 0] [:children2 1]])  => [[:children1 0] [:children2 2]]
        (next-focus-path-parts state [[:children1 0] [:children2 2]])  => [[:children1 1] [:children2 0]]
        (next-focus-path-parts state [[:children1 1] [:children2 1]])  => [[:children1 1] [:children2 2] [:children3 0]]
        (next-focus-path-parts state [[:children1 1] [:children2 2] [:children3 2]])  => nil))

(defn move-focus-on-tab-beforehand [app]
  (fn [state event]
    (let [state (if (events/key-pressed? event :tab)
                  (set-focus state (or (next-focus-path-parts state (:focused-state-paths state))
                                       (initial-focus-paths state)))
                  state)]
      (app state event))))


;; Cache

(defn propagate-only-when-view-state-changed [app]
  (fn [state event]

    (let [state (if (and (:view-state state)
                         (identical?  (:old-view-state state)
                                      (:view-state state)))
                  state
                  (app state event))]

      (assoc state :old-view-state (:view-state state)))))

;; App

(defn reset-children [state]
  (-> state
      (assoc :old-children (or (:children state)
                               []))
      (assoc :children [])))

(defn remove-unused-children [state view-context]
  (let [child-set (set (:children state))
        children-to-be-removed (->> (:old-children state)
                                    (filter (complement child-set)))]
    (reduce (fn [state child-to-be-removed]
              (do (call-destructors (get-in state [:child-states child-to-be-removed])
                                    (-> view-context :application-state :destructor-decorator))
                  (update-in state [:child-states] dissoc child-to-be-removed)))
            state
            children-to-be-removed)))

(defn wrap-with-remove-unused-children [view]
  (fn [view-context state]
    (let [view-result (view view-context
                            (reset-children state))]
      (update-in view-result [:state] remove-unused-children view-context))))

(def ^:dynamic current-view-state-atom)

(defn wrap-with-current-view-state-atom [view]
  (fn [view-context state]
    (binding [current-view-state-atom (atom state)]
      (let [layoutable (view view-context state)]
        {:layoutable layoutable
         :state @current-view-state-atom}))))

(defn start-app [app]
  (-> {}
      (add-window)
      (assoc :constructor-decorator (comp add-control-channel-to-view-state))


      (assoc :view-context-decorator (comp add-event-channel))

      (assoc :view-decorator (comp wrap-with-current-view-state-atom
                                   wrap-with-remove-unused-children))

      (assoc :destructor-decorator (comp close-control-channel-beforehand))

      (event-loop (-> app
                      (add-layout-afterwards)
                      #_(propagate-only-when-view-state-changed)
                      (apply-view-state-applications-beforehand)
                      (apply-layout-event-handlers-beforehand)
                      (apply-keyboard-event-handlers-beforehand)
                      #_(move-focus-on-tab-beforehand)
                      (set-focus-on-mouse-click-beforehand)
                      (add-layout-paths-under-mouse-beforehand)
                      (close-when-requested-beforehand)
                      (call-destructors-when-close-requested)
                      (wrap-with-separate-events)
                      (wrap-with-sleep-time-atom)
                      (limit-frames-per-second-afterwards 60)
                      #_(add-drawables-for-layout-afterwards)
                      (transform-layout-to-drawables-afterwards)
                      (render-drawables-afterwards)
                      (wrap-with-close-window-on-exception)))))

(defn control-to-application [constructor]
  (fn [application-state event]
    (let [root-view-context ((:view-context-decorator application-state)
                             {:state-path [:view-state]
                              :application-state application-state})

          constructor ((:constructor-decorator application-state)
                       constructor)

          state (or (:view-state application-state)
                    (constructor root-view-context))

          #_application-state #_(set-focus application-state
                                           (initial-focus-paths state))

          view ((-> application-state :view-decorator)
                (:view state))

          view-result (view root-view-context
                            state)]

      (assoc application-state
        :view-state (:state view-result)
        :layoutable (assoc (:layoutable view-result) :state-path [:view-state])))))

(defn start-control [control]
  (start-app (control-to-application control)))


;; control api



(defn create-apply-to-view-state-event [function]
  {:type :apply-to-view-state
   :function function})

(defn apply-to-state [view-context function & arguments]
  (async/go (async/>! (:event-channel view-context)
                      (create-apply-to-view-state-event (fn [state]
                                                          (if (get-in state (:state-path view-context))
                                                            (apply update-or-apply-in state (:state-path view-context) function arguments)
                                                            (flow-gl.debug/debug-timed "tried to apply to empty state" (vec (:state-path view-context)))))))))

(defn call-view
  ([parent-view-context constructor child-id]
     (call-view parent-view-context constructor child-id {} []))

  ([parent-view-context constructor child-id state-overrides]
     (call-view parent-view-context constructor child-id state-overrides []))

  ([parent-view-context constructor child-id state-overrides constructor-parameters]

     (let [state-path-part [:child-states child-id]
           state-path (concat (:state-path parent-view-context) state-path-part)

           constructor ((-> parent-view-context :application-state :constructor-decorator)
                        constructor)

           view-context ((-> parent-view-context :application-state :view-context-decorator)
                         (assoc parent-view-context
                           :state-path state-path))

           state (-> (or (get-in @current-view-state-atom state-path-part)
                         (apply constructor view-context
                                constructor-parameters))
                     (conj state-overrides))

           view ((-> parent-view-context :application-state :view-decorator)
                 (:view state))

           {:keys [state layoutable]} (view view-context
                                            state)]

       (swap! current-view-state-atom assoc-in state-path-part state)
       (swap! current-view-state-atom update-in [:children] conj child-id)
       (assoc layoutable
         :state-path state-path))))
