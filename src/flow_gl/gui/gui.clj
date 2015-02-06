(ns flow-gl.gui.gui
  (:require [clojure.core.async :as async]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad :as quad]
                                 [render-target :as render-target])
            [datomic.api :as d]
            (flow-gl.gui [drawable :as drawable]
                         [transformer :as transformer]
                         [renderer :as renderer]
                         [window :as window]
                         [layout :as layout]
                         [events :as events]
                         [layoutable :as layoutable]
                         [cache :as cache]
                         [quad-view :as quad-view])
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
      (debug/debug-timed-and-return :total (app state events))
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
    (debug/add-event (:type event))
    (let [state (app state event)]
      (when (= (:type event) :close-requested)
        (call-destructors (:view-state state) (-> state :view-context :destructor-decorator)))
      state)))

(defn close-control-channel-beforehand [destructor]
  (fn [state]
    (when-let [control-channel (:control-channel state)]
      (async/close! control-channel))
    (destructor state)))

(def control-channel-feature {:constructor-decorator add-control-channel-to-view-state
                              :destructor-decorator close-control-channel-beforehand
                              :application-decorator call-destructors-when-close-requested})

(defn add-event-channel [state]
  (assoc-in state [:view-context :event-channel]  (window/event-channel (-> state :window))))

(defn apply-view-state-applications-beforehand [app]
  (fn [state event]
    (let [state (if (= (:type event)
                       :apply-to-view-state)
                  ((:function event) state)
                  state)]
      (app state event))))

(def apply-to-view-state-feature {:view-context-decorator add-event-channel
                                  :application-decorator apply-view-state-applications-beforehand})




(def ^:dynamic event-channel-atom (atom nil))

(defn set-event-channel-atom [state]
  (reset! event-channel-atom (-> state :window (window/event-channel)))
  state)

(defn redraw-last-started-view []
  (when-let [event-channel-atom @event-channel-atom]
    (async/put! event-channel-atom {:type :request-redraw})))

;; Rendering

(defn add-renderers [gpu-state]
  (assoc gpu-state :renderers {:quad-view (renderer/create-quad-view-renderer (:gl gpu-state))
                               :quad (renderer/create-quad-renderer (:gl gpu-state))
                               :nanovg (renderer/create-nanovg-renderer)}))

(defn set-position [partition parent-x parent-y parent-z]
  (assoc partition
    :x (+ parent-x (:x partition))
    :y (+ parent-y (:y partition))
    :z (+ parent-z (or (:z partition) 0))))

(defn add-partition [partitions partition parent-x parent-y parent-z is-equal]
  (conj partitions
        (-> (set-position partition parent-x parent-y parent-z)
            (assoc :recurring? is-equal))))

(defn partition-by-differences
  ([layout-1 layout-2]
     (partition-by-differences layout-1 layout-2 0 0 0 []))

  ([layout-1 layout-2 parent-x parent-y parent-z partitions]
     (if (or (= layout-1 layout-2)
             (not (:children layout-1))
             (not= (type layout-1)
                   (type layout-2)))
       (add-partition partitions layout-1 parent-x parent-y parent-z (= layout-1 layout-2))
       (let [parent-x (+ parent-x (:x layout-1))
             parent-y (+ parent-y (:y layout-1))
             parent-z (+ parent-z (or (:z layout-1) 0))]
         (loop [partitions partitions
                children-1 (:children layout-1)
                children-2 (:children layout-2)]
           (if-let [child-1 (first children-1)]
             (let [child-2 (first children-2)
                   partitions (partition-by-differences child-1 child-2 parent-x parent-y parent-z partitions)]
               (recur partitions
                      (rest children-1)
                      (rest children-2)))
             partitions))))))

(defn dirty-partitions
  ([layout-1 layout-2]
     (dirty-partitions layout-1 layout-2 0 0 0 [] []))

  ([layout-1 layout-2 parent-x parent-y parent-z partitions-to-be-redrawn partitions-to-be-cleared]
     (if (= layout-1 layout-2)
       [partitions-to-be-redrawn
        partitions-to-be-cleared]
       (if (and (:children layout-1)
                (= (type layout-1)
                   (type layout-2)))
         (let [parent-x (+ parent-x (:x layout-1))
               parent-y (+ parent-y (:y layout-1))
               parent-z (+ parent-z (or (:z layout-1) 0))]
           (loop [partitions-to-be-redrawn partitions-to-be-redrawn
                  partitions-to-be-cleared partitions-to-be-cleared
                  children-1 (:children layout-1)
                  children-2 (:children layout-2)
                  dirty-layers #{}]
             (if-let [child-1 (first children-1)]
               (if (not (empty? (disj dirty-layers (:z child-1)))) ;; how to test this also for the children before this?
                 (recur (conj partitions-to-be-redrawn child-1)
                        partitions-to-be-cleared
                        (rest children-1)
                        (rest children-2)
                        dirty-layers)
                 (let [child-2 (first children-2)
                       [new-partitions-to-be-redrawn partitions-to-be-cleared] (dirty-partitions child-1
                                                                                                 child-2
                                                                                                 parent-x
                                                                                                 parent-y
                                                                                                 parent-z
                                                                                                 partitions-to-be-redrawn
                                                                                                 partitions-to-be-cleared)
                       dirty-layers (if (= new-partitions-to-be-redrawn partitions-to-be-redrawn)
                                      dirty-layers
                                      (conj dirty-layers (:z child-1)))]


                   (recur new-partitions-to-be-redrawn
                          partitions-to-be-cleared
                          (rest children-1)
                          (rest children-2)
                          dirty-layers)))

               [partitions-to-be-redrawn
                partitions-to-be-cleared])))
         [(conj partitions-to-be-redrawn
                (set-position layout-1 parent-x parent-y parent-z))
          (conj partitions-to-be-cleared
                (set-position layout-2 parent-x parent-y parent-z))]))))


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

(defn layout-to-render-trees [gpu-state layout]
  (assoc gpu-state
    :render-trees (transformer/render-trees-for-layout layout)))

(defn render-trees-to-drawables [gpu-state]
  (let [[gpu-state drawables] (debug/debug-timed-and-return :transform (transformer/transform-trees gpu-state
                                                                                                    (:render-trees gpu-state)))]
    (assoc gpu-state
      :drawables drawables)))

(defn apply-to-renderers [gpu-state function]
  (update-in gpu-state
             [:renderers]
             map-vals
             function))

(defn start-frame [gpu-state]
  (apply-to-renderers gpu-state
                      #(renderer/start-frame % (:gl gpu-state))))

(defn end-frame [gpu-state]
  (apply-to-renderers gpu-state
                      #(renderer/end-frame % (:gl gpu-state))))

(defn render-drawables-with-renderers  [renderers gl drawables]
  (let [[quad-view quad nanovg] (renderer/render-frame-drawables drawables
                                                                 gl
                                                                 [(:quad-view renderers)
                                                                  (:quad renderers)
                                                                  (:nanovg renderers)])]
    {:quad-view quad-view
     :quad quad
     :nanovg nanovg}))


(defn clear [gpu-state]
  (doseq [partition (:partitions-to-be-cleared gpu-state)]
    (opengl/clear-rectangle (:gl gpu-state)
                            (:x partition)
                            (:y partition)
                            (:width partition)
                            (:height partition)
                            0 0 0 1))
  gpu-state)

(defn render-drawables [gpu-state]
  (let [gl (:gl gpu-state)
        {:keys [width height]} (opengl/size gl)
        render-target (if-let [render-target (:render-target gpu-state)]
                        (if (and (= width
                                    (:width render-target))
                                 (= height
                                    (:height render-target)))
                          render-target
                          (do (render-target/delete render-target gl)
                              (render-target/create width height gl)))
                        (render-target/create width height gl))
        gpu-state (render-target/render-to render-target gl
                                           (clear gpu-state)
                                           #_(opengl/clear gl 0 0 0 1)
                                           (update-in gpu-state [:renderers] render-drawables-with-renderers gl (:drawables gpu-state)))]

    (render-target/blit render-target gl)

    (assoc gpu-state
      :render-target render-target)))


(defn layout-to-partitions [gpu-state]
  (let [size (opengl/size (:gl gpu-state))
        [partitions-to-be-redrawn partitions-to-be-cleared] (if (= size (:previous-size gpu-state))
                                                              (dirty-partitions (:layout gpu-state)
                                                                                (:previous-layout gpu-state))
                                                              [[(:layout gpu-state)] []])]

    (assoc gpu-state
      :partitions partitions-to-be-redrawn
      :partitions-to-be-cleared partitions-to-be-cleared
      :previous-layout (:layout gpu-state)
      :previous-size size)))

(defn partition-to-render-trees [gpu-state partition]
  (assoc gpu-state
    :render-trees (transformer/render-trees-for-layout partition)))

(defn add-partition-textures [gpu-state partitions]
  (let [quad-view (get-in gpu-state [:renderers :quad-view :quad-view])
        new-partitions (filter #(not (quad-view/has-texture? quad-view %))
                               partitions)]
    (reduce (fn [gpu-state partition]
              #_(println "partition" (:width partition) (:height partition))
              (let [render-target (render-target/create (max 1 (:width partition))
                                                        (max 1 (:height partition))
                                                        (:gl gpu-state))
                    gpu-state (-> (render-target/render-to render-target
                                                           (:gl gpu-state)
                                                           (opengl/clear (:gl gpu-state)
                                                                         0 1 0 0)
                                                           (-> gpu-state
                                                               (layout-to-render-trees (assoc partition
                                                                                         :x 0 :y 0 :z 0))
                                                               (render-trees-to-drawables)
                                                               (render-drawables)))
                                  (update-in [:renderers :quad-view :quad-view]
                                             quad-view/add-gl-texture
                                             partition
                                             (:texture render-target)
                                             (:width partition)
                                             (:height partition)
                                             (:gl gpu-state)))]
                (render-target/delete render-target (:gl gpu-state))
                gpu-state))
            gpu-state
            new-partitions)))

(defn bake-recurring-partitions [gpu-state]
  (add-partition-textures gpu-state (filter :recurring? (:partitions gpu-state))))

(defn partitions-to-drawables [gpu-state]
  (println "partitions" (map type (:partitions gpu-state)))
  (assoc gpu-state
    :drawables (mapcat (fn [partition]
                         (if (:recurring? partition)
                           [#_(assoc partition
                                :has-predefined-texture true)]
                           (do #_(opengl/clear-rectangle (:gl gpu-state)
                                                         (:x partition)
                                                         (:y partition)
                                                         (:width partition)
                                                         (:height partition)
                                                         0 0 1 1)
                               (concat [(assoc (drawable/->Rectangle (:width partition)
                                                                     (:height partition)
                                                                     [0 0 0 255])
                                          :x (:x partition)
                                          :y (:y partition))]
                                       (drawables-for-layout partition)))))
                       (:partitions gpu-state))))

(defn swap-buffers [gpu-state]
  (window/swap-buffers (:window gpu-state))
  gpu-state)



(defn render-frame [gpu-state]
  (-> gpu-state
      (start-frame)
      (layout-to-partitions)
      #_(bake-recurring-partitions)
      (partitions-to-drawables)
      (render-drawables)
      (end-frame)))

(defn render [gpu-state layout]
  (try
    (-> (assoc gpu-state :layout layout)
        (render-frame)
        (swap-buffers))
    (catch Exception e
      #_(window/close (:window gpu-state))
      (throw e))))

(defn render-drawables-afterwards [app]
  (fn [state events]
    (let [state (app state events)]

      (async/put! (:with-gl-channel state)
                  {:function render
                   :arguments [(:layout state)]})

      state)))

;; Animation

(def ^:dynamic current-view-state-atom)

(defn choose-sleep-time [sleep-time-1 sleep-time-2]
  (if sleep-time-1
    (if sleep-time-2
      (min sleep-time-1 sleep-time-2)
      sleep-time-1)
    sleep-time-2))

(defn set-wake-up [view-context sleep-time]
  (swap! current-view-state-atom
         (fn [view-state]
           (update-in view-state [:sleep-time] choose-sleep-time sleep-time))))

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

#_(defn add-sleep-time-atom [state]
    (assoc-in state [:view-context :sleep-time-atom] (atom nil)))

#_(defn wrap-with-sleep-time-atom [app]
    (fn [state event]
      (reset! (-> state :view-context :sleep-time-atom)
              nil)
      (-> state
          (app event)
          (assoc :sleep-time @(-> state :view-context :sleep-time-atom)))))

;; Layout

(defn add-layout-afterwards [app]
  (fn [state event]
    (let [state (app state event)
          width (window/width (:window state))
          height (window/height (:window state))
          [state layout] (debug/debug-timed-and-return :layout (layout/layout (:layoutable state)
                                                                              state
                                                                              width
                                                                              height))
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
      (reduce (fn [state handler]
                (apply handler
                       state
                       arguments))
              state
              handlers))
    state))

(defn apply-layout-event-handler [state layout layout-paths handler-key & arguments]
  (if layout-paths
    (let [handlers (apply concat (mapcat #(layout-path-to-handlers % layout handler-key)
                                         layout-paths))]
      (if-let [handler (last handlers)]
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

(defn set-mouse-over [state mouse-over-paths]
  (if (not (= mouse-over-paths (:mouse-over-paths state)))
    (move-hierarchical-state state mouse-over-paths :mouse-over-paths :mouse-over-child :mouse-over :on-mouse-enter :on-mouse-leave)
    state))

(defn apply-mouse-over-layout-event-handlers [state layout new-mouse-over-layout-paths]
  (if (not (= new-mouse-over-layout-paths
              (:mouse-over-layout-paths state)))
    (let [old-mouse-over-layout-paths-set (apply hash-set (:mouse-over-layout-paths state))
          new-mouse-over-layout-paths-set (apply hash-set new-mouse-over-layout-paths)]
      (-> state
          (apply-layout-event-handlers layout (clojure.set/difference old-mouse-over-layout-paths-set new-mouse-over-layout-paths-set) :handle-mouse-event {:type :mouse-leave})
          (apply-layout-event-handlers layout (clojure.set/difference new-mouse-over-layout-paths-set old-mouse-over-layout-paths-set) :handle-mouse-event {:type :mouse-enter})
          (assoc :mouse-over-layout-paths new-mouse-over-layout-paths)))
    state))

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

(defn apply-mouse-movement-event-handlers-beforehand [app]
  (fn [state event]
    (let [state (if (and (:layout state)
                         (= (:source event)
                            :mouse)
                         (:type event :mouse-moved))
                  (let [state-paths-under-mouse (layout-path-to-state-paths (:layout state) (first (:layout-paths-under-mouse state)))]
                    (-> state
                        (set-mouse-over state-paths-under-mouse)
                        (apply-mouse-over-layout-event-handlers (:layout state) (:layout-paths-under-mouse state) #_[(first (:layout-paths-under-mouse state))])))

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

(defn wrap-with-cached [view]
  (let [cached-view (cache/cached (with-meta view
                                    {:name (str "view: " (hash view))}))]
    (fn [view-context state]
      (cached-view
       view-context
       state))))

(defn add-cache [state]
  (assoc state :cache (cache/create)))

(defn wrap-with-cache [app]
  (fn [state events]
    (cache/with-cache (-> state :cache)
      (cache/clear-usages)
      (let [state (app state events)]
        (cache/remove-unused)
        state))))

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
                                    (-> view-context :common-view-context :destructor-decorator))
                  (update-in state [:child-states] dissoc child-to-be-removed)))
            state
            children-to-be-removed)))

(defn wrap-with-remove-unused-children [view]
  (fn [view-context state]
    (let [view-result (view view-context
                            (reset-children state))]
      (update-in view-result [:state] remove-unused-children view-context))))



(defn wrap-with-current-view-state-atom [view]
  (fn [view-context state]
    (binding [current-view-state-atom (atom state)]
      (let [layoutable (view view-context state)]
        {:layoutable layoutable
         :state @current-view-state-atom}))))

(defn initialize-gpu-state [window]
  (window/with-gl window gl
    (-> {:window window
         :gl gl
         :previous-partitions #{}}
        (add-renderers))))

(defn event-loop [initial-state app]
  (let [initial-state (assoc initial-state
                        :with-gl-channel (async/chan 50)
                        :with-gl-dropping-channel (async/chan (async/dropping-buffer 1)))]
    (async/thread (loop [state initial-state]
                    (if (:close-requested state)
                      (do (println "exiting event loop")
                          (async/close! (:with-gl-channel initial-state))
                          (async/close! (:with-gl-dropping-channel initial-state))
                          (window/close (:window state)))

                      (let [events (csp/drain (window/event-channel (:window state))
                                              (:sleep-time state))

                            events (if (empty? events)
                                     [{:type :wake-up}]
                                     events)]

                        (recur (app state
                                    events))))))

    (loop [gpu-state (initialize-gpu-state (:window initial-state))]
      (async/alt!! (:with-gl-dropping-channel initial-state) ([{:keys [function arguments]}]
                                                                (when function (recur (window/with-gl (:window initial-state) gl
                                                                                        (apply function
                                                                                               (assoc gpu-state :gl gl)
                                                                                               arguments)))))

                   (:with-gl-channel initial-state) ([{:keys [function arguments]}]
                                                       (when function (recur (window/with-gl (:window initial-state) gl
                                                                               (apply function
                                                                                      (assoc gpu-state :gl gl)
                                                                                      arguments)))))
                   :priority true))

    (println "exiting render loop")))

(defn start-app [app]
  (-> {}
      (add-window)
      (add-cache)
      (set-event-channel-atom)
      (add-event-channel)
      #_(add-sleep-time-atom)
      (assoc-in [:view-context :constructor-decorator] (comp add-control-channel-to-view-state))

      (assoc-in [:view-context :view-decorator]  (comp wrap-with-cached
                                                       wrap-with-remove-unused-children
                                                       wrap-with-current-view-state-atom))

      (assoc-in [:view-context :destructor-decorator]  (comp close-control-channel-beforehand))

      (event-loop (-> app
                      (add-layout-afterwards)
                      #_(propagate-only-when-view-state-changed)
                      (apply-view-state-applications-beforehand)
                      (apply-mouse-movement-event-handlers-beforehand)
                      (apply-layout-event-handlers-beforehand)
                      (apply-keyboard-event-handlers-beforehand)
                      #_(move-focus-on-tab-beforehand)
                      (set-focus-on-mouse-click-beforehand)
                      (add-layout-paths-under-mouse-beforehand)
                      (close-when-requested-beforehand)
                      (call-destructors-when-close-requested)
                      (wrap-with-separate-events)
                      (wrap-with-cache)
                      #_(wrap-with-sleep-time-atom)
                      (limit-frames-per-second-afterwards 1)
                      (render-drawables-afterwards)
                      (wrap-with-close-window-on-exception)))))

(defn control-to-application [constructor]
  (fn [application-state event]
    (let [root-view-context {:state-path [:view-state]
                             :common-view-context (:view-context application-state)}

          constructor ((-> root-view-context :common-view-context :constructor-decorator)
                       constructor)

          state (or (:view-state application-state)
                    (constructor root-view-context))

          root-view-context (if (or (:sleep-time state)
                                    (not (contains? application-state :view-state)))
                              (assoc root-view-context :frame-started (:frame-started application-state))
                              root-view-context)

          state (dissoc state :sleep-time)

          #_application-state #_(set-focus application-state
                                           (initial-focus-paths state))

          state (if (:decorated-view state)
                  state
                  (assoc state :decorated-view ((-> root-view-context :common-view-context :view-decorator)
                                                (:view state))))

          view-result ((:decorated-view state)
                       root-view-context
                       state)]
      (assoc application-state
        :view-state (:state view-result)
        :layoutable (assoc (:layoutable view-result) :state-path [:view-state])
        :sleep-time (:sleep-time (:state view-result))))))

(defn start-control [control]
  (start-app (control-to-application control)))


;; control api

(defn create-apply-to-view-state-event [function]
  {:type :apply-to-view-state
   :function function})

(defn apply-to-state [view-context function & arguments]

  (async/go (async/>! (-> view-context :common-view-context :event-channel)
                      (create-apply-to-view-state-event (fn [state]
                                                          (if (get-in state (:state-path view-context))
                                                            (apply update-or-apply-in state (:state-path view-context) function arguments)
                                                            (flow-gl.debug/debug-timed "tried to apply to empty state" (vec (:state-path view-context)))))))))

(defn set-new [target-map override-map]
  (reduce (fn [target-map [key val]]
            (if (not= (get target-map key)
                      val)
              (assoc target-map key val)
              target-map))
          target-map
          override-map))

(defn call-view
  ([parent-view-context constructor child-id]
     (call-view parent-view-context constructor child-id {} []))

  ([parent-view-context constructor child-id state-overrides]
     (call-view parent-view-context constructor child-id state-overrides []))

  ([parent-view-context constructor child-id state-overrides constructor-parameters]
     (let [state-path-part [:child-states child-id]
           state-path (concat (:state-path parent-view-context) state-path-part)

           constructor ((-> parent-view-context :common-view-context :constructor-decorator)
                        constructor)

           view-context (assoc parent-view-context
                          :state-path state-path)

           state (-> (or (get-in @current-view-state-atom state-path-part)
                         (apply constructor view-context
                                constructor-parameters))
                     (set-new state-overrides))

           view-context (if (or (:sleep-time state)
                                (not (get-in @current-view-state-atom state-path-part)))
                          view-context
                          (dissoc view-context :frame-started))

           state (dissoc state :sleep-time)

           state (if (:decorated-view state)
                   state
                   (assoc state :decorated-view ((-> parent-view-context :common-view-context :view-decorator)
                                                 (:view state))))


           {:keys [state layoutable]} ((:decorated-view state) view-context
                                       state)]
       (swap! current-view-state-atom
              (fn [view-state]
                (-> view-state
                    (assoc-in state-path-part state)
                    (update-in [:children] conj child-id)
                    (assoc :sleep-time (choose-sleep-time (:sleep-time view-state)
                                                          (:sleep-time state))))))

       (assoc layoutable
         :state-path state-path))))

                                        ;set-wake-uppia ei kutsuta jos näkymän tulos tulee välimuistita
