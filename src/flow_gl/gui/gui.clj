(ns flow-gl.gui.gui
  (:require [clojure.core.async :as async]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad :as quad]
                                 [stencil :as stencil]
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

(defn close-when-requested-beforehand [state]
  (if (= (-> state :event :type) :close-requested)
    (assoc state :close-requested true)
    state))

(defn wrap-with-close-window-on-exception [app]
  (fn [state events]
    (try
      (app state events)

      (catch Exception e
        (window/close (:window state))
        (throw e)))))

;; CSP

(defn close-control-channel [view-state]
  (when-let [control-channel (:control-channel view-state)]
    (async/close! control-channel)))

(defn call-destructors [view-state]
  (when-let [destructor (:destructor view-state)]
    (destructor (:local-state view-state)))

  (close-control-channel view-state)
  (doseq [child-view-state (vals (:child-states view-state))]
    (call-destructors child-view-state)))

(defn call-destructors-when-close-requested [state]
  (when (= (-> state :event :type) :close-requested)
    (call-destructors (:view-state state)))
  state)

(defn add-event-channel [state]
  (assoc-in state [:common-view-context :event-channel] (window/event-channel (-> state :window))))

(debug/defn-timed apply-view-state-applications-beforehand [state]
  (if (= (-> state :event :type)
         :apply-to-view-state)
    ((-> state :event :function) state)
    state))


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
                               :nanovg (renderer/create-nanovg-renderer)
                               :triangle-list (renderer/create-triangle-list-renderer (:gl gpu-state))}))

(defn default-to-zero [value]
  (or value 0))

(defn add-vectors [vector-1 vector-2]
  (assoc vector-1
    :x (+ (default-to-zero (:x vector-1))
          (default-to-zero (:x vector-2)))
    :y (+ (default-to-zero (:y vector-1))
          (default-to-zero (:y vector-2)))
    :z (+ (default-to-zero (:z vector-1))
          (default-to-zero (:z vector-2)))))

(defn dirty-partitions
  ([layout-1 layout-2]
     (dirty-partitions layout-1 layout-2 [] []))

  ([layout-1 layout-2 partitions-to-be-redrawn partitions-to-be-cleared]
     (if (= layout-1
            layout-2)
       [partitions-to-be-redrawn
        partitions-to-be-cleared]
       (if (and (:children layout-1)
                (= (type layout-1)
                   (type layout-2)))
         (let [transposed-children-1 (map (fn [child]
                                            (add-vectors child layout-1))
                                          (:children layout-1))

               transposed-children-2 (map (fn [child]
                                            (add-vectors child layout-2))
                                          (:children layout-2))

               dirty-layers (loop [children-1 transposed-children-1
                                   children-2 transposed-children-2
                                   dirty-layers #{}]
                              (if-let [child-1 (first children-1)]
                                (let [child-2 (first children-2)]
                                  (if (= child-1 child-2)
                                    (recur (rest children-1)
                                           (rest children-2)
                                           dirty-layers)
                                    (recur (rest children-1)
                                           (rest children-2)
                                           (conj dirty-layers (:z child-1)))))
                                dirty-layers))]
           (loop [partitions-to-be-redrawn partitions-to-be-redrawn
                  partitions-to-be-cleared partitions-to-be-cleared
                  children-1 transposed-children-1
                  children-2 transposed-children-2]
             (if-let [child-1 (first children-1)]
               (let [child-2 (first children-2)]
                 (if (not (empty? (disj dirty-layers (:z child-1))))
                   (recur (conj partitions-to-be-redrawn (assoc child-1 :stenciled (not (= child-1 child-2))))
                          (if (and child-2
                                   (not (= child-1 child-2)))
                            (conj partitions-to-be-cleared child-2)
                            partitions-to-be-cleared)
                          (rest children-1)
                          (rest children-2))
                   (let [[partitions-to-be-redrawn partitions-to-be-cleared] (dirty-partitions child-1
                                                                                               child-2
                                                                                               partitions-to-be-redrawn
                                                                                               partitions-to-be-cleared)]
                     (recur partitions-to-be-redrawn
                            partitions-to-be-cleared
                            (rest children-1)
                            (rest children-2)))))

               [partitions-to-be-redrawn
                (into partitions-to-be-cleared children-2)])))

         [(conj partitions-to-be-redrawn
                (-> layout-1
                    (assoc :stenciled true)))
          (if layout-2
            (conj partitions-to-be-cleared
                  layout-2)
            partitions-to-be-cleared)]))))


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
  (let [[gpu-state drawables] (transformer/transform-trees gpu-state
                                                           (:render-trees gpu-state))]
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
  (let [[quad-view quad nanovg triangle-list] (renderer/render-frame-drawables drawables
                                                                               gl
                                                                               [(:quad-view renderers)
                                                                                (:quad renderers)
                                                                                (:nanovg renderers)
                                                                                (:triangle-list renderers)])]
    {:quad-view quad-view
     :quad quad
     :nanovg nanovg
     :triangle-list triangle-list}))


(defn add-clearing-drawable [gpu-state]
  (let [{:keys [width height]} (opengl/size (:gl gpu-state))]
    (assoc gpu-state
      :drawables (concat [(assoc (drawable/->Rectangle width height [0 0 0 255])
                            :x 0 :y 0 :z -1 :width width :height height)]
                         (:drawables gpu-state)))))

(defn add-stencil [gpu-state]
  (assoc gpu-state :stencil (stencil/create (:gl gpu-state))))

(defn set-stencil [gpu-state]
  (assoc gpu-state :stencil
         (stencil/set (:stencil gpu-state)
                      (concat (filter :stenciled (:partitions gpu-state))
                              (:partitions-to-be-cleared gpu-state))
                      (:gl gpu-state))))

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

                                           (let [gpu-state (-> (set-stencil gpu-state)
                                                               (add-clearing-drawable))
                                                 gpu-state (update-in gpu-state [:renderers] render-drawables-with-renderers gl (:drawables gpu-state))]
                                             (stencil/disable (:gl gpu-state))
                                             gpu-state))]

    (render-target/blit render-target gl)

    (assoc gpu-state
      :render-target render-target)))


(defn layout-to-partitions [gpu-state]
  (let [size (opengl/size (:gl gpu-state))
        [partitions-to-be-redrawn partitions-to-be-cleared] (if (= size (:previous-size gpu-state))
                                                              (dirty-partitions (:layout gpu-state)
                                                                                (:previous-layout gpu-state))
                                                              [[(:layout gpu-state)]
                                                               [{:x 0
                                                                 :y 0
                                                                 :width (:width size)
                                                                 :height (:height size)}]])]

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
  (assoc gpu-state
    :drawables (mapcat (fn [partition]
                         (drawables-for-layout partition)

                         #_(if (:recurring? partition)
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
      (window/close (:window gpu-state))
      (throw e))))

(debug/defn-timed render-drawables-afterwards [state]
  (async/put! (:with-gl-channel state)
              {:function render
               :arguments [(:layout state)]})

  state)

;; Animation

(defn choose-sleep-time [sleep-time-1 sleep-time-2]
  (if sleep-time-1
    (if sleep-time-2
      (min sleep-time-1 sleep-time-2)
      sleep-time-1)
    sleep-time-2))

(defn set-wake-up [view-context sleep-time]
  (swap! @(:current-view-state-atom view-context)
         (fn [view-state]
           (update-in view-state [:sleep-time] choose-sleep-time sleep-time))))

(defn add-frame-started [state]
  (assoc state :frame-started (System/currentTimeMillis)))

(defn save-sleep-time [state]
  (-> state
      (assoc :previous-sleep-time (:sleep-time state))
      (dissoc :sleep-time)))

(defn limit-frames-per-second-afterwards [state target-frames-per-second]
  (if (:sleep-time state)
    (let [minimum-sleep-time (/ 1000 target-frames-per-second)
          previous-frame-duration (- (System/currentTimeMillis)
                                     (or (:last-frame state)
                                         (System/currentTimeMillis)))
          sleep-time (max (:sleep-time state)
                          (- minimum-sleep-time
                             (max 0
                                  (- previous-frame-duration
                                     (or (:previous-sleep-time state)
                                         0)))))]
      (assoc state
        :sleep-time sleep-time
        :last-frame (System/currentTimeMillis)))
    state))


;; Layout

(debug/defn-timed add-layout-afterwards [state]
  (let [width (window/width (:window state))
        height (window/height (:window state))
        [state layout] (if (satisfies? layout/Layout (:layoutable state))
                         (layout/do-layout (:layoutable state)
                                           state
                                           width
                                           height
                                           (:cache state))
                         [state (:layoutable state)]
                         )
        layout (-> layout
                   (assoc :x 0
                          :y 0
                          :width width
                          :height height)
                   (layout/add-out-of-layout-hints))]
    (assoc state :layout layout)))

;; Mouse

(debug/defn-timed add-layout-paths-under-mouse-beforehand [state]
  (if (and (:layout state)
           (= (-> state :event :source)
              :mouse))
    (assoc state :layout-paths-under-mouse (reverse (layout/layout-paths-in-coordinates (:layout state) (-> state :event :x) (-> state :event :y))))
    state))

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
    (let [handlers-and-arguments (apply concat (mapcat #(layout-path-to-handlers % layout handler-key)
                                                       layout-paths))]
      (reduce (fn [state [handler handler-arguments]]
                (apply handler
                       state
                       (concat arguments handler-arguments)))
              state
              handlers-and-arguments))
    state))

(debug/defn-timed apply-layout-event-handlers-beforehand [state]
  (if (and (:layout state)
           (= (-> state :event :source)
              :mouse))
    (apply-layout-event-handlers state (:layout state) (:layout-paths-under-mouse state) :handle-mouse-event (:event state))
    state))


(defn set-hierarchical-state [state paths new-value child-state-key state-key state-gained-key state-lost-key]
  (if (seq paths)
    (loop [paths paths
           state state]
      (if (seq (rest paths))
        (recur (rest paths)
               (update-in state (concat (first paths) [:local-state]) assoc child-state-key new-value))
        (update-in state (concat (first paths) [:local-state]) (fn [state]
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

(debug/defn-timed apply-mouse-movement-event-handlers-beforehand [state]
  (if (and (:layout state)
           (= (-> state :event :source)
              :mouse)
           (= (-> state :event :type)
              :mouse-moved))
    (let [state-paths-under-mouse (layout-path-to-state-paths (:layout state) (first (:layout-paths-under-mouse state)))]
      (-> state
          #_(set-mouse-over state-paths-under-mouse)
          (apply-mouse-over-layout-event-handlers (:layout state) (:layout-paths-under-mouse state) #_[(first (:layout-paths-under-mouse state))])))
    state))

;; mouse api

(defn local-state-path [view-context]
  (concat (:state-path view-context)
          [:local-state]))

(defn apply-to-local-state [state view-context function & arguments]
  (apply update-in
         state
         (local-state-path view-context)
         function
         arguments))

(defn get-local-state [application-state view-context]
  (get-in application-state
          (local-state-path view-context)))

(defn add-mouse-event-handler [layoutable handler & arguments]
  (assoc layoutable
    :handle-mouse-event (conj (or (:handle-mouse-event layoutable)
                                  [])
                              [handler arguments])))

(defn handle-mouse-event-with-context [state event view-context handler arguments]
  (apply update-or-apply-in state (concat (:state-path view-context) [:local-state]) handler event arguments))

(defn handle-mouse-event [state event view-context handler arguments]
  (apply update-or-apply-in state (concat (:state-path view-context) [:local-state]) handler event arguments))

(defn add-mouse-event-handler-with-context [layoutable view-context handler arguments]
  (add-mouse-event-handler layoutable handle-mouse-event-with-context view-context handler arguments))

(defn handle-mouse-event-of-type [state event event-type handler arguments]
  (if (= event-type (:type event))
    (apply handler state event arguments)
    state))

(defn on-mouse-event-with-view-context [layoutable event-type view-context handler & arguments]
  (add-mouse-event-handler-with-context layoutable view-context handle-mouse-event-of-type [event-type handler arguments]))

(defn on-mouse-clicked-with-view-context [layoutable view-context handler & arguments]
  (apply on-mouse-event-with-view-context layoutable :mouse-clicked view-context handler arguments))


(defn on-mouse-event [layoutable event-type handler & arguments]
  (add-mouse-event-handler layoutable handle-mouse-event-of-type event-type handler arguments))

(defn on-mouse-clicked [layoutable handler & arguments]
  (apply on-mouse-event layoutable :mouse-clicked handler arguments))

;; Keyboard

(debug/defn-timed apply-keyboard-event-handlers-beforehand [state]
  (if (= (-> state :event :source)
         :keyboard)
    (loop [state state
           focused-state-paths (concat [[:view-state]] (:focused-state-paths state))]
      (if-let [focused-state-path (first focused-state-paths)]
        (if-let [keyboard-event-handler (get-in state (concat (vec focused-state-path)
                                                              [:handle-keyboard-event]))]
          (let [state (keyboard-event-handler state (:event state))]
            (if (:stop-keyboard-event-handling state)
              (dissoc state :stop-keyboard-event-handling)
              (recur state
                     (rest focused-state-paths))))
          (recur state
                 (rest focused-state-paths)))
        state))
    state))

(defn set-focus [state focus-paths]
  (move-hierarchical-state state focus-paths :focused-state-paths :child-has-focus :has-focus :on-focus-gained :on-focus-lost))

(debug/defn-timed set-focus-on-mouse-click-beforehand [state]
  (if (and (= (-> state :event :source)
              :mouse)
           (= (-> state :event :type)
              :mouse-clicked))

    (let [state-paths-under-mouse (layout-path-to-state-paths (:layout state)
                                                              (first (:layout-paths-under-mouse state)))]
      (if (get-in state (concat (last state-paths-under-mouse)
                                [:can-gain-focus]))
        (set-focus state state-paths-under-mouse)
        state))
    state))

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

(defn add-cache [state]
  (let [cache (cache/create)]
    (-> state
        (assoc :cache cache)
        (assoc-in [:common-view-context :cache] cache))))

(defn clear-cache [state]
  (let [event-batch (or (:event-batch state)
                        0)]
    (when (> event-batch
             100)
      (swap! (:cache state) cache/remove-unused)
      (swap! (:cache state) cache/clear-usages))

    (assoc state
      :event-batch (if (> event-batch
                          100)
                     0
                     (inc event-batch)))))

(defn clear-cache-when-requested [state]
  (if (= (-> state :event :type)
         :request-redraw)
    (assoc state :cache (cache/create))
    state))

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

(defn remove-unused-children [view-state view-context]
  (let [child-set (set (:children view-state))
        children-to-be-removed (->> (:old-children view-state)
                                    (filter (complement child-set)))]
    (-> (reduce (fn [view-state child-to-be-removed]
                  (do (call-destructors (get-in view-state [:child-states child-to-be-removed]))
                      (update-in view-state [:child-states] dissoc child-to-be-removed)))
                view-state
                children-to-be-removed)
        (reset-children))))


(defn wrap-with-current-view-state-atom [view]
  (-> (fn [view-context state]
        (let [view-context (assoc view-context
                             :current-view-state-atom (atom state))
              layoutable (view view-context state)]
          {:layoutable layoutable
           :state (deref (:current-view-state-atom view-context))}))
      (with-meta (meta view))))


(defn initialize-gpu-state [window]
  (window/with-gl window gl
    (-> {:window window
         :gl gl
         :previous-partitions #{}}
        (add-renderers)
        (add-stencil))))

(def state-atom (atom nil))

(defn event-loop [initial-state app]
  (let [initial-state (assoc initial-state
                        :with-gl-channel (async/chan 50)
                        :with-gl-dropping-channel (async/chan (async/dropping-buffer 1)))]

    (.start (Thread. (fn [] (loop [state initial-state]
                              (reset! state-atom state)

                              (if (:close-requested state)
                                (do (async/close! (:with-gl-channel initial-state))
                                    (async/close! (:with-gl-dropping-channel initial-state))
                                    (window/close (:window state)))

                                (let [events (csp/drain (window/event-channel (:window state))
                                                        (:sleep-time state))

                                      events (if (empty? events)
                                               [{:type :wake-up}]
                                               events)]

                                  (recur (debug/debug-timed-and-return :app (app state
                                                                                 events)))))))))

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
                   :priority true))))

;; View calls

(defrecord ViewCall [parent-view-context constructor child-id state-overrides constructor-parameters constructor-overrides])

(defn view-call-paths
  ([layoutable]
     (view-call-paths layoutable [] []))
  ([layoutable current-path paths]
     (if (instance? ViewCall layoutable)
       (conj paths current-path)
       (if-let [children (vec (:children layoutable))]
         (loop [child-index 0
                paths paths]
           (if-let [child (get children child-index)]
             (recur (inc child-index)
                    (view-call-paths child
                                     (concat current-path
                                             [:children child-index])
                                     paths))
             paths))
         paths))))

(deftest view-call-paths-test
  (is (= '[(:children 1)
           (:children 2)
           (:children 3 :children 0)
           (:children 3 :children 1)]
         (view-call-paths {:children [{}
                                      (->ViewCall nil nil nil nil nil)
                                      (->ViewCall nil nil nil nil nil)
                                      {:children [(->ViewCall nil nil nil nil nil)
                                                  (->ViewCall nil nil nil nil nil)]}]}))))

(defn children-to-vectors [layoutable]
  (if-let [children (:children layoutable)]
    (loop [children children
           processed-children []]
      (if-let [child (first children)]
        (recur (rest children)
               (conj processed-children (children-to-vectors child)))
        (assoc layoutable :children processed-children)))
    layoutable))


(deftest children-to-vectors-test
  (is (= {:children [{:children [{} {}]}]}
         (children-to-vectors {:children (list {:children (list {} {})})}))))

(defn handle-events [state events app]
  (try
    (let [state (-> state
                    (add-frame-started)
                    (save-sleep-time))

          state (reduce (fn [state event]
                          (flow-gl.debug/add-event :handle-event)
                          (-> state
                              (assoc :event event)
                              (clear-cache-when-requested)
                              (close-when-requested-beforehand)
                              (add-layout-paths-under-mouse-beforehand)
                              (set-focus-on-mouse-click-beforehand)
                              (apply-keyboard-event-handlers-beforehand)
                              (apply-layout-event-handlers-beforehand)
                              (apply-mouse-movement-event-handlers-beforehand)
                              (apply-view-state-applications-beforehand)
                              (app)
                              (add-layout-afterwards)))
                        state
                        events)]

      (-> state
          (call-destructors-when-close-requested)
          (limit-frames-per-second-afterwards 60)
          (render-drawables-afterwards)
          (clear-cache)))

    (catch Exception e
      (window/close (:window state))
      (throw e))))

(defn start-app [app]
  (-> {}
      (add-window)
      (add-cache)
      (set-event-channel-atom)
      (add-event-channel)
      (event-loop (fn [state events]
                    (handle-events state events app)))))


;; View calls

(defn bind [view-call view-context state parent-state-key child-state-key]
  (-> view-call
      (update-in [:state-overrides] assoc child-state-key (parent-state-key state))
      (update-in [:constructor-overrides] assoc [:on-change child-state-key] (fn [state new-value]
                                                                               (apply-to-local-state state
                                                                                                     view-context
                                                                                                     assoc parent-state-key new-value)))))

(defn update-binding [state view-context function key]
  (let [local-state (get-local-state state view-context)]
    ((get local-state [:on-change key])
     state
     (function (get local-state key)))))

(defn create-update-binding [view-context function key]
  (fn [state event]
    (update-binding state view-context function key)))

(defn call-view
  ([parent-view-context constructor child-id]
     (call-view parent-view-context constructor child-id {} [] {}))

  ([parent-view-context constructor child-id state-overrides]
     (call-view parent-view-context constructor child-id state-overrides [] {}))

  ([parent-view-context constructor child-id state-overrides constructor-parameters]
     (call-view parent-view-context constructor child-id state-overrides constructor-parameters {}))

  ([parent-view-context constructor child-id state-overrides constructor-parameters constructor-overrides]
     (->ViewCall parent-view-context constructor child-id state-overrides constructor-parameters constructor-overrides)))

(defn call-and-bind [view-context state from-key to-key & call-view-arguments]
  (-> (apply call-view view-context call-view-arguments)
      (bind view-context
            state
            from-key
            to-key)))

(defn set-new [target-map override-map]
  (reduce (fn [target-map [key val]]
            (if (not= (get target-map key)
                      val)
              (assoc target-map key val)
              target-map))
          target-map
          override-map))

(defn run-view [view view-context state]
  (let [layoutable (view view-context state)]
    (-> layoutable
        (assoc :view-call-paths (view-call-paths layoutable))
        (children-to-vectors))))

(defn ensure-child-state [parent-view-state state-path-part view-context constructor constructor-parameters constructor-overrides]
  (or (get-in parent-view-state state-path-part)
      (let [control-channel (async/chan)
            child-state (apply constructor (assoc view-context :control-channel control-channel)
                               constructor-parameters)
            child-state (if (:local-state child-state)
                          child-state
                          (assoc child-state :local-state {}))
            child-state (update-in child-state [:local-state] set-new constructor-overrides)]
        (assoc child-state :control-channel control-channel))))

(defn set-frame-started [view-context frame-started child-view-state parent-view-state state-path-part]
  (if (or (:sleep-time child-view-state)
          (not (get-in parent-view-state state-path-part)))
    (assoc view-context :frame-started frame-started)
    (dissoc view-context :frame-started)))

(def resolve-view-calls)

(defn run-view-call [cache state-path-part parent-view-context application-state constructor constructor-parameters state-overrides constructor-overrides]
  (let [parent-view-state (get-in application-state (:state-path parent-view-context))
        state-path (concat (:state-path parent-view-context) state-path-part)

        view-context (assoc parent-view-context
                       :state-path state-path)

        view-state (ensure-child-state parent-view-state
                                       state-path-part
                                       view-context
                                       constructor
                                       constructor-parameters
                                       constructor-overrides)

        view-state (update-in view-state [:local-state] set-new state-overrides)

        view-context (set-frame-started view-context
                                        (:frame-started parent-view-context)
                                        view-state
                                        parent-view-state
                                        state-path-part)


        view-state (dissoc view-state :sleep-time)

        layoutable (cache/call-with-cache-atom cache
                                               run-view
                                               (:view view-state)
                                               view-context
                                               (:local-state view-state))

        view-state (assoc view-state
                     :sleep-time (:sleep-time layoutable))

        application-state (assoc-in application-state state-path view-state)

        [application-state layoutable] (cache/call-with-cache-atom cache
                                                                   #'resolve-view-calls
                                                                   cache
                                                                   application-state
                                                                   layoutable)

        application-state (update-in application-state state-path remove-unused-children view-context)

        layoutable (assoc layoutable
                     :state-path state-path)]

    [application-state
     layoutable]))



(defn resolve-view-call [cache application-state view-call]
  (let [state-path-part [:child-states (:child-id view-call)]
        [application-state layoutable] (run-view-call cache
                                                      state-path-part
                                                      (:parent-view-context view-call)
                                                      application-state
                                                      (:constructor view-call)
                                                      (:constructor-parameters view-call)
                                                      (:state-overrides view-call)
                                                      (:constructor-overrides view-call))

        layoutable (conj layoutable
                         (dissoc view-call
                                 :parent-view-context
                                 :constructor
                                 :child-id
                                 :state-overrides
                                 :constructor-parameters
                                 :constructor-overrides))]

    [(update-in application-state
                (-> view-call :parent-view-context :state-path)
                (fn [parent-view-state]
                  (-> parent-view-state
                      (update-in [:children] conj (:child-id view-call))
                      (assoc :sleep-time (choose-sleep-time (:sleep-time parent-view-state)
                                                            (get-in parent-view-state (conj state-path-part :sleep-time)))))))

     layoutable]))

(defn resolve-view-calls [cache application-state layoutable]
  (loop [application-state application-state
         view-call-paths (:view-call-paths layoutable)
         layoutable layoutable]
    (if-let [view-call-path (first view-call-paths)]
      (let [[application-state child] (resolve-view-call cache application-state (get-in layoutable view-call-path))
            #_(cache/call-with-cache-atom cache #'resolve-view-call cache application-state (get-in layoutable view-call-path))]
        (recur application-state
               (rest view-call-paths)
               (assoc-in layoutable view-call-path child)))
      [application-state layoutable])))

(defn control-to-application [constructor]
  (fn [application-state]
    (let [[application-state layoutable] (run-view-call (:cache application-state)
                                                        [:view-state]
                                                        {:frame-started (:frame-started application-state)
                                                         :state-path []
                                                         :common-view-context (:common-view-context application-state)}
                                                        application-state
                                                        constructor
                                                        []
                                                        {}
                                                        {})

          #_application-state #_(set-focus application-state
                                           (initial-focus-paths view-state))]

      (-> application-state
          (assoc :layoutable layoutable
                 :sleep-time (get-in application-state [:view-state :sleep-time]))))))

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
                                                            (apply update-or-apply-in state (concat (:state-path view-context) [:local-state]) function arguments)
                                                            (throw (Exception. (str "tried to apply to empty state" (vec (:state-path view-context)))))))))))


(run-tests)
