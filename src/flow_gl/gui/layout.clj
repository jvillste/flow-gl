(ns flow-gl.gui.layout
  (:require  [clojure.spec :as spec]
             flow-gl.debug
             (flow-gl.gui [layoutable :as layoutable]
                          [cache :as cache]))
  (:use clojure.test))

(defprotocol Layout
  (layout [layout state requested-width requested-height])
  (children [layout]))

(extend Object
  Layout {:layout (fn [this state requested-width requsested-height]
                    [state this])
          :children (fn [this] (:children this))})

(defprotocol PruningLayout
  (pruned-layout [layout state requested-width requested-height min-x min-y max-x max-y]))

(defprotocol SpatialIndex
  (children-in-coordinates [layout x y]))

(def ^:dynamic cache)

(defn do-layout [layoutable state requested-width requested-height cache-to-be-used]
  (binding [cache cache-to-be-used]
    (layout layoutable state requested-width requested-height)))

(defn do-layout-without-state [layoutable]
  (second (do-layout layoutable
                     {}
                     java.lang.Integer/MAX_VALUE
                     java.lang.Integer/MAX_VALUE
                     (cache/create))))
#_(extend Object
    Layout {:layout (fn [this state requested-width requsested-height]
                      [state this])})

;; UTILITIES

(defn find-layoutable-paths
  ([layoutable predicate]
   (find-layoutable-paths layoutable predicate [] []))
  ([layoutable predicate current-path paths]
   (let [paths (if (predicate layoutable)
                 (conj paths current-path)
                 paths)]
     (if-let [children (vec (:children layoutable))]
       (loop [child-index 0
              paths paths]
         (if-let [child (get children child-index)]
           (recur (inc child-index)
                  (find-layoutable-paths child
                                         predicate
                                         (concat current-path
                                                 [:children child-index])
                                         paths))
           paths))
       paths))))


(defn add-global-coordinates [layout global-x global-y]
  (let [layout (assoc layout
                      :global-x global-x
                      :global-y global-y)]
    (if (:children layout)
      (update-in layout
                 [:children]
                 (fn [children]
                   (vec (for [child children]
                          (add-global-coordinates child
                                                  (+ (:x child)
                                                     global-x)

                                                  (+ (:y child)
                                                     global-y))))))
      layout)))

(def ^:dynamic current-state-atom)
#_(def ^:dynamic current-state-path [])

(defn layout-with-current-state [layout-instance width height]
  (let [state-path-part (or (:state-path-part layout-instance) [])
        local-state (get-in @current-state-atom state-path-part)
        [local-state layout] (layout layout-instance local-state width height)]

    (if (= state-path-part [])
      (reset! current-state-atom local-state)
      (swap! current-state-atom assoc-in state-path-part local-state))
    layout))

(defn set-dimensions-and-layout
  ([layout-instance x y width height]
   (-> layout-instance
       (cond-> (satisfies? Layout layout-instance)
         (layout-with-current-state width
                                    height))
       (assoc :x x
              :y y
              :width width
              :height height)))

  ([layout-instance state x y width height]
   (binding [current-state-atom (atom state)]
     (let [child-layout (set-dimensions-and-layout layout-instance x y width height)]
       [@current-state-atom
        child-layout]))))


(defn in-coordinates [x y layoutable]
  (and (>= x
           (:x layoutable))
       (<= x
           (+ (:x layoutable) (:width layoutable)))
       (>= y
           (:y layoutable))
       (<= y
           (+ (:y layoutable) (:height layoutable)))))

(defn filter-by-coordinates [x y layoutables]
  (filter (partial in-coordinates x y) layoutables))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
  is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

#_(defn children-in-coordinates [layout x y]
    (for [child-index (positions (partial in-coordinates x y) (:children layout))]
      (let [child (get-in layout [:children child-index])]
        (cons child-index (children-in-coordinates child
                                                   (-  x (:x child))
                                                   (-  y (:y child)))))))



#_(fact (children-in-coordinates {:x 0 :y 0 :width 100 :height 100
                                  :children [{:x 0 :y 0 :width 20 :height 30}
                                             {:x 10 :y 10 :width 10 :height 30
                                              :children [{:x 0 :y 0 :width 10 :height 10}
                                                         {:x 0 :y 10 :width 10 :height 10}]}]}
                                 15
                                 25)
        => '((0) (1 (1))))

(defn layout-index-path-to-layout-path [layout-index-path]
  (conj (interpose  :children layout-index-path) :children ))

#_(fact (layout-index-path-to-layout-path [0 0])
        => '(:children 0 :children 0))

(defn layout-index-paths-in-coordinates [layout parent-path x y z paths]
  (let [child-indexes (if (satisfies? SpatialIndex layout)
                        (children-in-coordinates layout x y)
                        (positions (fn [child] (or (in-coordinates x y child)
                                                   (:has-children-out-of-layout child)))
                                   (:children layout)))]
    (if (empty? child-indexes)
      (if (in-coordinates (+  x (:x layout))
                          (+  y (:y layout))
                          layout)
        (conj paths {:path parent-path
                     :z z})
        paths)
      (loop [child-indexes child-indexes
             paths paths]
        (if-let [child-index (first child-indexes)]
          (let [child (get-in layout [:children child-index])]
            (recur (rest child-indexes)
                   (layout-index-paths-in-coordinates child
                                                      (vec (conj parent-path child-index))
                                                      (- x (:x child))
                                                      (- y (:y child))
                                                      (+ z (or (:z child) 0))
                                                      paths)))
          paths)))))

(deftest layout-index-paths-in-coordinates-test
  (is (= (layout-index-paths-in-coordinates {:x 0 :y 0 :width 20 :height 40
                                             :children [{:x 0 :y 0 :width 20 :height 40
                                                         :children [{:x 0 :y 0 :width 40 :height 40}
                                                                    {:x 0 :y 10 :width 40 :height 40}]}

                                                        {:x 10 :y 10 :width 10 :height 10
                                                         :has-children-out-of-layout true
                                                         :children [{:x 0 :y 0 :width 10 :height 10}
                                                                    {:x 0 :y 10 :z 1 :width 10 :height 10}]}

                                                        {:x 10 :y 20 :width 10 :height 10
                                                         :children [{:x 0 :y 0 :width 10 :height 10}
                                                                    {:x 0 :y 10 :z 1 :width 10 :height 10}]}]}
                                            []
                                            15
                                            25
                                            0
                                            [])
         [{:path [0 0], :z 0} {:path [0 1], :z 0} {:path [1 1], :z 1} {:path [2 0], :z 0}])))

(defn layout-paths-in-coordinates [layout x y]
  (map layout-index-path-to-layout-path
       (->> (layout-index-paths-in-coordinates layout [] x y 0 [])
            (sort-by :z)
            (map :path))))

(defn layout-index-paths-with-keyboard-event-handlers [layout parent-path]
  (if-let [child-indexes (seq (positions #(or (:handle-keyboard-event %) (:children %))
                                         (:children layout)))]
    (concat (if (:handle-keyboard-event layout)
              [parent-path]
              [])
            (mapcat (fn [child-index]
                      (let [child (get-in layout [:children child-index])]
                        (layout-index-paths-with-keyboard-event-handlers child
                                                                         (vec (conj parent-path child-index)))))
                    child-indexes))
    [parent-path]))

#_(fact (layout-index-paths-with-keyboard-event-handlers {:children [{:handle-keyboard-event :handler
                                                                      :children [{}
                                                                                 {:handle-keyboard-event :handler}]}

                                                                     {:children [{:handle-keyboard-event :handler}
                                                                                 {}]}]}
                                                         [])
        => '([0] [0 1] [1 0]))

(defn layout-paths-with-keyboard-event-handlers [layout]
  (map layout-index-path-to-layout-path
       (layout-index-paths-with-keyboard-event-handlers layout [])))

(defmacro deflayout [name parameters layout-implementation preferred-size-implementation]
  (let [[layout-name layout-parameters & layout-body] layout-implementation
        layout-parameters (vec (concat layout-parameters parameters))
        [preferred-size-name preferred-size-parameters & preferred-size-body] preferred-size-implementation
        preferred-size-parameters (vec (concat preferred-size-parameters parameters))
        layout-implementation-symbol (symbol (str name "-layout"))
        preferred-size-implementation-symbol (symbol (str name "-preferred-size"))]
    (assert (= layout-name 'layout) (str "invalid layout name" layout-name))
    (assert (= preferred-size-name 'preferred-size) (str "invalid preferred size name" preferred-size-name))

    `(do (defn ~layout-implementation-symbol ~layout-parameters ~@layout-body)
         (defn ~preferred-size-implementation-symbol ~preferred-size-parameters ~@preferred-size-body)
         (defrecord ~name ~parameters
           Layout
           (layout [layoutable# state# width# height#]
             (binding [current-state-atom (atom state#)]
               (let [layout# (cache/call-with-cache-atom cache #_(:cache state#) ~layout-implementation-symbol
                                                         layoutable# width# height# ~@parameters)]
                 [@current-state-atom
                  layout#])))

           layoutable/Layoutable
           (layoutable/preferred-size [this# available-width# available-height#]
             (cache/call-with-cache-atom cache ~preferred-size-implementation-symbol this# available-width# available-height# ~@parameters))

           Object
           (toString [this#] (layoutable/describe-layoutable this#))))))

(defmacro deflayout-with-state [name parameters layout-implementation preferred-size-implementation]
  (let [[layout-name layout-parameters & layout-body] layout-implementation
        [preferred-size-name preferred-size-parameters & preferred-size-body] preferred-size-implementation]
    (assert (= layout-name 'layout) (str "invalid layout name" layout-name))
    (assert (= preferred-size-name 'preferred-size) (str "invalid preferred size name" preferred-size-name))

    `(defrecord ~name ~parameters
       Layout
       ~layout-implementation

       layoutable/Layoutable
       (layoutable/preferred-size ~preferred-size-parameters ~@preferred-size-body)

       Object
       (toString [this#] (layoutable/describe-layoutable this#)))))

(defn add-out-of-layout-hints [layout]
  (loop [children (:children layout)
         hinted-children []
         has-children-out-of-layout false]
    (if-let [child (first children)]
      (let [hinted-child (add-out-of-layout-hints child)]
        (recur (rest children)
               (conj hinted-children hinted-child)
               (or has-children-out-of-layout
                   (:has-children-out-of-layout hinted-child)
                   (:out-of-layout hinted-child))))
      (cond-> layout
        has-children-out-of-layout (assoc :has-children-out-of-layout true)
        (seq hinted-children) (assoc :children hinted-children)))))

(deftest add-out-of-layout-hints-test
  (is (= (add-out-of-layout-hints {:x 0 :y 0 :width 20 :height 40
                                   :children [{:x 0 :y 0 :width 20 :height 40
                                               :children [{:x 0 :y 0 :width 40 :height 40}
                                                          {:x 0 :y 10 :width 40 :height 40}]}

                                              {:x 10 :y 10 :width 10 :height 10
                                               :children [{:x 0 :y 0 :width 10 :height 10}
                                                          {:x 0 :y 10 :z 1 :width 10 :height 10 :out-of-layout true}]}]})
         {:x 0 :y 0 :width 20 :height 40 :has-children-out-of-layout true
          :children [{:x 0 :y 0 :width 20 :height 40
                      :children [{:x 0 :y 0 :width 40 :height 40}
                                 {:x 0 :y 10 :width 40 :height 40}]}

                     {:x 10 :y 10 :width 10 :height 10 :has-children-out-of-layout true
                      :children [{:x 0 :y 0  :width 10 :height 10}
                                 {:x 0 :y 10 :z 1 :width 10 :height 10 :out-of-layout true}]}]})))




(spec/def ::available-width int?)
(spec/def ::available-height int?)
(spec/def ::node-with-space (spec/keys :req-un [::available-width ::available-height]))

(defn adapt-to-space [node]
  (if-let [adapt-function (:adapt-to-space node)]
    (adapt-function node)
    node))

(defn ensure-available-space [node]
  (assoc node
         :available-width (or (:available-width node)
                              java.lang.Integer/MAX_VALUE)
         :available-height (or (:available-height node)
                               java.lang.Integer/MAX_VALUE)))

(defn add-space [node]
  (if (:children node)
    (let [node (ensure-available-space node)]
      (if-let [space-function (:give-space node)]
        (space-function node)
        (update-in node [:children]
                   (fn [children]
                     (map (fn [child]
                            (assoc child
                                   :available-width (:available-width node)
                                   :available-height (:available-height node)))
                          children)))))
    
    node))

(defn size [node]
  (if-let [get-size (:get-size node)]
    (get-size node)
    {:width (or (:width node)
                (:available-width node))
     :height (or (:height node)
                 (:available-height node))}))

(defn add-size [node]
  (conj node (size node)))

(defn add-layout [node]
  (if-let [layout-function (:make-layout node)]
    (layout-function node)
    (update-in node [:children]
               (fn [children]
                 (if children
                   (map (fn [child]
                          (assoc child
                                 :x (or (:x child)
                                        0)
                                 :y (or (:y child)
                                        0)))
                        children)
                   nil)))))

(defn do-layout [node]
  (-> node
      (adapt-to-space)
      (add-space)
      (update-in [:children]
                 (fn [children]
                   (if children
                     (map do-layout
                          children)
                     nil)))
      (add-size)
      (add-layout)))
