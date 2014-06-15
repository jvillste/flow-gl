(ns examples.change-tracking
  (:require
   [flow-gl.tools.layoutable-inspector :as layoutable-inspector]
   (flow-gl.gui [drawable :as drawable]
                [layout :as layout]
                [layout-dsl :as l])
   (flow-gl.graphics [font :as font]))
  (:use clojure.test))

;;Debug

(defn text [content & {:keys [color size] :or {color [1 1 1 1] size 20}}]
  (drawable/->Text content
                   (font/create "LiberationSans-Regular.ttf" size)
                   color))

(defmulti debug-draw-single-layoutable :type)

(defmethod debug-draw-single-layoutable :default [this]
  (text (str (:type this))))

(defmethod debug-draw-single-layoutable :text [this]
  (text (str ":text " (:text this))))

(defn debug-draw-layoutable [layoutable]
  (l/vertically (l/horizontally (if (:state-path-part layoutable)
                                  (text (str (:state-path-part layoutable) " ") :color [0.8 0.8 0.8 1.0])
                                  (drawable/->Empty 0 0))
                                (debug-draw-single-layoutable layoutable))
                (l/margin 0 0 0 10
                          (layout/->VerticalStack (map debug-draw-layoutable (:children layoutable))))))



;; Utilities


(defmacro defn-memoized [name parameters & body]
  `(def ~name (memoize (fn ~parameters ~@body))))


;; Multimethods

(defmulti preferred-width :type)

(defmulti layout (fn [this requested-width y] (:type this)))


;; Views

(defn-memoized item-view [item]
  {:type :text
   :text item})

(defn-memoized view [items]
  {:type :vertical-stack
   :children (map-indexed (fn [index item]
                            {:type :box
                             :children [(assoc (item-view item)
                                          :state-path-part [:item index])
                                        {:type :rectangle}]})
                          items)})


;;Layoutables

(defmethod preferred-width :box [this]
  (+ 5 (preferred-width (first (:children this)))))

(defmethod layout :box [this requested-width y]
  (assoc this
    :width requested-width
    :children (let [[inner-child outer-child] (:children this)]
                [(layout inner-child
                         (- requested-width 5)
                         5)
                 (layout outer-child
                         requested-width
                         0)])))


(defmethod preferred-width :text [this]
  (count (:text this)))

(defmethod layout :text [this requested-width y]
  (assoc this
    :width requested-width))


(defmethod preferred-width :vertical-stack [this]
  (apply max (map preferred-width (:children this))))

(defmethod layout :vertical-stack [this requested-width y]
  (let [width (preferred-width this)]
    (assoc this
      :width width
      :children (map-indexed (fn [index child]
                               (layout child width (* 10 index)))
                             (:children this)))))


(defmethod preferred-width :rectangle [this]
  (or (:width this)
      0))

(defmethod layout :rectangle [this requested-width y]
  (assoc this
    :width requested-width))




#_(defn parent? [node]
    (contains? node :children ))

#_(defn quad-changes [path old-layout new-layout]
    (let [changes {}
          old-child-layout (get-in path old-layout)
          new-child-layout (get-in path new-layout)]
      (if (not (= (dissoc old-child-layout :children)
                  (dissoc new-child-layout :children)))
        )))

{[] 0
 [:item 0] 1
 [:item 1] 2}

{:set-position [{:path [0 1]
                 :x 10
                 :y 11}
                {:path [0 2]
                 :x 1
                 :y 11}]

 :remove [[3]
          [3 0]
          [3 1]]

 :add [{:path [4]
        :x 0 :y 0
        :parent []}
       {:path [4 0]
        :x 0 :y 0
        :parent [4]
        :image "Foo"}
       {:path [4 1]
        :x 0 :y 10
        :parent [4]
        :image "Bar"}]

 :set-image [{:path [3 0]
              :image "Baz"}]}

(let [items ["FooBar" "Bar"]
      layoutable (view items)
      first-layout (layout layoutable 100 0)
      new-items (update-in items [1] #(apply str (conj (vec %) \a)))
      new-layoutable (view new-items)
      new-layout (layout new-layoutable 100 0)
      #_quad-changes #_(quad-changes [] first-layout new-layout)]
  (println)
  (clojure.pprint/pprint items)
  (clojure.pprint/pprint first-layout)
  (layoutable-inspector/show-layoutable (l/margin 10 10 10 10
                                                  (l/horizontally (debug-draw-layoutable first-layout)
                                                                  (debug-draw-layoutable new-layout))))
  (clojure.pprint/pprint new-items)
  (clojure.pprint/pprint new-layout))
