(ns fungl.layouts
  (:require  [clojure.spec :as spec]
             (fungl [cache :as cache]
                    [layout :as layout])))

(defn flatten-contents [values]
  (->> values
       (filter (complement nil?))
       (flatten)
       (filter (complement nil?))))

(def vertical-stack
  {:get-size (fn [node]
               {:width (apply max
                              (conj (map :width (:children node))
                                    0))
                :height (reduce + (map :height (:children node)))})
   
   :give-space (fn [node]
                 (update-in node [:children]
                            (fn [children]

                              (map (fn [child]
                                     (assoc child
                                            :available-width (:available-width node)
                                            :available-height java.lang.Integer/MAX_VALUE))
                                   children))))
   :make-layout (fn [node]
                  (assoc node :children
                         (loop [layouted-nodes []
                                y 0
                                children (:children node)]
                           (if-let [child (first children)] 
                             (recur (conj layouted-nodes
                                          (assoc child
                                                 :x 0
                                                 :y y))
                                    (+ y (:height child))
                                    (rest children))
                             layouted-nodes))))})

(defn vertically [& children]
  (assoc vertical-stack
         :children (flatten-contents children)))


(def horizontal-stack
  {:get-size (fn [node]
               {:width (reduce + (map :width (:children node)))
                :height (apply max
                               (conj (map :height (:children node))
                                     0))})
   
   :give-space (fn [node]
                 (update-in node [:children]
                            (fn [children]
                              (map (fn [child]
                                     (assoc child
                                            :available-width java.lang.Integer/MAX_VALUE
                                            :available-height (:available-width node)))
                                   children))))
   :make-layout (fn [node]
                  (assoc node :children
                         (loop [layouted-nodes []
                                x 0
                                children (:children node)]
                           (if-let [child (first children)] 
                             (recur (conj layouted-nodes
                                          (assoc child
                                                 :x x
                                                 :y 0))
                                    (+ x (:width child))
                                    (rest children))
                             layouted-nodes))))})

(defn horizontally [& children]
  (assoc horizontal-stack
         :children (flatten-contents children)))

;; box

(spec/def ::margin int?)

(defn box-get-size [{:keys [margin children]}]
  (let [[outer inner] children]
    {:width (+ (* 2 margin)
               (:width inner))
     :height (+ (* 2 margin)
                (:height inner))}))

(defn box-give-space [{:keys [available-width available-height margin] :as node}]
  (spec/assert ::margin margin)
  (update-in node [:children]
             (fn [[outer inner]]
               [(assoc outer
                       :available-width available-width
                       :available-height available-height)
                (assoc inner
                       :available-width (- available-width
                                           (* 2 margin))
                       :available-height (- available-height
                                            (* 2 margin)))])))

(defn box-make-layout [{:keys [width height margin] :as node}]
  (update-in node
             [:children]
             (fn [[outer inner]]
               [(assoc outer
                       :x 0
                       :y 0
                       :z 0
                       :width (max width
                                   (+ (:width inner)
                                      (* 2 margin)))
                       :height (max height
                                    (+ (:height inner)
                                       (* 2 margin))))
                (assoc inner
                       :x margin
                       :y margin
                       :z 1)])))

(defn box [margin outer inner]
  (when (and outer inner)
    {:get-size box-get-size
     :give-space box-give-space
     :make-layout box-make-layout
     :margin margin
     :children [outer inner]}))


;; with-minimum-size

(spec/def ::minimum-width int?)
(spec/def ::minimum-height int?)

(defn minimum-size-get-size [{:keys [minimum-width minimum-height children]}]
  (spec/assert ::minimum-width minimum-width)
  (spec/assert ::minimum-height minimum-height)
  
  {:width (max minimum-width
               (:width (first children)))
   :height (max minimum-height
                (:height (first children)))} )

(defn minimum-size-make-layout [{:keys [width height] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(layout/add-layout (assoc child
                                          :x 0
                                          :y 0
                                          :z 0
                                          :width width
                                          :height height))])))

(defn with-minimum-size [minimum-width minimum-height child]
  (when child
    {:get-size minimum-size-get-size
     :make-layout minimum-size-make-layout

     :minimum-width minimum-width
     :minimum-height minimum-height
     
     :children [child]}))


;; with-margins

(defn with-margins-get-size [{:keys [left-margin right-margin top-margin bottom-margin children]}]
  (let [child (first children)]
    {:width (+ left-margin
               right-margin
               (:width child))
     :height (+ top-margin
                bottom-margin
                (:height child))}))

(defn with-margins-give-space [{:keys [available-width available-height left-margin right-margin top-margin bottom-margin] :as node}]
  (update-in node [:children]
             (fn [[child]]
               [(assoc child
                       :available-width (- available-width
                                           left-margin
                                           right-margin)
                       :available-height (- available-height
                                            top-margin
                                            bottom-margin))])))

(defn with-margins-make-layout [{:keys [left-margin top-margin] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(assoc child
                       :x left-margin
                       :y top-margin)])))

(defn with-margins [top-margin right-margin bottom-margin left-margin child]
  (when child
    {:get-size with-margins-get-size
     :give-space with-margins-give-space
     :make-layout with-margins-make-layout

     :top-margin top-margin
     :right-margin right-margin
     :bottom-margin bottom-margin
     :left-margin left-margin

     :children [child]}))

(defn superimpose [& children]
  (let [children (flatten-contents children)]
    {:children children
     :get-size (fn [node]
                 (let [child-sizes (map layout/size
                                        children)]
                   {:width (apply max
                                  (map :width
                                       child-sizes))
                    :height (apply max
                                   (map :height
                                        child-sizes))}))}))
