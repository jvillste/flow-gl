(ns fungl.render)

(defn keys-to-seq [source-map keys]
  (reduce (fn [result key]
            (conj result (key source-map)))
          []
          keys))

(defn image-function-parameters [quad]
  (-> []
      (cond-> (:width-dependent quad)
        (conj (:width quad)))
      (cond-> (:height-dependent quad)
        (conj (:height quad)))
      (concat (if-let [image-function-parameters (:image-function-parameter-keys quad)]
                (keys-to-seq quad
                             image-function-parameters)
                []))))
