(ns fungl.callable)

(defn call [callable & arguments]
  (cond (vector? callable)
        (apply (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply callable arguments)))

(defn call-arguments-first [callable & arguments]
  (cond (vector? callable)
        (apply (first callable)
               (concat arguments
                       (rest callable)))

        (fn? callable)
        (apply callable arguments)))
