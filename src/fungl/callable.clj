(ns fungl.callable)

(defn call [callable & arguments]
  (cond (vector? callable)
        (apply (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply callable arguments)))
