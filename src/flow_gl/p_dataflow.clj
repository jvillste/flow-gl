(ns flow-gl.p-dataflow)

(defprotocol Dataflow
  (define [dataflow cell function-or-value])
  (undefine [dataflow cell])
  (get-value [dataflow cell])
  (is-defined? [dataflow cell])
  (initialize [dataflow cell function])
  (update-cell [dataflow cell])
  (propagate-changes [dataflow]))


(defn undefine-many [dataflow cells]
  (reduce (fn [dataflow cell]
            (undefine dataflow cell))
          dataflow
          cells))


(defn get-value-or-initialize [dataflow cell default]
  (when (not (is-defined? dataflow cell))
    (initialize dataflow cell default))
  (get-value dataflow cell))

(defn apply-to-value [dataflow cell function & arguments]
  (define dataflow cell (apply function (get-value dataflow cell) arguments)))
