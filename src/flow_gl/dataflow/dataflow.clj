(ns flow-gl.dataflow.dataflow)

(defprotocol Dataflow
  (define [dataflow cell function-or-value])
  (undefine [dataflow cell])
  (unlogged-get-value [dataflow cell])
  (get-value [dataflow cell])
  (is-defined? [dataflow cell])
  (update-cell [dataflow cell])
  (propagate-changes [dataflow])
  (set-dependencies [dataflow cell reads]))


(defn undefine-many [dataflow cells]
  (reduce (fn [dataflow cell]
            (undefine dataflow cell))
          dataflow
          cells))

(defn initialize [dataflow & keys-and-functions]

  (let [keys-and-functions (->> keys-and-functions
                                (partition 2)
                                (filter (fn [[cell function]]
                                          (not (is-defined? dataflow cell))))
                                (apply concat))]
    (if (empty? keys-and-functions)
      dataflow
      (apply define dataflow keys-and-functions))))

(defn maybe-get-value [dataflow cell]
  (if (is-defined? dataflow cell)
    (get-value dataflow cell)
    nil))

(defn get-value-or-initialize [dataflow cell default]
  (when (not (is-defined? dataflow cell))
    (initialize dataflow cell default))
  (get-value dataflow cell))

(defn apply-to-value [dataflow cell function & arguments]
  (define dataflow cell (apply function (get-value dataflow cell) arguments)))
