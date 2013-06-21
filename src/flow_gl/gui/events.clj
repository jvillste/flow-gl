(ns flow-gl.gui.events
  (:require  [flow-gl.dataflow :as dataflow]
             [flow-gl.gui.awt-input :as input]))


(defn on-key-apply-one [state event key path function]
  `((input/key-pressed? ~event ~key)
    (dataflow/apply-to-value ~state ~path ~function)))

(defmacro on-key-apply [state event & specs]
  (let [specs (partition 3 specs)]
    `(cond ~@(mapcat (fn [[key path function]]
                       (on-key-apply-one state event key path function))
                     specs )
           :default ~state)))
