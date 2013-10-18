(ns flow-gl.gui.events
  (:require  [flow-gl.dataflow :as dataflow]
             [flow-gl.gui.awt-input :as input]
             [flow-gl.gui.application :as application]))


(defn on-key-apply-one [state event key path function]
  `((input/key-pressed? ~event ~key)
    (dataflow/apply-to-value ~state ~path ~function)))

(defmacro on-key-apply [state event & specs]
  (let [specs (partition 3 specs)]
    `(cond ~@(mapcat (fn [[key path function]]
                       (on-key-apply-one state event key path function))
                     specs )
           :default ~state)))


(defn close-on-esc [state event]
  (if (input/key-pressed? event input/esc)
    (do (application/request-close)
        state)
    state))
