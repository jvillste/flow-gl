(ns flow-gl.gui.events
  (:require  [flow-gl.dataflow.triple-dataflow :as triple-dataflow]
             [flow-gl.gui.application :as application]))

(defn key-pressed? [keyboard-event key]
  (and (= (:key keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

#_(defn on-key-apply-one [state event key path function]
  `((key-pressed? ~event ~key)
    (dataflow/apply-to-value ~state ~path ~function)))

#_(defmacro on-key-apply [state event & specs]
  (let [specs (partition 3 specs)]
    `(cond ~@(mapcat (fn [[key path function]]
                       (on-key-apply-one state event key path function))
                     specs )
           :default ~state)))

(defn close-on-esc [view-state event]
  (if (key-pressed? event :esc)
    (do (application/close (::triple-dataflow/dataflow view-state) )
        view-state)
    view-state))



(defn create-close-requested-event []
  {:type :close-requested})

(defn create-resize-requested-event [width height]
  {:type :resize-requested
   :width width
   :height height})

(defn create-keyboard-event [type key character time]
  {:type type
   :key key
   :character character
   :time time
   :source :keyboard})
