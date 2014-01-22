(ns flow-gl.gui.events
  (:require  [flow-gl.dataflow.triple-dataflow :as triple-dataflow]))

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
