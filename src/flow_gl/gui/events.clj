(ns flow-gl.gui.events
  (:require  [flow-gl.dataflow.triple-dataflow :as triple-dataflow]))

(defn key-pressed? [keyboard-event key]
  (and (= (:key keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn on-key-apply-one [state event key predicate function]
  `((key-pressed? ~event ~key)
    (update-in ~state [~predicate] ~function)))

(defmacro on-key-apply [state event & specs]
  (let [specs (partition 3 specs)]
    `(cond ~@(mapcat (fn [[key predicate function]]
                       (on-key-apply-one state event key predicate function))
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
