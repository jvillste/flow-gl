(ns flow-gl.gui.events
  (:require [flow-gl.debug :as debug]))

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

(defn on-key-one [event key body]
  `((key-pressed? ~event ~key)
    ~body))

(defmacro on-key [state event & specs]
  (let [specs (partition 2 specs)]
    `(cond ~@(mapcat (fn [[key body]]
                       (on-key-one event key body))
                     specs)
           :default ~state)))


(defn create-close-requested-event []
  {:type :close-requested})

(defn create-resize-requested-event [width height]
  (flow-gl.debug/debug-timed "create resize requested" width height)
  {:type :resize-requested
   :width width
   :height height})

(defn create-keyboard-event [type key character time shift control alt is-auto-repeat]
  {:type type
   :key key
   :character character
   :time time
   :shift shift
   :control control
   :alt alt
   :source :keyboard
   :is-auto-repeat is-auto-repeat})

(defn create-mouse-event [type x y key time]
  {:x x
   :y y
   :type type
   :source :mouse
   :time time})
