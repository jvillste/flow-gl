(ns flow-gl.gui.layoutable
  (:require clojure.reflect
            clojure.string))

(defprotocol Layoutable
  (preferred-size [this available-width available-height]))


;; DESCRIPTION

(defn describe-property [layoutable property]
  (str (name property) ": " (property layoutable)))

(defn describe-properties [layoutable properties]
  (apply str (interpose " " (map (partial describe-property layoutable)
                                 properties))))

(defn layoutable-name [layoutable]
  (-> (clojure.reflect/typename (type layoutable))
      (clojure.string/replace  "flow_gl.gui.layouts." "")
      (clojure.string/replace  "flow_gl.gui.drawable." "")))

(defn describe-layoutable [layoutable]
  (let [name (layoutable-name layoutable)]
    (if (:children layoutable)
      (str "(" name " (" (:state-path-part layoutable) ") " (apply str (interpose " " (map describe-layoutable (:children layoutable)))) ")")
      name)))
