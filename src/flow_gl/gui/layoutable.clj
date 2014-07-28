(ns flow-gl.gui.layoutable
  (:require clojure.reflect
            clojure.string))

(defprotocol Layoutable
  (preferred-width [element])
  (preferred-height [element])
  (preferred-size [element available-width available-height]))


;; DESCRIPTION

(defn describe-property [layoutable property]
  (str (name property) ": " (property layoutable)))

(defn describe-properties [layoutable properties]
  (apply str (interpose " " (map (partial describe-property layoutable)
                                 properties))))

(defn describe-layoutable [layoutable]
  (let [name (-> (clojure.reflect/typename (type layoutable))
                 (clojure.string/replace  "flow_gl.gui.layout." "")
                 (clojure.string/replace  "flow_gl.gui.drawable." ""))]
    (if (:children layoutable)
      (str "(" name " (" (:state-path-part layoutable) ") " (apply str (interpose " " (map describe-layoutable (:children layoutable)))) ")")
      name)))
