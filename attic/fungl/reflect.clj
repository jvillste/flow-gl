(ns fungl.reflect
  (:require [clojure.reflect :as reflect]))

(defn search-method [type search]
  (filter (fn [method] (.contains (.toLowerCase (name (:name method)))
                                  (.toLowerCase search)))
          (:members (reflect/reflect type))))
