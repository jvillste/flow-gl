(ns fungl.atom-registry
  (:require [fungl.depend :as depend]
            [fungl.value-registry :as value-registry])
  (:refer-clojure :exclude [get]))

(defn refine-specification [specification]
  (-> specification
      (update :create (partial comp atom))
      (assoc :on-get (fn [id value-atom]
                       (depend/add-dependency {:type ::atom-registry
                                               :atom value-atom
                                               :id id}
                                              @value-atom)
                       (when-let [on-get (:on-get specification)]
                         (on-get id value-atom))))))


;; dynamic state


(defn get! [id specification]
  (value-registry/get! id
                       (refine-specification specification)))

(defmethod depend/current-value ::atom-registry [dependency]
  @(:atom dependency))

(defmethod depend/dependency-added ::atom-registry [dependency]
  (value-registry/mark-reference! (:id dependency)))

