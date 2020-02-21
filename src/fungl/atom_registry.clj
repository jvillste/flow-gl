(ns fungl.atom-registry
  (:require [fungl.depend :as depend]
            [fungl.value-registry :as value-registry])
  (:refer-clojure :exclude [get]))

(defn add-dependency [value-atom]
  (depend/add-dependency (assoc (meta value-atom)
                                :type ::atom-registry)
                         @value-atom))

(defn refine-specification [id specification]
  (-> specification
      (update :create (fn [create]
                        (fn []
                          (atom (create)
                                :meta {:id id
                                       :specification specification}))))
      (assoc :on-get (fn [id value-atom]
                       (add-dependency value-atom)
                       (when-let [on-get (:on-get specification)]
                         (on-get id value-atom))))))


;; dynamic state

(defn deref! [value-atom]
  (add-dependency value-atom)
  @value-atom)

(defn get!
  ([id specification]
   (value-registry/get! id
                        (refine-specification id specification)))
  ([id]
   (value-registry/get! id)))

(defmethod depend/current-value ::atom-registry [dependency]
  @(value-registry/get! (:id dependency)
                        (refine-specification (:id dependency)
                                              (:specification dependency))))

(defmethod depend/dependency-added ::atom-registry [dependency]
  (value-registry/mark-reference! (:id dependency)))
