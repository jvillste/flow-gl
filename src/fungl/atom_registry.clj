(ns fungl.atom-registry
  (:require [fungl.depend :as depend]
            [fungl.value-registry :as value-registry])
  (:refer-clojure :exclude [get]))

(defn refine-specification [specification]
  (let [specification-that-creates-atoms (update specification :create (partial comp atom))]
    (assoc specification-that-creates-atoms
           :on-get (fn [id value-atom]
                     (depend/add-dependency {:type ::atom-registry
                                             :atom value-atom
                                             :id id
                                             :specification specification-that-creates-atoms}
                                            @value-atom)
                     (when-let [on-get (:on-get specification)]
                       (on-get id value-atom))))))


;; dynamic state


(defn get! [id specification]
  (value-registry/get! id
                       (refine-specification specification)))

(defmethod depend/current-value ::atom-registry [dependency]
  @(value-registry/get! (:id dependency)
                        (:specification dependency)))

(defmethod depend/dependency-added ::atom-registry [dependency]
  (value-registry/mark-reference! (:id dependency)))

