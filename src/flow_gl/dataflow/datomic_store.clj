(ns flow-gl.datomic-store
  (:require [datomic.api :as d]))

(def uri "datomic:mem://hello")
(d/create-database uri)
(def conn (d/connect uri))

;; transaction input is data
(def tx-result
  (d/transact
   conn
   [[:db/add
     (d/tempid :db.part/user)
     :db/doc
     "Hello world"]]))
