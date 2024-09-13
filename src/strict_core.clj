(ns strict-core
  "clojure.core with type assertions.

   Usage:

   (ns my-ns
     (:require [strict-core :refer :all]))"
  (:refer-clojure :exclude [get-in]))

(defn get-in
  ([m ks]
   (assert (instance? clojure.lang.Associative
                      m))
   (assert (instance? clojure.lang.Seqable
                      ks))
   (clojure.core/get-in m ks))

  ([m ks not-found]
   (assert (instance? clojure.lang.Associative
                      m))
   (assert (instance? clojure.lang.Seqable
                      ks))
   (clojure.core/get-in m ks not-found)))
