(ns flow-gl.file-map)


#_(deftype FileMap [a b]
  clojure.lang.IPersistentCollection
  (seq [self] (if (seq a) self nil))
  (cons [self o] (Foo. a (conj b o)))
  (empty [self] (Foo. [] []))
  (equiv
   [self o]
   (if (instance? Foo o)
     (and (= a (.a o))
          (= b (.b o)))
     false))
  clojure.lang.ISeq
  (first [self] (first a))
  (next [self] (next a))
  (more [self] (rest a))
  Object
  (toString [self] (str "Foo of a: " a ", b: " b)))

#_(defn assoc [file-map]
  )
