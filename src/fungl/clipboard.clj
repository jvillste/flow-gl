(ns fungl.clipboard
  (:import [java.awt.datatransfer DataFlavor StringSelection]
           [java.awt Toolkit]))

(defn- get-clipboard []
  (.getSystemClipboard (Toolkit/getDefaultToolkit)))

(defn slurp-clipboard []
  (try
    (.getTransferData (.getContents (get-clipboard)
                                    nil)
                      (DataFlavor/stringFlavor))
    (catch Throwable _throwable
      nil)))

(defn spit-clipboard [text]
  (.setContents (get-clipboard)
                (StringSelection. text)
                nil))
