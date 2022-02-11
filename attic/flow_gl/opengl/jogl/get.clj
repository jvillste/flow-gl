(ns flow-gl.opengl.jogl.get
  (:require [flow-gl.graphics.native-buffer :as native-buffer]))

(defn integer [gl parameter]
  (let [result-buffer (native-buffer/create-native-buffer :int 1)]
    (.glGetIntegerv gl parameter result-buffer)
    (.get result-buffer 0)))
