(ns flow-gl.graphics.native-buffer
  (:import [com.jogamp.common.nio Buffers]
           [java.nio FloatBuffer IntBuffer]))

(def native-buffers (atom {}))

(defn float-buffer-to-array [^FloatBuffer float-buffer]
  (let [result (float-array (.limit float-buffer))]
    (.rewind float-buffer)
    (.get float-buffer result)
    result))

(defn int-buffer-to-array [^IntBuffer int-buffer]
  (let [result (int-array (.limit int-buffer))]
    (.rewind int-buffer)
    (.get int-buffer result)
    result))

(defn create-native-buffer [type capacity]
  (case type
    :int (Buffers/newDirectIntBuffer capacity)
    :byte (Buffers/newDirectByteBuffer capacity)
    :short (Buffers/newDirectShortBuffer capacity)
    :float (Buffers/newDirectFloatBuffer capacity)))

(defn buffer-capacity [minimum-capacity]
  (loop [capacity 256]
    (if (>= capacity minimum-capacity)
      capacity
      (recur (* 2 capacity)))))

(defn add-native-buffer [type minimum-capacity]
  (let [native-buffer (create-native-buffer type
                                            (buffer-capacity minimum-capacity))]
    (swap! native-buffers assoc
           type
           native-buffer)
    native-buffer))

(defn native-buffer [type minimum-capacity]
  (let [native-buffer (get @native-buffers type)]
    (if native-buffer
      (if (>= (.capacity native-buffer)
              minimum-capacity)
        native-buffer
        (add-native-buffer type minimum-capacity))
      (add-native-buffer type minimum-capacity))))

(defn coercion [type]
  (case type
    :int unchecked-int
    :byte unchecked-byte
    :short unchecked-short
    :float float))

(defn native-buffer-with-values [type values]
  (let [native-buffer (native-buffer type (count values))
        coerce (coercion type)]
    (.rewind native-buffer)
    (doseq [value values]
      (.put native-buffer (coerce value)))
    (.rewind native-buffer)
    native-buffer))
