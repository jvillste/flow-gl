(ns flow-gl.graphics.native-buffer
  (:import [com.jogamp.common.nio Buffers]
           [java.nio FloatBuffer IntBuffer]))

#_(def native-buffers (atom {}))

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

(defn buffer-type [native-buffer]
  (let [type (type native-buffer)]
    (cond (isa? type java.nio.IntBuffer)
          :int

          (isa? type java.nio.FloatBuffer)
          :float

          (isa? type java.nio.ByteBuffer)
          :byte

          (isa? type java.nio.ShortBuffer)
          :short)))

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

(defn coercion [type]
  (case type
    :int unchecked-int
    :byte unchecked-byte
    :short unchecked-short
    :float float))

(defn put-values [native-buffer values]
  (let [coerce (coercion (buffer-type native-buffer))]
    (doseq [value values]
      (.put native-buffer (coerce value)))
    (.rewind native-buffer)
    native-buffer))

(defn ensure-buffer-capacity [native-buffer minimum-capacity]
  (let [native-buffer (if (>= (.capacity native-buffer)
                              minimum-capacity)
                        native-buffer
                        (create-native-buffer (buffer-type native-buffer) (buffer-capacity minimum-capacity)))]

    (.rewind native-buffer)
    (.limit native-buffer minimum-capacity)
    native-buffer))

(defn ensure-buffer-capacity-with-values [native-buffer values]
  (let [native-buffer (ensure-buffer-capacity native-buffer
                                                            (count values))]
    (put-values native-buffer values)))

#_(defn add-native-buffer [type minimum-capacity]
    (let [native-buffer (create-native-buffer type
                                              (buffer-capacity minimum-capacity))]
      (swap! native-buffers assoc
             type
             native-buffer)
      native-buffer))

#_(defn native-buffer [type minimum-capacity]
    (let [native-buffer (if-let [native-buffer (get @native-buffers type)]
                          (if (>= (.capacity native-buffer)
                                  minimum-capacity)
                            native-buffer
                            (add-native-buffer type minimum-capacity))
                          (add-native-buffer type minimum-capacity))]
      (.rewind native-buffer)
      (.limit native-buffer minimum-capacity)
      native-buffer))





(defn create-native-buffer-with-values [type values]
  (let [native-buffer (create-native-buffer type (count values))]
    (put-values native-buffer values)
    native-buffer))
