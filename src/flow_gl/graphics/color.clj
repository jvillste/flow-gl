(ns flow-gl.graphics.color)

(defn convert-color-channel-values-to-floats [color-channel-values]
  (map (fn [color-channel-value]
         (if (or (integer? color-channel-value)
                 (< 1 color-channel-value))
           (float (/ color-channel-value 255))
           (float color-channel-value)))
       color-channel-values))
