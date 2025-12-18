(ns fungl.color
  (:require
   [clojure.test :refer [deftest is]])
  (:import
   [org.hsluv HsluvColorConverter]))

(defn hsluv-to-rgb [hue saturation lightness]
  (let [converter (HsluvColorConverter.)]
    (set! (.hsluv_h converter) hue)
    (set! (.hsluv_s converter) (* 100 saturation))
    (set! (.hsluv_l converter) (* 100 lightness))
    (.hsluvToRgb converter)
    [(.rgb_r converter)
     (.rgb_g converter)
     (.rgb_b converter)]))

(deftest test-hsluv-to-rgb
  (is (= [0.43954415230363597 0.48541117687197427 0.3213345008070774]
         (hsluv-to-rgb 100 0.5 0.5))))

(defn hsl-to-rgb [h s l]
  (let [h (float h)
        s (float s)
        l (float l)

        c (* (- 1
                (Math/abs (- (* 2 l)
                             1)))
             s)

        x (* c
             (- 1
                (Math/abs (- (mod (/ h 60)
                                  2)
                             1))))
        m (- l (/ c 2))
        [r g b] (cond
                  (< h 60) [c x 0]
                  (< h 120) [x c 0]
                  (< h 180) [0 c x]
                  (< h 240) [0 x c]
                  (< h 300) [x 0 c]
                  :else [c 0 x])]
    [(int (* 255 (+ r m)))
     (int (* 255 (+ g m)))
     (int (* 255 (+ b m)))]))

(defn convert-color-channel-values-to-floats [color-channel-values]
  (map (fn [color-channel-value]
         (if (or (integer? color-channel-value)
                 (< 1 color-channel-value))
           (float (/ color-channel-value 255))
           (float color-channel-value)))
       color-channel-values))
