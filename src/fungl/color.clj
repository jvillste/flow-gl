(ns fungl.color)

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
