(ns flow-gl.gui.fps)

;; FPS

(defn add-fps [view-state time-now]
  (update-in view-state [:fpss]
             (fn [fpss]
               (let [fpss (conj fpss (/ 1
                                        (/ (- time-now
                                              (:last-update-time view-state))
                                           1E9)))]
                 (if (> (count fpss)
                        20)
                   (vec (rest fpss))
                   fpss)))))

(defn average-fps [view]
  (let [fpss (:fpss view)]
    (/ (apply + fpss)
       (max 1 (count fpss)))))

(defn update-fps [view]
  (let [time-now (System/nanoTime)]
    (-> view
        (add-fps time-now)
        (assoc :fps (average-fps view))
        (assoc :last-update-time time-now))))
