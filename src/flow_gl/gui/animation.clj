(ns flow-gl.gui.animation
  (:use clojure.test))

(defn initialize-state []
  {})

(defn set-time-in-milliseconds [state time]
  (assoc state :time time))

(defn choose-sleep-time [sleep-time-1 sleep-time-2]
  (if sleep-time-1
    (if sleep-time-2
      (min sleep-time-1 sleep-time-2)
      sleep-time-1)
    sleep-time-2))

(defn set-wake-up [state sleep-time]
  (update-in state [:sleep-time]
             choose-sleep-time sleep-time))

(defn save-sleep-time [state]
  (-> state
      (assoc :previous-sleep-time (:sleep-time state))
      (dissoc :sleep-time)))


(defn adjust-sleep-time-according-to-target-frames-per-second [state target-frames-per-second]
  (if (:sleep-time state)
    (let [minimum-sleep-time (/ 1000 target-frames-per-second)
          previous-frame-duration (- (System/currentTimeMillis)
                                     (or (:previous-frame-started state)
                                         (System/currentTimeMillis)))
          sleep-time (max (:sleep-time state)
                          (- minimum-sleep-time
                             (max 0
                                  (- previous-frame-duration
                                     (or (:previous-sleep-time state)
                                         0)))))]
      (assoc state
             :sleep-time sleep-time
             :previous-frame-started (System/currentTimeMillis)))
    state))


(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom (initialize-state))})

(defn swap-state! [function & arguments]
  (apply swap! state-atom function arguments))

(defmacro with-time! [given-time & body]
  `(let [old-time (:time @state-atom)]
     (swap-state! set-time given-time)
     ~@body
     (swap-state! set-time old-time)))

(defn time! []
  (:time @state-atom))

(defn sine [from to duration-in-seconds time-in-millis]
  (let [duration-in-millis (* duration-in-seconds 1e6)]
    (-> time-in-millis
        (mod duration-in-millis)
        (/ duration-in-millis)
        (* 2 Math/PI)
        (+ (* 1.5 Math/PI))
        (Math/sin)
        (+ 1)
        (/ 2)
        (* (- to from))
        (+ from))))

(deftest sine-test
  (are [time result] (= result
                        (sine 0 10 1 time))
    0.0 0.0
    (* 1e6 2) 0.0
    (* 1e6 1) 0.0
    (* 1e6 1/2) 10.0))

(defn once [runtime duration]
  (let [phase (min 1
                   (/ runtime
                      duration))]
    {:sleep (if (< phase 1)
              0
              nil)
     :phase phase}))

(defn repeat [runtime cycle-time]
  {:phase (/ (mod runtime
                  cycle-time)
             cycle-time)
   :sleep 0})

(defn linear [phase from to]
  (+ from
     (* (- to from)
        phase )))

(defn call-phaser [phaser & phaser-arguments]
  (let [{:keys [phase sleep-time]} (apply phaser
                                          phaser-arguments)]
    (swap-state! set-wake-up sleep-time)
    phase))

(defn start-stoppable-animation [state key time]
  (-> state
      (assoc [key :running] true)
      (assoc [key :started] (- time
                               (or (get state [key :runtime])
                                   0)))))

(defn stop-stoppable-animation [state key time]
  (-> state
      (assoc [key :running] false)
      (assoc [key :runtime] (- time
                               (get state [key :started])))))

(defn toggle-stoppable-animation [state key time]
  (if (get state [key :running])
    (stop-stoppable-animation state key time)
    (start-stoppable-animation state key time)))

(defn stoppable-animation-runtime [state key]
  (if (get state [key :running])
    (- (:time @state-atom)
       (get state [key :started]))
    (or (get state [key :runtime])
        0)))

(defn stoppable-animation-running [state key]
  (get state [:animation :running]))

(defn stoppable-animation-phase [state key phaser & phaser-arguments]
  (let [{:keys [phase sleep]} (apply phaser
                                     (stoppable-animation-runtime state :animation)
                                     phaser-arguments)]
    (when (stoppable-animation-running state :animation)
      (swap-state! set-wake-up sleep))
    phase))



