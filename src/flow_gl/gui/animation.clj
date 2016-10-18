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

(defn remove-wake-up [state]
  (dissoc state :sleep-time))


(defn adjust-sleep-time-according-to-target-frames-per-second [state target-frames-per-second current-time-in-milliseconds]
  (if (:sleep-time state)
    (let [minimum-sleep-time (/ 1000 target-frames-per-second)
          previous-frame-duration (- current-time-in-milliseconds
                                     (or (:previous-frame-started state)
                                         current-time-in-milliseconds))
          sleep-time (float (max (:sleep-time state)
                                 (- minimum-sleep-time
                                    (max 0
                                         (- previous-frame-duration
                                            (or (:previous-sleep-time state)
                                                0))))))]
      (assoc state
             :sleep-time sleep-time
             :previous-sleep-time sleep-time
             :previous-frame-started current-time-in-milliseconds))  
    state))

(deftest adjust-sleep-time-according-to-target-frames-per-second-test
  (testing "Sleep time should be nil when no wake up is requested"
    (is (= nil
           (:sleep-time (adjust-sleep-time-according-to-target-frames-per-second {:sleep-time nil}
                                                                                 1
                                                                                 0)))))
  (is (= 900
         (:sleep-time (adjust-sleep-time-according-to-target-frames-per-second {:sleep-time 0
                                                                                :previous-frame-started 0
                                                                                :previous-sleep-time 100}
                                                                               1
                                                                               200))))
  (is (= 1000
         (:sleep-time (adjust-sleep-time-according-to-target-frames-per-second {:sleep-time 1000
                                                                                :previous-frame-started 0
                                                                                :previous-sleep-time 100}
                                                                               1
                                                                               200)))))


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

;; phasers

(defn linear-phaser [duration runtime]
  (/ runtime
     duration))

#_(defn once [runtime duration]
    (let [phase (min 1
                     (/ runtime
                        duration))]
      {:sleep (if (<= phase 1)
                0
                nil)
       :phase phase}))

#_(defn repeat [runtime cycle-time]
    {:phase (/ (mod runtime
                    cycle-time)
               cycle-time)
     :sleep 0})

#_(defn call-phaser [phaser & phaser-arguments]
    (let [{:keys [phase sleep-time]} (apply phaser
                                            phaser-arguments)]
      (swap-state! set-wake-up sleep-time)
      
      phase))


;; mappings

(defn linear-mapping [phase from to]
  (+ from
     (* (- to from)
        phase )))


;; runners

#_(defn once [phase]
    (and (<= phase 1)
         (>= phase 0)))

#_(defn repeat [phase]
    true)


;; animation state

(defn animation-state [state key]
  (get-in state [:animations key]))

(defn update-animation [state key function & arguments]
  (apply update-in state [:animations key] function arguments))


(defn start [state key]
  (update-animation state key assoc :start-time (:time state)))

(defn stop [state key]
  (update-animation state key assoc
                    :start-time nil
                    :phase-offset (:phase (animation-state state key))))

(defn toggle [state key]
  (if (:start-time (animation-state state key))
    (stop state key)
    (start state key)))


(defn runtime [state key]
  (let [{:keys [start-time]} (animation-state state key)]
    (if start-time
      (- (:time state)
         start-time)
      0)))

(defn set-reversed [state key reversed]
  (update-animation state key assoc
                    :start-time (:time state)
                    :phase-offset (:phase (animation-state state key))
                    :reversed reversed))

(defn limit [min max value]
  (if (< value min)
    min
    (if (> value max)
      max
      value)))

(defn ping-pong [phase]
  (let [limited-phase (mod phase 2)]
    (if (> limited-phase
           1)
      (- 2 limited-phase)
      limited-phase)))

;; Dynamic state

(def ^:dynamic state-atom)

(defn state! []
  @state-atom)

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

(defn start! [key]
  (swap-state! start key)
  (swap-state! set-wake-up 0))

(defn stop! [key]
  (swap-state! stop key))

(defn toggle! [key]
  (swap-state! toggle key))

(defn reverse! [key reversed]
  (swap-state! set-reversed key reversed)
  (swap-state! set-wake-up 0))

(defn set-wake-up-in-range! [min-phase max-phase phase]
  (when (and (< phase max-phase)
             (> phase min-phase))
    (swap-state! set-wake-up 0)))

(defn limit! [min-phase max-phase phase]
  (set-wake-up-in-range! min-phase
                         max-phase
                         phase)

  (float (limit min-phase
                max-phase
                phase)))

(defn no-limit! [key phase]
  (when (:start-time (animation-state @state-atom key))
    (swap-state! set-wake-up 0))
  phase)

(defn ping-pong-once! [phase]
  (ping-pong (limit! 0 2 phase)))

(defn infinite-ping-pong! [key phase]
  (when (:start-time (animation-state @state-atom key))
    
    (swap-state! set-wake-up 0))
  
  (ping-pong phase))

(defn phase! [key phaser limiter transformer]

  (let [{:keys [phase-offset reversed]} (animation-state @state-atom key)

        limited-phase  (-> (runtime @state-atom key)
                           (phaser)
                           (* (if reversed
                                -1
                                1))
                           (+ (or phase-offset
                                  0))
                           (limiter))]

    (swap-state! update-animation key
                 assoc :phase (float limited-phase))

    
    (transformer limited-phase)))


;; TODO:
;; animate multiple properties at once
;; more than two keyframes per animation
;; delays during animation
;; phaser over many keyframes
