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
  (is (= 900.0
         (:sleep-time (adjust-sleep-time-according-to-target-frames-per-second {:sleep-time 0
                                                                                :previous-frame-started 0
                                                                                :previous-sleep-time 100}
                                                                               1
                                                                               200))))
  (is (= 1000.0
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
       :runtime phase}))

#_(defn repeat [runtime cycle-time]
    {:runtime (/ (mod runtime
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
  (update-animation state key assoc
                    :start-time (:time state)))

(defn restart [state key]
  (update-animation state key assoc
                    :start-time (:time state)
                    :runtime-offset 0))

(defn stop [state key]
  (update-animation state key assoc
                    :start-time nil
                    :runtime-offset (:runtime (animation-state state key))))

(defn toggle [state key]
  (if (:start-time (animation-state state key))
    (stop state key)
    (start state key)))


(defn running? [state key]
  (:start-time (animation-state state key)))

(defn runtime [state key]
  (let [{:keys [start-time]} (animation-state state key)]
    (if start-time
      (- (:time state)
         start-time)
      0)))

(defn set-reversed [state key reversed]
  (update-animation state key assoc
                    :start-time (:time state)
                    :runtime-offset (:runtime (animation-state state key))
                    :reversed reversed))

(defn limit [min max value]
  (if (and min
           (< value min))
    min
    (if (and max
             (> value max))
      max
      value)))

#_(defn ping-pong [duration time]
    (let [phase (/ time duration)
          half-time (mod time (/ duration
                                 2))]
      (-> (if (>= time
                  (/ duration
                     2))
            (- duration
               half-time)
            half-time)
          (/ (/ duration 2)))))

(defn ping-pong [duration time]
  (let [phase (mod (/ time duration)
                   1)
        ping-pong-phase (* 2 (if (> phase
                                    0.5)
                               (- 1
                                  phase)
                               phase))]
    ping-pong-phase))

(deftest ping-pong-test
  (is (= 0 (ping-pong 1 0)))
  (is (= 0.5 (ping-pong 1 0.25)))
  (is (= 1.0 (ping-pong 1 0.5)))
  (is (= 0.5 (ping-pong 1 0.75)))
  (is (= 0 (ping-pong 1 1)))
  (is (= 0.5 (ping-pong 1 1.25)))

  (is (= 0 (ping-pong 2 0)))
  (is (= 0.5 (ping-pong 2 0.5)))
  (is (= 1N (ping-pong 2 1)))
  (is (= 0.5 (ping-pong 2 1.5)))
  (is (= 0 (ping-pong 2 2))))

(defn wake-up-in-range [min-phase max-phase phase]
  (if (and (or (not max-phase) (<= phase max-phase))
           (or (not min-phase) (>= phase min-phase)))
    0
    nil))

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
  (swap-state! start key))

(defn restart! [key]
  (swap-state! restart key))

(defn stop! [key]
  (swap-state! stop key))

(defn toggle! [key]
  (swap-state! toggle key))

(defn reverse! [key reversed]
  (swap-state! set-reversed key reversed))

(defn toggle-direction! [key]
  (swap-state! set-reversed key (let [reversed (:reversed (animation-state @state-atom
                                                                           key))]
                                  (if (or reversed
                                          (= reversed nil))
                                    false
                                    true))))

(defn set-wake-up-in-range! [min-phase max-phase phase]
  (when (and (< phase max-phase)
             (> phase min-phase))
    (swap-state! set-wake-up 0)))

#_(defn limit! [min-phase max-phase phase]
    (set-wake-up-in-range! min-phase
                           max-phase
                           phase)

    (float (limit min-phase
                  max-phase
                  phase)))

#_(defn no-limit! [key phase]
    (when (:start-time (animation-state @state-atom key))
      (swap-state! set-wake-up 0))
    phase)

#_(defn ping-pong-once! [phase]
    (ping-pong (limit! 0 2 phase)))

#_(defn infinite-ping-pong! [key phase]
    (when (:start-time (animation-state @state-atom key))
      
      (swap-state! set-wake-up 0))
    
    (ping-pong phase))

#_(defn phase! [key phaser limiter wake-up]

    (let [{:keys [runtime-offset reversed]} (animation-state @state-atom key)
          phase (-> (runtime @state-atom key)
                    (phaser)
                    (* (if reversed
                         -1
                         1))
                    (+ (or runtime-offset
                           0)))

          limited-phase (limiter phase)]

      (when (running? @state-atom key)
        (swap-state! set-wake-up (wake-up phase)))

      (swap-state! update-animation key
                   assoc :runtime (float limited-phase))

      limited-phase))

#_(defn runtime-to-phase [runtime speed]
    (* speed
       (/ runtime
          1000)))

(defn transformed-runtime [state key]
  (let [{:keys [runtime-offset reversed]} (animation-state state key)]

    (-> (runtime state key)
        (* (if reversed
             -1
             1))
        (+ (or runtime-offset
               0)))))

(defn transformed-runtime! [key]
  (transformed-runtime @state-atom key))

(defn runtime! [key duration]

  (let [runtime (transformed-runtime! key)

        limited-runtime (limit 0 duration runtime)]

    (when (running? @state-atom key)
      (swap-state! set-wake-up (wake-up-in-range 0 duration runtime)))

    (swap-state! update-animation key
                 assoc :runtime limited-runtime)
    limited-runtime))

(defn phase! [key duration]
  
  (/ (runtime! key duration)
     (or duration 1000)))


(defn interpolate [from to phase]
  (+ (* from (- 1 phase))
     (* to phase)))

(deftest interpolate-test
  (is (= 0
         (interpolate 0 10 0)))

  (is (= 10
         (interpolate 0 10 1)))

  (is (= 5.0
         (interpolate 0 10 0.5)))

  (is (= 2.5
         (interpolate 0 10 0.25))))


(defn key-frame-mapping [phase key-frames]
  (let [key-frames (partition 2 key-frames)
        [key-frame-phase-before key-frame-value-before] (->> key-frames
                                                             (filter (fn [[keyframe-phase value]]
                                                                       (<= keyframe-phase
                                                                           phase)))
                                                             (last))
        [key-frame-phase-after key-frame-value-after] (->> key-frames
                                                           (filter (fn [[keyframe-phase value]]
                                                                     (>= keyframe-phase
                                                                         phase)))
                                                           (first))]

    (if (= key-frame-phase-before
           key-frame-phase-after)
      key-frame-value-before
      (interpolate key-frame-value-before
                   key-frame-value-after
                   (/ (- phase
                         key-frame-phase-before)
                      (- key-frame-phase-after
                         key-frame-phase-before))))))

(deftest key-frame-mapping-test
  (is (= 1
         (key-frame-mapping 0
                            [0 1
                             0.5 10
                             1 5])))

  (is (= 5
         (key-frame-mapping 1
                            [0 1
                             0.5 10
                             1 5])))

  (is (= 10
         (key-frame-mapping 0.5
                            [0 1
                             0.5 10
                             1 5])))


  (is (= 5.0
         (key-frame-mapping 0.25
                            [0 0
                             0.5 10
                             1 5])))

  (is (= 7.5
         (key-frame-mapping 0.75
                            [0 0
                             0.5 10
                             1 5]))))


(defn key-frames-for-key [key multi-channel-key-frames]
  (let [multi-channel-key-frames (partition 2 multi-channel-key-frames)]
    (loop [key-frames []
           multi-channel-key-frames multi-channel-key-frames]
      (if-let [[phase values] (first multi-channel-key-frames)]
        (recur (if-let [value (get values key)]
                 (concat key-frames
                         [phase value])
                 key-frames)
               (rest multi-channel-key-frames))
        
        key-frames))))

(deftest key-frames-for-key-test
  (is (= '(0 0
             0.5 10
             1 1)
         (key-frames-for-key :x
                             [0 {:x 0
                                 :y 0}
                              0.5 {:x 10
                                   :y 5}
                              1 {:x 1
                                 :y 5}]))))

(defn multi-channel-key-frame-mapping [phase multi-channel-key-frames]
  (reduce (fn [values-by-key key]
            (assoc values-by-key
                   key
                   (key-frame-mapping phase
                                      (key-frames-for-key key
                                                          multi-channel-key-frames))))
          {}
          (keys (second multi-channel-key-frames))))

(deftest multi-channel-key-frame-mepping-test
  (is (= {:x 0, :y 0}
         (multi-channel-key-frame-mapping 0
                                          [0 {:x 0
                                              :y 0}
                                           0.5 {:x 10
                                                :y 5}
                                           1 {:x 1
                                              :y 5}])))

  (is (= {:x 5.0, :y 2.5}
         (multi-channel-key-frame-mapping 0.25
                                          [0 {:x 0
                                              :y 0}
                                           0.5 {:x 10
                                                :y 5}
                                           1 {:x 1
                                              :y 5}]))))


;; TODO:
;; animate multiple properties at once
;; more than two keyframes per animation
;; delays during animation
;; phaser over many keyframes
