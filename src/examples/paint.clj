(ns examples.paint
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [atom-registry :as atom-registry]
                   [application :as application]
                   [layouts :as layouts]
                   [layout :as layout]
                   [cache :as cache]
                   [handler :as handler])
            (flow-gl.gui 
             
             [visuals :as visuals]
             [quad-renderer :as quad-renderer]
             [render-target-renderer :as render-target-renderer]
             [animation :as animation]
             [stateful :as stateful])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [quad :as quad]
                                 [texture :as texture]
                                 [render-target :as render-target])
            [flow-gl.graphics.buffered-image :as buffered-image])
  (:use clojure.test))

(def fragment-shader-source "
  #version 140

  uniform int number_of_points;

  uniform sampler2D texture;

  uniform vec2 points[50];

  uniform int erase;

  in vec2 texture_coordinate;

  out vec4 outColor;

  // from https://www.shadertoy.com/view/4dfXDn
  float distance_to_line(vec2 p, vec2 start, vec2 end, float width)
  {
	vec2 dir = start - end;
	float lngth = length(dir);
	dir /= lngth;
	vec2 proj = max(0.0, min(lngth, dot((start - p), dir))) * dir;
	return length( (start - p) - proj ) - (width / 2.0);
  }

  void main() {
    float distance = 1;
    for (int i = 0; i < (number_of_points -1); i++){
      distance = min(distance, distance_to_line(texture_coordinate, points[i], points[i+1], 0.1) / 0.01);
    }
    distance = min(1,distance);

     vec2 flipped_texture_coordinate = vec2(texture_coordinate.x, 1.0 - texture_coordinate.y);
     outColor = vec4(0,0,0, max(texture(texture, flipped_texture_coordinate).a,  1.0 - distance));


    //outColor = vec4(0,0,0, distance); //max( 0.0, 1.0 - distance));
  }
  ")

(def diff-fragment-shader-source "
  #version 140

  uniform sampler2D target_texture;
  uniform sampler2D source_texture;

  in vec2 texture_coordinate;

  out vec4 outColor;

  void main() {
      vec2 flipped_texture_coordinate = vec2(texture_coordinate.x, 1.0 - texture_coordinate.y);

      outColor = vec4(1,0,0, abs(texture(target_texture, texture_coordinate).a - texture(source_texture, flipped_texture_coordinate).a));
  }
  ")

(defn create-render-target [gl width height]
  (let [render-target (render-target/create width height gl)]
    (render-target/render-to render-target gl
                             (opengl/clear gl 1 1 1 0))
    render-target))

(defn canvas-state-atom-specification [gl width height]
  {:create (fn []
             {:source (create-render-target gl width height)
              :target (create-render-target gl width height)})
   :delete (fn [state-atom]
             (render-target/delete (:source @state-atom) gl)
             (render-target/delete (:target @state-atom) gl))})

(defn swap-and-return-old-and-new! [atom f & args]
  (loop []
    (let [old @atom
          new (apply f old args)]
      (if (compare-and-set! atom old new)
        [old new]
        (recur)))))


(defn diff-view [target-buffered-image canvas-state-id]
  (let [diff-width 200
        diff-height 200]
    {:width diff-width
     :height diff-height
     :render (fn [scene-graph gl]
               (let [render-target-renderer-atom (atom-registry/get! :diff (render-target-renderer/atom-specification gl))
                     canvas-state-atom (atom-registry/get! canvas-state-id)]
                 (render-target-renderer/render render-target-renderer-atom gl scene-graph
                                                (fn []
                                                  (opengl/clear gl 1 1 1 1)
                                                  (when canvas-state-atom
                                                    (quad/draw gl
                                                               ["target_texture" (cache/call-with-key! texture/create-for-buffered-image
                                                                                                       target-buffered-image
                                                                                                       target-buffered-image
                                                                                                       gl)
                                                                "source_texture" (:texture (:target @canvas-state-atom))]
                                                               []
                                                               (cache/call-with-key! quad/create-program
                                                                                     diff-fragment-shader-source
                                                                                     diff-fragment-shader-source
                                                                                     gl)
                                                               0 0
                                                               diff-width
                                                               diff-height
                                                               diff-width
                                                               diff-height))))))}))

(def target-buffered-image (buffered-image/create-from-file "pumpkin.png"))

(defn create-events-set [& events]
  (apply sorted-set-by (fn [a b]
                         (compare (:time a)
                                  (:time b)))
         events))

(defn partition-on [predicate xs]
  (loop [xs xs
         partitions []
         partition []]
    (if-let [x (first xs)]
      (if (predicate x)
        (recur (rest xs)
               (if (empty? partition)
                 partitions
                 (conj partitions partition))
               [x])
        (recur (rest xs)
               partitions
               (conj partition x)))
      (if (empty? partition)
        partitions
        (conj partitions partition)))))

(deftest partition-on-test
  (is (= [[1 1] [2 1 1 1] [2]]
         (partition-on even? [1 1 2 1 1 1 2])))

  (is (= [[2 1] [2 1 1] [2]]
         (partition-on even? [2 1 2 1 1 2])))

  (is (= []
         (partition-on even? []))))

(defn strokes [events last-painted-event]
  (->> (if last-painted-event
         (subseq events >= last-painted-event)
         events)
       (partition-on (fn [event]
                       (= :start-stroke
                          (:type event))))))

(deftest strokes-test
  (let [stroke-1 [{:time 1 :type :start-stroke}
                  {:time 2 :type :draw-stroke}
                  {:time 3 :type :draw-stroke}
                  {:time 4 :type :end-stroke}]
        stroke-2 [{:time 10 :type :start-stroke}
                  {:time 11 :type :draw-stroke}
                  {:time 12 :type :end-stroke}]]
    (is (= [[{:time 1, :type :start-stroke}
             {:time 2, :type :draw-stroke}
             {:time 3, :type :draw-stroke}
             {:time 4, :type :end-stroke}]]
           (strokes (apply create-events-set stroke-1)
                    nil)))

    (is (= [stroke-1
            stroke-2]
           (strokes (apply create-events-set (concat stroke-1
                                                     stroke-2))
                    nil)))

    (is (= [[{:time 3, :type :draw-stroke}
             {:time 4, :type :end-stroke}]]
           (strokes (apply create-events-set stroke-1)
                    {:time 3 :type :draw-stroke})))

    (is (= [[{:time 3 :type :draw-stroke}]]
           (strokes (create-events-set {:time 3, :type :draw-stroke})
                    {:time 3 :type :draw-stroke})))

    (is (= []
           (strokes (create-events-set)
                    {:time 3 :type :draw-stroke})))))


(defn draw-stroke [events canvas-state-atom gl]
  (let [width (:width (:target @canvas-state-atom))
        height (:height (:target @canvas-state-atom))
        coordinates (apply concat (map (fn [event]
                                         [(float (/ (:x event)
                                                    width))
                                          (float (/ (:y event)
                                                    height))])
                                       events))]
    (prn coordinates)
    (render-target/render-to (:target @canvas-state-atom) gl
                             (let [program (cache/call-with-key! quad/create-program
                                                                 fragment-shader-source
                                                                 fragment-shader-source
                                                                 gl)]
                               (quad/draw gl
                                          ["texture" (:texture (:source @canvas-state-atom))]
                                          [:2fv "points" coordinates
                                           :1i "number_of_points" (/ (count coordinates)
                                                                     2)]
                                          program
                                          0 0
                                          width
                                          height
                                          width
                                          height)))

    (swap! canvas-state-atom (fn [state]
                               (assoc state
                                      :source (:target state)
                                      :target (:source state))))))

(handler/def-handler-creator create-canvas-renderer [events] [scene-graph gl]
  (let [canvas-state-atom (atom-registry/get! [:canvas-state
                                               (:width scene-graph)
                                               (:height scene-graph)]
                                              (canvas-state-atom-specification gl
                                                                               (:width scene-graph)
                                                                               (:height scene-graph)))
        strokes (strokes events (:last-painted-event @canvas-state-atom))]
    
    (doseq [stroke strokes]
      (when (not= [(:last-painted-event @canvas-state-atom)]
                       stroke)
        
        (draw-stroke (if (= 1 (count stroke))
                       [(first stroke)
                        (first stroke)]
                       stroke)
                     canvas-state-atom
                     gl)))

    (swap! canvas-state-atom assoc :last-painted-event (last (last strokes)))

    (assoc (select-keys scene-graph [:x :y :width :height])
           :texture-id (:texture (:source  @canvas-state-atom))
           :texture-hash (hash scene-graph))))

(handler/def-handler-creator create-canvas-mouse-event-handler [event-state-atom] [node event]
  (when-let [paint-event (case (:type event)
                           :mouse-pressed {:type :start-stroke
                                           :x (:local-x event)
                                           :y (:local-y event)
                                           :time (:time event)}

                           :mouse-dragged {:type :draw-stroke
                                           :x (:local-x event)
                                           :y (:local-y event)
                                           :time (:time event)}

                           :mouse-released {:type :end-stroke
                                            :x (:local-x event)
                                            :y (:local-y event)
                                            :time (:time event)}

                           nil)]
    (swap! event-state-atom update :events conj paint-event))
  event)



(defn create-scene-graph [width height]
  (let [canvas-width 500
        canvas-height 500
        event-state-atom (atom-registry/get! :state {:create (fn [] {:events (create-events-set)})})]
    (animation/swap-state! animation/set-wake-up 1000)
    (-> {:x 0
         :y 0
         :width width
         :height height
         :children [(assoc (visuals/rectangle [255 255 255 255] 0 0)
                           :width width
                           :height height)
                    (layouts/horizontally-with-margin 10
                                                      (layouts/vertically-with-margin 10
                                                                                      (visuals/image target-buffered-image)
                                                                                      (diff-view target-buffered-image
                                                                                                 [:canvas-state canvas-width canvas-height]))
                                                      {:x 200
                                                       :y 0
                                                       :width canvas-width
                                                       :height canvas-height
                                                       :id :canvas
                                                       :mouse-event-handler (create-canvas-mouse-event-handler event-state-atom)
                                                       :render (create-canvas-renderer (:events @event-state-atom))})]}
        (application/do-layout width height))))

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))
