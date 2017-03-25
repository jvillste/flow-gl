(ns examples.paint
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [atom-registry :as atom-registry]
                   [application :as application])
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [render-target-renderer :as render-target-renderer]
                         [animation :as animation]
                         [stateful :as stateful])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [quad :as quad]
                                 [render-target :as render-target])))

(def fragment-shader-source "
  #version 140

  uniform int number_of_points;

  uniform sampler2D texture;

  uniform vec2 points[50];

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
    for (int i = 0; i < (number_of_points - 1); i++){
      distance = min(distance, distance_to_line(texture_coordinate, points[i], points[i+1], 0.1));
    }

    outColor = texture(texture, texture_coordinate) +  vec4(1,0,1, 1.0 - distance_to_line(texture_coordinate, points[0], points[1], 0.1)/ 0.01);

    //outColor = vec4(1,0,1, distance); //max( 0.0, 1.0 - distance));
  }
  ")

(defn create-render-target [gl width height]
  (let [render-target (render-target/create width height gl)]
    (render-target/render-to render-target gl
                             (opengl/clear gl 0 0 0 1))
    render-target))

(defn render-state-atom-specification [gl width height]
  {:create (fn []
             {:source (create-render-target gl width height)
              :target (create-render-target gl width height)
              :blit-program (quad/create-program quad/fragment-shader-source gl)})
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

(defn create-scene-graph [width height]
  (let [event-state-atom (atom-registry/get! :state {:create (fn [] {:points []})})]
    (animation/swap-state! animation/set-wake-up 1000)
    {:x 0
     :y 0
     :width width
     :height height
     :children [(let [canvas-width 500
                      canvas-height 500]
                  {:x 100
                   :y 0
                   :width canvas-width
                   :height canvas-height
                   :id :canvas
                   :mouse-event-handler (fn [node event]
                                          (when (= (:type event)
                                                   :mouse-dragged)
                                            (swap! event-state-atom update :points conj [(:local-x event)
                                                                                         (:local-y event)]))
                                          event)
                   :render (fn [scene-graph gl]
                             (let [render-state-atom (atom-registry/get! [:state canvas-width canvas-height] (render-state-atom-specification gl canvas-width canvas-height))
                                   points (:points (first (swap-and-return-old-and-new! event-state-atom assoc :points [])))
                                   points (if (= 1 (count points))
                                            (concat points points)
                                            points)
                                   points (map (fn [[x y]]
                                                 [(float (/ x canvas-width))
                                                  (- 1.0 (float (/ y canvas-height)))])
                                               points)
                                   coordinates (flatten points)]

                               (when (> (count coordinates)
                                        0)
                                 (opengl/clear gl 0 0 0 1)
                                 #_(let [program (quad/create-program fragment-shader-source gl)]
                                     (quad/draw gl
                                                ["texture" (:texture (:source @render-state-atom))]
                                                [:2fv "points" coordinates
                                                 :1i "number_of_points" (/ (count points)
                                                                           2)]
                                                program
                                                0 0
                                                width
                                                height
                                                width
                                                height))
                                 (render-target/render-to (:target @render-state-atom) gl
                                                          #_(opengl/clear gl 0 0 0 0)
                                                          (let [program (quad/create-program fragment-shader-source gl)]
                                                            (quad/draw gl
                                                                       ["texture" (:texture (:source @render-state-atom))]
                                                                       [:2fv "points" coordinates
                                                                        :1i "number_of_points" (/ (count points)
                                                                                                  2)]
                                                                       program
                                                                       0 0
                                                                       canvas-width
                                                                       canvas-width
                                                                       canvas-width
                                                                       canvas-width)))

                                 (swap! render-state-atom (fn [state]
                                                            (assoc state
                                                                   :source (:target state)
                                                                   :target (:source state)))))

                               #_(quad/draw gl ["texture" (:texture (:target @render-state-atom))]
                                          []
                                          (:blit-program @render-state-atom)
                                          0 0
                                          width
                                          height
                                          width
                                          height)
                               
                               (assoc (select-keys scene-graph [:x :y])
                                      :width canvas-width
                                      :height canvas-height
                                      :texture-id (:texture (:target @render-state-atom))
                                      :texture-hash (hash scene-graph))

                               ))})]}))

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))
