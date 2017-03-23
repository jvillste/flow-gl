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

    outColor = vec4(1,1,1, min(0.1, distance/ 0.002));
  }
  ")

(defn create-render-target [gl width height]
  (let [render-target (render-target/create width height gl)]
    (render-target/render-to render-target gl
                             (opengl/clear gl 0 0 0 1))
    render-target))

(defn atom-specification [gl width height]
  {:create (fn []
             {:source (create-render-target gl width height)
              :target (create-render-target gl width height)})
   :delete (fn [state-atom]
             (render-target/delete (:source @state-atom) gl)
             (render-target/delete (:target @state-atom) gl))})

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 1000)
  {:x 0
   :y 0
   :width width
   :height height
   :render (fn [scene-graph gl]
             (let [state-atom (atom-registry/get! [:state width height] (atom-specification gl width height))]
               (opengl/clear gl 0 0 0 0)
               
               (render-target/render-to (:target @state-atom)
                                        gl

                                        (let [program (quad/create-program fragment-shader-source gl)]
                                          (let [points [0.8 0.2
                                                        0.8 0.8
                                                        0.6 1.0
                                                        0.5 0.5
                                                        0.2 0.2]]
                                            (quad/draw gl ["texture" (:texture (:source @state-atom))]
                                                       [:2fv "points" points
                                                        :1i "number_of_points" (/ (count points)
                                                                                  2)]
                                                       program
                                                       0 0
                                                       width
                                                       height
                                                       width
                                                       height))))

               (quad/draw gl ["texture" (:texture (:target @state-atom))]
                          []
                          (quad/create-program quad/fragment-shader-source gl)
                          0 0
                          width
                          height
                          width
                          height)
               
               (swap! state-atom (fn [state]
                                   (assoc state
                                          :source (:target state)
                                          :target (:source state))))))})

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))
