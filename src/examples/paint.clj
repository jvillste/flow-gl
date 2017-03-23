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
                                 [quad :as quad])))

(def fragment-shader-source "
  #version 140

  uniform float points;

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
  outColor = vec4(1,1,1,distance_to_line(texture_coordinate, vec2(0.2,0.2), vec2(0.5,0.5), 0.1) / 0.005);
  }
  ")

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 1000)
  {:x 0
   :y 0
   :width width
   :height height
   :render (fn [scene-graph gl]
             (let [program (quad/create-program fragment-shader-source gl)]
               (opengl/clear gl 0 0 0 0)
               (quad/draw gl [] [:1f "x" 0.0]
                          program
                          0 0
                          width
                          height
                          width
                          height))
             )})

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))
