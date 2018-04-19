(ns examples.hi-quad
  (:require [flow-gl.graphics.text :as text]
            (flow-gl.gui [window :as window])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]
                                 [quad :as quad]
                                 [texture :as texture]
                                 [shader :as shader])
            [clojure.java.io :as io])
  
  (:use flow-gl.utils
        clojure.test))


(def blur-fragment-shader-source "
  // adopted from https://github.com/mattdesl/lwjgl-basics/wiki/ShaderLesson5

  #version 140
  
  in vec2 texture_coordinate;

  uniform sampler2D texture;
//  uniform float resolution;
//  uniform float radius;
//  uniform float blur;
  uniform vec2 dir;

  out vec4 outColor;

  void main() {

  //this will be our RGBA sum
  vec4 sum = vec4(0.0);

  //our original texcoord for this fragment
  vec2 tc = texture_coordinate;

  //the amount to blur, i.e. how far off center to sample from 
  //1.0 -> blur by one pixel
  //2.0 -> blur by two pixels, etc.
  float blur = 3.0; //radius/resolution; 

  //the direction of our blur
  //(1.0, 0.0) -> x-axis blur
  //(0.0, 1.0) -> y-axis blur
  float hstep = dir.x;
  float vstep = dir.y;

  //apply blurring, using a 9-tap filter with predefined gaussian weights

  sum += texture(texture, vec2(tc.x - 4.0*blur*hstep, tc.y - 4.0*blur*vstep)) * 0.0162162162;
  sum += texture(texture, vec2(tc.x - 3.0*blur*hstep, tc.y - 3.0*blur*vstep)) * 0.0540540541;
  sum += texture(texture, vec2(tc.x - 2.0*blur*hstep, tc.y - 2.0*blur*vstep)) * 0.1216216216;
  sum += texture(texture, vec2(tc.x - 1.0*blur*hstep, tc.y - 1.0*blur*vstep)) * 0.1945945946;

  sum += texture(texture, vec2(tc.x, tc.y)) * 0.2270270270;

  sum += texture(texture, vec2(tc.x + 1.0*blur*hstep, tc.y + 1.0*blur*vstep)) * 0.1945945946;
  sum += texture(texture, vec2(tc.x + 2.0*blur*hstep, tc.y + 2.0*blur*vstep)) * 0.1216216216;
  sum += texture(texture, vec2(tc.x + 3.0*blur*hstep, tc.y + 3.0*blur*vstep)) * 0.0540540541;
  sum += texture(texture, vec2(tc.x + 4.0*blur*hstep, tc.y + 4.0*blur*vstep)) * 0.0162162162;

   outColor = vec4(1.0, 0.0, 0.0, 1.0) * 1.5 * vec4(sum.rgb, 1.0);
  // outColor = 1.5 * vec4(sum.rgb, 1.0);
  // outColor = sum;
  }")

(defn render [texture gl]
  (let [blur-program (quad/create-program blur-fragment-shader-source gl)
        {:keys [width height]} (opengl/size gl)]
    (opengl/clear gl 0 0 1 1)
    (quad/draw gl
               ["texture" texture]
               [:1f "blur" 3.0
                :2f "dir" [3.0 0.0]]
               blur-program
               100 100
               200 200
               width height)
    (Thread/sleep 1000)))

(defn run-window []
  (let [window (jogl-window/create 400
                                   400
                                   :close-automatically true)]

    (let [texture (window/with-gl window gl
                    (texture/create-for-file (io/resource "pumpkin.png") gl))]
        
        (while (window/visible? window)
          (window/with-gl window gl
            (render texture gl)
            (window/swap-buffers window))))))

(defn start []
  (.start (Thread. (fn [] (run-window)))))






