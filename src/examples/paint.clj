(ns examples.paint
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [atom-registry :as atom-registry]
                   [application :as application]
                   [layouts :as layouts]
                   [layout :as layout])
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
            [flow-gl.graphics.buffered-image :as buffered-image]))

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

(def diff-fragment-shader-source "
  #version 140

  uniform sampler2D target_texture;
  uniform sampler2D source_texture;

  in vec2 texture_coordinate;

  out vec4 outColor;

  void main() {
    // outColor = texture(source_texture, texture_coordinate);
//  outColor = vec4(1,0,0, texture(source_texture, texture_coordinate).a);
      outColor = vec4(1,0,0, abs(texture(target_texture, texture_coordinate).a - texture(source_texture, texture_coordinate).a));

  }
  ")

(defn create-render-target [gl width height]
  (let [render-target (render-target/create width height gl)]
    (render-target/render-to render-target gl
                             (opengl/clear gl 0 0 0 0))
    render-target))

(defn canvas-state-atom-specification [gl width height]
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
                                                  (opengl/clear gl 0 0 0 1)
                                                  (when canvas-state-atom
                                                    (quad/draw gl
                                                               ["target_texture" (texture/create-for-buffered-image target-buffered-image gl)
                                                                "source_texture" (:texture (:target @canvas-state-atom))]
                                                               []
                                                               (quad/create-program diff-fragment-shader-source gl)
                                                               0 0
                                                               diff-width
                                                               diff-height
                                                               diff-width
                                                               diff-height))))))}))

(def target-buffered-image (buffered-image/create-from-file "pumpkin.png"))

(defn create-scene-graph [width height]
  (let [canvas-width 500
        canvas-height 500
        event-state-atom (atom-registry/get! :state {:create (fn [] {:points []})})]
    (animation/swap-state! animation/set-wake-up 1000)
    (-> {:x 0
         :y 0
         :width width
         :height height
         :children [(layouts/horizontally-with-margin 10
                                                      (layouts/vertically-with-margin 10
                                                                                      (visuals/image target-buffered-image)
                                                                                      (diff-view target-buffered-image
                                                                                                 [:canvas-state canvas-width canvas-height]))
                                                      {:x 200
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
                                                                 (let [canvas-state-atom (atom-registry/get! [:canvas-state canvas-width canvas-height] (canvas-state-atom-specification gl canvas-width canvas-height))
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
                                                                     (render-target/render-to (:target @canvas-state-atom) gl
                                                                                              #_(opengl/clear gl 0 0 0 0)
                                                                                              (let [program (quad/create-program fragment-shader-source gl)]
                                                                                                (quad/draw gl
                                                                                                           ["texture" (:texture (:source @canvas-state-atom))]
                                                                                                           [:2fv "points" coordinates
                                                                                                            :1i "number_of_points" (/ (count points)
                                                                                                                                      2)]
                                                                                                           program
                                                                                                           0 0
                                                                                                           canvas-width
                                                                                                           canvas-height
                                                                                                           canvas-width
                                                                                                           canvas-height)))

                                                                     (swap! canvas-state-atom (fn [state]
                                                                                                (assoc state
                                                                                                       :source (:target state)
                                                                                                       :target (:source state)))))

                                                                   (assoc (select-keys scene-graph [:x :y])
                                                                          :width canvas-width
                                                                          :height canvas-height
                                                                          :texture-id (:texture (:source @canvas-state-atom))
                                                                          :texture-hash (hash scene-graph))))})]}
        (application/do-layout width height))))

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))
