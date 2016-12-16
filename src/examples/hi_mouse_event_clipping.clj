(ns examples.hi-tiled-rendering
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (fungl [cache :as cache])
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [tiled-renderer :as tiled-renderer]
                         [animation :as animation]
                         [layouts :as layouts]
                         [scene-graph :as scene-graph]
                         [stateful :as stateful])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [quad :as quad]
                                 [render-target :as render-target])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])))


(def state (atom {}))
(def mask-state (atom 0))

(defn stateful-rectangle [id]
  (let [rectangle-state (get @state id)]
    (-> (visuals/rectangle (if (:mouse-over rectangle-state)
                             [255 255 255 250]
                             [0 255 255 250])
                           80 80)
        (assoc :width 200
               :height 200
               :id id
               :mouse-event-handler (fn [node event]
                                      (swap! state update-in [id]
                                             (fn [rectangle-state]
                                               (if (= (:type event)
                                                      :nodes-under-mouse-changed)
                                                 (if (= id (:id (last (:nodes-under-mouse event))))
                                                   (assoc rectangle-state :mouse-over true)
                                                   (assoc rectangle-state :mouse-over false))
                                                 (if (and (= id (:id (last (:nodes-under-mouse event))))
                                                          (= :mouse-clicked
                                                             (:type event)))
                                                   (do (swap! mask-state (fn [state]
                                                                           (-> (inc (or state 0))
                                                                               (mod 3))))
                                                       rectangle-state)
                                                   rectangle-state))))
                                      
                                      event)))))


(def mask-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D color_texture;
  uniform sampler2D mask_texture;

  out vec4 outColor;

  void main() {
  vec4 color = texture(color_texture, texture_coordinate);
  
  outColor = vec4(color.r, color.g, color.b, texture(mask_texture, texture_coordinate).a * color.a);
  //outColor = texture(mask_texture, texture_coordinate);

  }
  ")

(defn apply-mask [mask scene-graph]
  {:children [(assoc scene-graph
                     :x 0 :y 0)]
   :x (:x scene-graph)
   :y (:y scene-graph)
   :width (:width scene-graph)
   :height (:height scene-graph)
   :clip-mouse-events (fn [node x y]
                        (or (not (scene-graph/in-coordinates? node x y))
                            (not (->> (scene-graph/leaf-nodes mask)
                                      (some (fn [mask-node]
                                              (scene-graph/hits? mask-node
                                                                 (- x (:x node))
                                                                 (- y (:y node)))))))))
   :render (fn [scene-graph gl]
             (let [{:keys [width height]} scene-graph]
               (stateful/with-state-atoms! [quad-renderer-atom :render-target-quad-renderer (quad-renderer/stateful gl)
                                            render-target-1-atom :render-target-1 (render-target/stateful width height gl)
                                            render-target-2-atom :render-target-2 (render-target/stateful width height gl)
                                            render-target-3-atom :render-target-3 (render-target/stateful width height gl)
                                            program-atom [:program (hash mask-shader-source)]  (quad/program-stateful mask-shader-source gl)]
                 
                 (render-target/render-to @render-target-1-atom gl
                                          (opengl/clear gl 0 0 0 0)
                                          (quad-renderer/render quad-renderer-atom gl (assoc scene-graph
                                                                                             :x 0 :y 0)))
                 (render-target/render-to @render-target-2-atom gl
                                          (opengl/clear gl 0 0 0 0)
                                          (quad-renderer/render quad-renderer-atom gl mask))

                 (render-target/render-to @render-target-3-atom gl
                                          (opengl/clear gl 0 0 0 0)
                                          
                                          (quad/draw gl
                                                     ["texture" (:texture @render-target-1-atom)
                                                      "mask_texture" (:texture @render-target-2-atom)]
                                                     []
                                                     @program-atom 
                                                     0 0
                                                     (:width scene-graph)
                                                     (:height scene-graph)
                                                     (:width scene-graph)
                                                     (:height scene-graph)))
                 
                 {:texture-id (:texture @render-target-3-atom)
                  :texture-hash (hash [mask-shader-source
                                       width
                                       height
                                       scene-graph])
                  :x (:x scene-graph)
                  :y (:y scene-graph)
                  :width width
                  :height height})))})

(def font (font/create "LiberationSans-Regular.ttf" 40))
(def pumpkin (buffered-image/create-from-file "pumpkin.png"))

(def mask (assoc (visuals/image pumpkin)
                 :x 100
                 :y 100))

(defn create-scene-graph [width height]
  ;; (animation/swap-state! animation/set-wake-up 1000) ;; TODO: remove this
  
  (let [margin 0]
    {:children (-> [(assoc (visuals/text [255 255 255 255]
                                   font
                                   (prn-str @mask-state))
                           :y 200)]
                   (cond-> (#{0 2} @mask-state)
                     (conj (stateful-rectangle :rectangle-1)))
                   (cond-> (= 1 @mask-state)
                     (conj (cache/call apply-mask mask
                                       (assoc (stateful-rectangle :rectangle-1)
                                              :x 0
                                              :y 0))))
                   (cond-> (= 2 @mask-state)
                     (conj mask)))
     :x margin
     :y margin
     :width (- width (* 2 margin))
     :height (- height (* 2 margin))}))


#_ (create-scene-graph 100 100)

(defn start []
  (do (spec-test/instrument)
      (spec/check-asserts true))
  
  #_(do (spec-test/unstrument)
        (spec/check-asserts false))

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :target-frame-rate 30)))))
