(ns examples.editor-scrolling
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            (flow-gl.opengl.jogl [quad :as quad]
                                 [render-target :as render-target]
                                 [opengl :as opengl])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font]))
  (:import [javax.media.opengl GL2])
  (:use flow-gl.utils
        clojure.test))



(defn ensure-render-target [render-target width height gl]
  (if render-target
    (if (and (= width
                (:width render-target))
             (= height
                (:height render-target)))
      render-target
      (do (render-target/delete render-target gl)
          (render-target/create width height gl)))
    (render-target/create width height gl)))

(defn bloom [radius]
  {:transformer (fn [layout gpu-state state]
                  (println "bloom" (:height layout))
                  (let [gl (:gl gpu-state)
                        width (:width layout)
                        height (:height layout)
                        render-target-1 (ensure-render-target (:render-target-1 state) width height gl)
                        render-target-2 (ensure-render-target (:render-target-2 state) width height #_(/ width 4) #_(/ height 4) gl)
                        gpu-state (render-target/render-to render-target-1 gl
                                                           (opengl/clear gl 0 0 0 0)
                                                           (-> (assoc gpu-state :drawables (gui/drawables-for-layout (assoc layout
                                                                                                                            :x 0
                                                                                                                            :y 0)))
                                                               (gui/render-drawables)))

                        gpu-state (render-target/render-to render-target-2 gl
                                                           (opengl/clear gl 0 0 0 0)
                                                           (-> (assoc gpu-state :drawables [(drawable/->Quad ["texture" (:texture render-target-1)]
                                                                                                             [:1f "resolution" width
                                                                                                              :1f "radius" radius
                                                                                                              :2f "dir" [1.0 0.0]]
                                                                                                             ;;quad/fragment-shader-source
                                                                                                             quad/blur-fragment-shader-source
                                                                                                             0 0 width height #_(/ width 4) #_(/ height 4))])
                                                               (gui/render-drawables)))

                        gpu-state (render-target/render-to render-target-1 gl
                                                           (opengl/clear gl 0 0 0 0)
                                                           (-> (assoc gpu-state :drawables [(drawable/->Quad ["texture" (:texture render-target-2)]
                                                                                                             [:1f "resolution" width
                                                                                                              :1f "radius" radius
                                                                                                              :2f "dir" [0.0 1.0]]
                                                                                                             ;;quad/fragment-shader-source
                                                                                                             quad/blur-fragment-shader-source
                                                                                                             0 0 width height)])
                                                               (gui/render-drawables)))]
                    
                    [{:x (:x layout)
                      :y (:y layout)
                      :width width
                      :height height
                      :children [#_(assoc (drawable/->Quad ["texture" (:texture render-target-1)]
                                                           [:1f "resolution" width
                                                            :1f "radius" radius
                                                            :2f "dir" [1.0 0.0]]
                                                           ;;quad/fragment-shader-source
                                                           quad/blur-fragment-shader-source
                                                           0 0 width height)
                                          :content-hash (hash layout))
                                 
                                 (assoc (drawable/->Quad ["texture" (:texture render-target-1)]
                                                         [;; :1f "resolution" width
                                                          ;; :1f "radius" 0
                                                          ;; :2f "dir" [1.0 0.0]
                                                          ]
                                                         quad/fragment-shader-source
                                                         ;; quad/blur-fragment-shader-source
                                                         0 0 width height)
                                        :content-hash (hash layout))
                                 
                                 (assoc layout
                                        :z 1
                                        :x 0
                                        :y 0)]} 
                     gpu-state
                     (assoc state
                            :render-target-1 render-target-1
                            :render-target-2 render-target-2)]))
   
   :destructor (fn [state gl]
                 (when-let [render-target (:render-target state)]
                   (render-target/delete render-target gl)))})


  (def clip-fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;

  out vec4 outColor;

  void main() {
  vec4 color = texture(texture, texture_coordinate);
  outColor = vec4(color.r, color.g, color.b, color.a);
  }
")

(def clip
  {:transformer (fn [layout gpu-state state]
                  (let [gl (:gl gpu-state)
                        width (:width layout)
                        height (:height layout)
                        render-target-1 (ensure-render-target (:render-target state) width height gl)
                        gpu-state (render-target/render-to render-target-1 gl
                                                           (opengl/clear gl 0 0 0 0)
                                                           (-> (assoc gpu-state :drawables (gui/drawables-for-layout (assoc layout
                                                                                                                            :x 0
                                                                                                                            :y 0)))
                                                               (gui/render-drawables)))]
                    
                    [(drawable/->Quad ["texture" (:texture render-target-1)]
                                      []
                                      clip-fragment-shader-source
                                      (:x layout) (:y layout) width height)
                     gpu-state
                     (assoc state
                            :render-target render-target-1)]))
   
   :destructor (fn [state gl]
                 (when-let [render-target (:render-target state)]
                   (render-target/delete render-target gl)))})


(defrecord ScrollPanel [view-context state children]
  layout/Layout
  (layout [this application-state requested-width requested-height]
    (let [#_state #_(gui/get-local-state application-state view-context)
          child-layoutable (let [{preferred-width :width preferred-height :height} (layoutable/preferred-size (first children)
                                                                                                              requested-width
                                                                                                              requested-height)
                                 maximum-x-scroll (- preferred-width requested-width)
                                 maximum-y-scroll (- preferred-height requested-height)
                                 scroll-bar-width 5
                                 scroll-bar-color [255 255 255 120]]
                             (-> (l/superimpose (-> (layouts/->Margin (- (:scroll-position-y state)) 0 0 (- (:scroll-position-x state))
                                                                      [(l/preferred (first children))])
                                                    (assoc :transformer (assoc clip
                                                                               :id :transformer-2)))
                                                (when true #_(:mouse-over state)
                                                      (l/absolute (when (< requested-height preferred-height)
                                                                    (let [scroll-bar-length (* requested-height
                                                                                               (/ requested-height preferred-height))]
                                                                      (assoc (drawable/->Rectangle scroll-bar-width
                                                                                                   scroll-bar-length
                                                                                                   scroll-bar-color)
                                                                             :x (- requested-width scroll-bar-width)

                                                                             :y (* (/ (:scroll-position-y state)
                                                                                      maximum-y-scroll)
                                                                                   (- requested-height
                                                                                      scroll-bar-length)))))
                                                                  (when (< requested-width preferred-width)
                                                                    (let [scroll-bar-length (* requested-width
                                                                                               (/ requested-width preferred-width))]
                                                                      (assoc (drawable/->Rectangle scroll-bar-length
                                                                                                   scroll-bar-width
                                                                                                   scroll-bar-color)
                                                                             :x (* (/ (:scroll-position-x state)
                                                                                      maximum-x-scroll)
                                                                                   (- requested-width
                                                                                      scroll-bar-length))
                                                                             :y (- requested-height scroll-bar-width)))))))
                                 
                                 (gui/add-mouse-event-handler-with-context view-context
                                                                           (fn [state event]
                                                                             (cond (= (:type event)
                                                                                      :mouse-wheel-moved)
                                                                                   (-> state
                                                                                       (update-in [:scroll-position-x] (fn [position]
                                                                                                                         (max 0 (min maximum-x-scroll
                                                                                                                                     (- position
                                                                                                                                        (:x-distance event))))))
                                                                                       (update-in [:scroll-position-y] (fn [position]
                                                                                                                         (max 0 (min maximum-y-scroll
                                                                                                                                     (- position
                                                                                                                                        (:y-distance event)))))))

                                                                                   (= (:type event)
                                                                                      :mouse-enter)
                                                                                   (do #_(println "mouse-enter")
                                                                                       (assoc state :mouse-over true))

                                                                                   (= (:type event)
                                                                                      :mouse-leave)
                                                                                   (do #_(println "mouse leave")
                                                                                       (assoc state :mouse-over false))

                                                                                   :default state)))))
          this (assoc this :children [child-layoutable])
          [application-state this] (gui/resolve-size-dependent-view-calls view-context application-state this)
          child-layoutable (first (:children this))
          [application-state child-layout] (layout/set-dimensions-and-layout child-layoutable
                                                                             application-state
                                                                             0
                                                                             0
                                                                             requested-width
                                                                             requested-height)]
      [application-state
       (assoc this :children [child-layout])]))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    (layoutable/preferred-size (first children)
                               available-width available-height)))



(defn scroll-panel-view [view-context state]
  (assoc (->ScrollPanel view-context state [(:content state)])
         :size-dependent? true))

(defn scroll-panel [view-context]
  {:local-state {:scroll-position-x 0
                 :scroll-position-y 0}
   :view #'scroll-panel-view})


(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" 15)
                    color)))



(defn barless-root-view [view-context state]
  #_(l/vertically (for [i (range 5)]
                    (gui/call-and-bind view-context state i :text controls/text-editor i)))

  (l/margin 50 50 50 50 (gui/call-view scroll-panel
                                         :scroll-panel-1
                                         {:content
                                          #_(l/vertically (for [i (range 40)]
                                                            (text (str "as fasf asdf asf asdf asdf ads faas fas fasdf" i))))
                                          (l/vertically (for [i (range (:editor-count state))]
                                                          (gui/call-and-bind view-context state i :text controls/text-editor i)))}))

  


  #_(l/box 10 (drawable/->Rectangle 10 10 [255 0 0 255])
         (l/vertically (text "as fasf asdf asf asdf asdf ads faas fas fasdf")
                       (-> (text "as fasf asdf asf asdf asdf ads faas fas fasdf")
                           (assoc :transformer (assoc clip
                                                      :id :transformer-1)))))
  

  #_(l/horizontally (gui/call-view scroll-panel
                                   :scroll-panel-1
                                   {:content (l/vertically (for [i (range 20)]
                                                             (controls/text (str "as fasf asdf asf asdf asdf ads faas fas fasdf" i))))
                                    #_(l/vertically (for [i (range 5)]
                                                      (gui/call-and-bind view-context state i :text controls/text-editor i)))})

                    (gui/call-view scroll-panel
                                   :scroll-panel-2
                                   {:content (l/vertically (for [i (range 20)]
                                                             (controls/text (str "as fasf asdf asf asdf asdf ads faas fas fasdf" i))))
                                    #_(l/vertically (for [i (range 5)]
                                                      (gui/call-and-bind view-context state i :text controls/text-editor i)))})))

(defn barless-root [view-context]
  {:local-state {0 "foobar"
                 :editor-count 5}
   :handle-keyboard-event (fn [state event]
                            (cond
                              (events/key-pressed? event :enter)
                              (do (println "dec")
                                  (gui/apply-to-local-state state view-context update-in [:editor-count] dec))

                              :default
                              state))
   :view #'barless-root-view})


(defn start []
  #_(.start (Thread. (fn []
                       (trace/untrace-ns 'flow-gl.gui.gui)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus-if-can-gain-focus)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-size-dependent-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))
  
  (.start (Thread. (fn []
                     (gui/start-control barless-root))))

  #_(profiler/with-profiler (gui/start-control barless-root)))


