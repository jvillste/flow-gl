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
  (:use flow-gl.utils
        clojure.test))

(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" 35)
                    color)))



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

(defn scroll-panel-view [view-context state]
  
  (layouts/->SizeDependent view-context
                           (fn [available-width available-height]
                             {:width available-width
                              :height available-height}
                             #_(layoutable/preferred-size (:content state)
                                                          available-width
                                                          available-height))
                           
                           (fn [requested-width requested-height]
                             (let [{preferred-width :width preferred-height :height} {:width 100 :height 200} #_(layoutable/preferred-size (:content state)
                                                                                                                                           requested-width
                                                                                                                                           requested-height)
                                   maximum-x-scroll (- preferred-width requested-width)
                                   maximum-y-scroll (- preferred-height requested-height)
                                   scroll-bar-width 5
                                   scroll-bar-color [255 255 255 120]]
                               (-> (l/superimpose (-> (layouts/->Margin (- (:scroll-position-y state)) 0 0 (- (:scroll-position-x state))
                                                                        [(l/preferred (:content state))])
                                                      (assoc :transformer (assoc (bloom (/ (:scroll-position-y state) 100))
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

                                                                                     :default state))))))))

(defn scroll-panel [view-context]
  {:local-state {:scroll-position-x 0
                 :scroll-position-y 0}
   :view #'scroll-panel-view})

(defn barless-root-view [view-context state]
  (l/vertically (for [i (range 5)]
                  (gui/call-and-bind view-context state i :text controls/text-editor i)))

  #_(l/margin 50 50 50 50 (gui/call-view scroll-panel
                                       :scroll-panel-1
                                       {:content
                                        #_(l/vertically (for [i (range 40)]
                                                          (text (str "as fasf asdf asf asdf asdf ads faas fas fasdf" i))))
                                        (l/vertically (for [i (range 5)]
                                                        (gui/call-and-bind view-context state i :text controls/text-editor i)))}))
  

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
  {:local-state {0 "foobar"}
   :view #'barless-root-view})


(defn start []
  #_(.start (Thread. (fn []
                       (trace/trace-ns 'flow-gl.gui.gui)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))

  (.start (Thread. (fn []
                     (gui/start-control barless-root))))

  #_(profiler/with-profiler (gui/start-control barless-root)))


