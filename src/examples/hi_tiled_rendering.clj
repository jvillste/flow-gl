(ns examples.hi-tiled-rendering
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [tiled-renderer :as tiled-renderer]
                         [render-target-renderer :as render-target-renderer]
                         [animation :as animation]
                         [layouts :as layouts])
            (flow-gl.graphics [font :as font])))

(def font (font/create "LiberationSans-Regular.ttf" 15))

(def state (atom {}))

(defn text-box [text]
  (assoc layouts/box
         :margin 5
         :children [(visuals/rectangle [155 155 255 255]
                                       5
                                       5)
                    (visuals/text [0 0 0 255]
                                  font
                                  text)]))



(def table (-> {:children (for [x (range 5)
                                y (range 5)]
                            (assoc (text-box (str x "," y))
                                   :x (* x 40)
                                   :y (* y 40)))}
               (application/do-layout 1000 1000)))

(def text
  (-> (with-open [rdr (clojure.java.io/reader "src/examples/hi_tiled_rendering.clj")]
        (apply layouts/vertically
               (for [line (doall (line-seq rdr))]
                 (visuals/text [255 255 255 255]
                               font
                               line))))
      (application/do-layout 1000 1000)))

(defn scroll-pane [id x-translation y-translation width height content-scene-graph]
  (let [quad-renderer (assoc quad-renderer/renderer
                             :id [id :quad-renderer])]
    
    {:children [(assoc content-scene-graph
                       :x x-translation
                       :y y-translation
                       :renderers [(assoc (let [tile-width width
                                                tile-height height]
                                            (tiled-renderer/renderer [quad-renderer]
                                                                     tile-width
                                                                     tile-height
                                                                     (tiled-renderer/tiles-in-view tile-width tile-height
                                                                                                   (- x-translation) (- y-translation)
                                                                                                   width height)
                                                                     :visualize-tiles true))
                                          :id [id :tiled])])]
     :width width
     :height height
     #_:renderers #_[(assoc (render-target-renderer/renderer quad-renderer)
                            :id [id :render-target])]}))

#_(flow-gl.gui.scene-graph/bounding-box (flow-gl.gui.scene-graph/leave-nodes table))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/start-if-not-running :offset)
  (animation/swap-state! animation/set-wake-up 1000)

  (let [margin 100
        scroll-pane-width (- width (* 2 margin))
        scroll-pane-height (- height (* 2 margin))]
    {:children [(assoc (let [phase (animation/phase! :offset)]
                         (scroll-pane :scroll-pane-1
                                      (- 110
                                         (+ (* 200 (animation/ping-pong 40 phase))
                                            #_(* 50 (animation/ping-pong 6 phase))))
                                      (- 110
                                         (* 800
                                            (animation/ping-pong 20 phase)))
                                      scroll-pane-width
                                      scroll-pane-height
                                      text  #_table))
                       :x margin
                       :y margin
                       :width scroll-pane-width
                       :height scroll-pane-height)
                (assoc (visuals/rectangle [255 0 0 100] 0 0)
                       :x margin
                       :y margin
                       :width scroll-pane-width
                       :height scroll-pane-height)]
     :x 0 :y 0 :width width :height height
     :renderers [(assoc quad-renderer/renderer
                        :id :root)]}))

#_ (with-bindings (application/create-event-handling-state)
     (time (create-scene-graph 100 100)))

(defn start []
 #_ (do (spec-test/instrument)
      (spec/check-asserts true))
  
  (do (spec-test/unstrument)
        (spec/check-asserts false))

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :target-frame-rate 60)))))
