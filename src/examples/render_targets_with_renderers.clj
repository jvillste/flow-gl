(ns examples.render-targets-with-renderers
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [renderer :as renderer]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 2]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn start-view [drawables-for-time]
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)]


    (try
      (let [renderers-atom (atom (window/with-gl window gl
                                   [(renderer/create-render-target-renderer [(renderer/create-quad-view-renderer gl)
                                                                             (renderer/create-nanovg-renderer)]

                                                                            gl)]
                                   #_[(renderer/create-nanovg-renderer)
                                      (renderer/create-quad-view-renderer gl)]))]
        (loop []
          (let [frame-started (System/currentTimeMillis)]
            (let [drawables (drawables-for-time frame-started)]
              (window/set-display window gl
                                  (opengl/clear gl 0 0 0 1)
                                  (reset! renderers-atom
                                          (renderer/render-frame drawables gl @renderers-atom))))

            (when (window/visible? window)
              (do (wait-for-next-frame frame-started)
                  (recur))))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(def image (buffered-image/create-from-file "pumpkin.png"))

(defn set-size [drawable]
  (let [preferred-size (layoutable/preferred-size drawable 1000 1000)]
    (assoc drawable
      :width (:width preferred-size)
      :height (:height preferred-size))))

(defn text [text]
  (set-size (drawable/->Text text
                             (font/create "LiberationSans-Regular.ttf" 54)
                             [1 1 1 1])))

(defn drawables-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]

    (gui/drawables-for-layout (let [[state layout] (layout/layout (assoc (layouts/->VerticalStack [(assoc (text "foo 1")
                                                                                                     :blur 0.4)
                                                                                                   (text "foo 2")])
                                                                    :render-target? true
                                                                    :key :root
                                                                    :constructor (fn [gl]
                                                                                   [(renderer/create-quad-view-renderer gl)
                                                                                    (renderer/create-nanovg-renderer)])
                                                                    :destructor (fn [state gl]
                                                                                  (doall map renderer/delete state))
                                                                    :render (fn [drawables state width height gl]
                                                                              (renderer/render-frame drawables gl state))
                                                                    :x 10
                                                                    :y 10
                                                                    :width 200
                                                                    :height 100)
                                                                  {}
                                                                  200 200)]
                                layout))
    #_[{:render-target? true
        :x 100
        :y 100
        :width 100
        :height 100
        :child-drawables [(assoc (text "foo 3")
                            :x (- (* 100 phase) 50)
                            :y 0)

                          #_{:render-target? true
                             :width 200
                             :height 200
                             :child-drawables [(assoc (text "foo 2")
                                                 :x (* 100 phase)
                                                 :y 0)]}]}

       #_(assoc (set-size (drawable/->Image image))
           :x (* 100 phase)
           :y 40)

       #_(assoc (drawable/->Rectangle 10 100 #_[1 0 0 1] [255 0 0 255])
           :x (* 100 phase)
           :y 10)]))

(defn start []
  (start-view drawables-for-time))

(run-tests)

#_( render drawables to multiple textures with filtering and transposing)



(println (gui/drawables-for-layout (let [[state layout] (layout/layout (assoc (layouts/->VerticalStack
                                                                               [(assoc (layouts/->HorizontalStack [(text "foo 1")
                                                                                                                   (text "foo 2")])
                                                                                  :render-target? true
                                                                                  :key :texts
                                                                                  :constructor (fn [gl]
                                                                                                 {:renderers [(renderer/create-quad-view-renderer gl)
                                                                                                              (renderer/create-nanovg-renderer)]})
                                                                                  :destructor (fn [state gl]
                                                                                                (doall map renderer/delete (:renderers state))
                                                                                                state)
                                                                                  :render (fn [drawables state width height gl]
                                                                                            (let [render-target (render-target/create width
                                                                                                                                      height
                                                                                                                                      gl)]

                                                                                              (let [renderers (render-target/render-to render-target gl
                                                                                                                                       (opengl/clear gl 0 0 0 1)
                                                                                                                                       (render-frame-drawables drawables
                                                                                                                                                               gl
                                                                                                                                                               renderers))]
                                                                                                (let [{:keys [width height]} (opengl/size gl)]
                                                                                                  (render-target/draw render-target (:x render-target-drawable) (:y render-target-drawable) width height gl))

                                                                                                (render-target/delete render-target gl)
                                                                                                renderers))
                                                                                            (renderer/render-frame drawables gl state)))
                                                                                (text "foo 3")])
                                                                         :render-target? true
                                                                         :key :root
                                                                         :constructor (fn [gl]
                                                                                        {:renderers [(renderer/create-quad-view-renderer gl)
                                                                                                     (renderer/create-nanovg-renderer)]})
                                                                         :destructor (fn [state gl]
                                                                                       (doall map renderer/delete (:renderers state))
                                                                                       state)
                                                                         :render (fn [state drawables width height gl]
                                                                                   [(assoc state :renderers (renderer/render-frame drawables
                                                                                                                                   gl
                                                                                                                                   (:renderers state)))
                                                                                    nil])
                                                                         :x 10
                                                                         :y 10
                                                                         :width 200
                                                                         :height 100)
                                                                       {}
                                                                       200 200)]
                                     layout)))



(defn render [render-target-state render-target-drawable gl]
  (let [[render-target-state render-target-drawables] (reduce (fn [[render-target-state render-target-drawables] drawable]
                                                                (if (:render-target? drawable)
                                                                  (let [child-render-target-state (get-in render-target-state [:child-render-target-states (:key drawable)])
                                                                        [child-render-target-state child-render-target-drawable] (render child-render-target-state
                                                                                                                                         (:child-drawables drawable)
                                                                                                                                         gl)]
                                                                    [(assoc-in render-target-state [:child-render-target-states (:key drawable)] child-render-target-state)
                                                                     (conj render-target-drawables child-render-target-drawable)])
                                                                  [render-target-state
                                                                   (conj render-target-drawables drawable)]))
                                                              [render-target-state []]
                                                              (:child-drawables render-target-drawable))
        [render-target-state drawable] ((:render render-target-drawable)
                                        render-target-state
                                        render-target-drawables
                                        (:width render-target-drawable)
                                        (:height render-target-drawable)
                                        gl)])
  )
