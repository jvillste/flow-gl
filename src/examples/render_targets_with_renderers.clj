(ns examples.render-targets-with-renderers
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target])
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

(defn text [text]
  (set-size (drawable/->Text text
                             (font/create "LiberationSans-Regular.ttf" 30)
                             [1 1 1 1])))

(defn render-target-drawable-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]
    (first (gui/drawables-for-layout
            (let [[state layout] (layout/layout (assoc (layouts/->VerticalStack
                                                        [(assoc (layouts/->HorizontalStack [(text "foo 1")
                                                                                            (text "foo 2")])
                                                           :render-target? true
                                                           :key :texts
                                                           :constructor (fn [gl]
                                                                          {:renderers [(renderer/create-quad-view-renderer gl)
                                                                                       (renderer/create-nanovg-renderer)]})
                                                           :destructor (fn [state gl]
                                                                         (dorun (map renderer/delete (:renderers state)))
                                                                         (render-target/delete (:render-target state) gl))
                                                           :render (fn [state drawables x y width height gl]
                                                                     (let [old-render-target (:render-target state)
                                                                           render-target (if (and old-render-target
                                                                                                  (= (:width old-render-target)
                                                                                                     width)
                                                                                                  (= (:height old-render-target)
                                                                                                     height))
                                                                                           old-render-target
                                                                                           (render-target/create width
                                                                                                                 height
                                                                                                                 gl))]

                                                                       (when (and old-render-target
                                                                                  (not= old-render-target render-target))
                                                                         (render-target/delete old-render-target gl))

                                                                       (let [renderers (render-target/render-to render-target gl
                                                                                                                (opengl/clear gl 0 0 0 1)
                                                                                                                (renderer/render-frame-drawables drawables
                                                                                                                                                 gl
                                                                                                                                                 (:renderers state)))]

                                                                         [(assoc state
                                                                            :renderers renderers
                                                                            :render-target render-target)
                                                                          (drawable/->Quad (:texture render-target)
                                                                                           x
                                                                                           y
                                                                                           width
                                                                                           height)]))))
                                                         (text "foo 3")])
                                                  :render-target? true
                                                  :key :root
                                                  :constructor (fn [gl]
                                                                 {:renderers [(renderer/create-quad-view-renderer gl)
                                                                              (renderer/create-nanovg-renderer)
                                                                              (renderer/create-gl-renderer)]})
                                                  :destructor (fn [state gl]
                                                                (doall map renderer/delete (:renderers state))
                                                                state)
                                                  :render (fn [state drawables x y width height gl]
                                                            (println "rendering " drawables)
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
              layout)))))


(defn render [render-target-state render-target-drawable gl]

  (let [old-child-render-target-state-keys (->> render-target-state :child-render-target-states keys (apply hash-set))
        new-child-render-target-state-keys (->> render-target-drawable :child-drawables (filter :render-target?) (map :key) (apply hash-set))
        [render-target-state render-target-drawables] (reduce (fn [[render-target-state render-target-drawables] drawable]
                                                                (if (:render-target? drawable)
                                                                  (let [child-state-path [:child-render-target-states (:key drawable)]
                                                                        child-render-target-state (or (get-in render-target-state child-state-path)
                                                                                                      ((:constructor drawable) gl))
                                                                        [child-render-target-state child-render-target-drawable] (render child-render-target-state
                                                                                                                                         drawable
                                                                                                                                         gl)]
                                                                    [(assoc-in render-target-state child-state-path child-render-target-state)
                                                                     (conj render-target-drawables child-render-target-drawable)])
                                                                  [render-target-state
                                                                   (conj render-target-drawables drawable)]))
                                                              [render-target-state []]
                                                              (:child-drawables render-target-drawable))]



    (dorun (map (fn [child-render-target-key]
                  (let [child-render-target-state (get-in render-target-state [:child-render-target-states child-render-target-key])]
                    ((:destructor child-render-target-state)
                     child-render-target-state
                     gl)))
                (filter (complement new-child-render-target-state-keys)
                        old-child-render-target-state-keys)))

    ((:render render-target-drawable)
     render-target-state
     render-target-drawables
     (:x render-target-drawable)
     (:y render-target-drawable)
     (:width render-target-drawable)
     (:height render-target-drawable)
     gl)))

(defn start-view []
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)
        render-target-state-atom (atom nil)]

    (try
      (loop []
        (let [frame-started (System/currentTimeMillis)]
          (let [render-target-drawable (render-target-drawable-for-time frame-started)]
            (window/set-display window gl
                                (opengl/clear gl 0 0 0 1)
                                (swap! render-target-state-atom
                                       (fn [render-target-state]
                                         (let [[render-target-state render-target-drawable] (-> (or render-target-state
                                                                                                    ((:constructor render-target-drawable) gl))
                                                                                                (render render-target-drawable gl))]
                                           render-target-state)))))

          (when (window/visible? window)
            (do (wait-for-next-frame frame-started)
                (recur)))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view))

(run-tests)

#_( render drawables to multiple textures with filtering and transposing)



(println )
