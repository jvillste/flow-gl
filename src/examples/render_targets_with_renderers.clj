(ns examples.render-targets-with-renderers
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]
                                 [quad :as quad])
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
  (let [target-frames-per-second 1]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))


(defn text [text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 30)
                   [1 1 1 1]))

(defn child-render-target [key layout]
  (assoc layout
    :render-target? true
    :key key
    :constructor (fn [gl]
                   {:renderers [(renderer/create-quad-view-renderer gl)
                                #_(renderer/create-nanovg-renderer)]})
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
                                                         (opengl/clear gl 0 0 0 0)
                                                         (renderer/render-frame-drawables drawables
                                                                                          gl
                                                                                          (:renderers state)))]

                  [(assoc state
                     :renderers renderers
                     :render-target render-target)
                   [(drawable/->Quad ["texture" (:texture render-target)]
                                     []
                                     quad/fragment-shader-source
                                     x
                                     y
                                     width
                                     height)
                    (drawable/->Quad ["texture" (:texture render-target)]
                                     []
                                     quad/fragment-shader-source
                                     x
                                     y
                                     width
                                     height)]])))))

(defn root-render-target [layout]
  (assoc layout
    :render-target? true
    :key :root
    :constructor (fn [gl]
                   {:renderers [(renderer/create-quad-view-renderer gl)
                                (renderer/create-nanovg-renderer)
                                #_(renderer/create-gl-renderer)
                                (renderer/create-quad-renderer gl)]})
    :destructor (fn [state gl]
                  (doall map renderer/delete (:renderers state))
                  state)
    :render (fn [state drawables x y width height gl]
              [(assoc state :renderers (renderer/render-frame drawables
                                                              gl
                                                              (:renderers state)))
               []])
    :x 0
    :y 0
    :width 200
    :height 300))

(defn drawables-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]
    (gui/drawables-for-layout
     (let [[state layout] (layout/layout (assoc (layouts/->VerticalStack
                                                 [(child-render-target :child-1 (layouts/->Margin 0 0 0 0 [(text "child 1")]))
                                                  (child-render-target :child-2 (layouts/->Margin 10 0 0 0 [(text "child 2")]))
                                                  (child-render-target :child-3 (layouts/->Margin 0 0 0 0 [(text "child 3")]))])
                                           :width 200
                                           :height 200
                                           :x 0
                                           :y 0)
                                         {}
                                         200 200)]
       #_(flow-gl.debug/ppreturn layout)
       layout))))


(defn render [render-target-state render-target-drawable gl]

  (let [old-child-render-target-state-keys (->> render-target-state :child-render-target-states keys (apply hash-set))
        new-child-render-target-state-keys (->> render-target-drawable :child-drawables (filter :render-target?) (map :key) (apply hash-set))
        [render-target-state render-target-drawables] (reduce (fn [[render-target-state render-target-drawables] drawable]
                                                                (if (:render-target? drawable)
                                                                  (let [child-state-path [:child-render-target-states (:key drawable)]
                                                                        child-render-target-state (or (get-in render-target-state child-state-path)
                                                                                                      ((:constructor drawable) gl))
                                                                        [child-render-target-state child-render-target-drawables] (render child-render-target-state
                                                                                                                                          drawable
                                                                                                                                          gl)]
                                                                    [(assoc-in render-target-state child-state-path child-render-target-state)
                                                                     (concat render-target-drawables child-render-target-drawables)])
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
        render-target-state-atom (atom (window/with-gl window gl
                                         {:renderers [(renderer/create-quad-view-renderer gl)
                                                      (renderer/create-nanovg-renderer)
                                                      (renderer/create-quad-renderer gl)]}))]

    (try
      (loop []
        (let [frame-started (System/currentTimeMillis)]
          (let [drawables ( drawables-for-time frame-started)]
            ;;(println "drawables" drawables)
            (window/set-display window gl
                                (opengl/clear gl 0 0 0 1)
                                (let [{:keys [width height]} (opengl/size gl)]
                                  (swap! render-target-state-atom
                                         (fn [render-target-state]
                                           (let [[render-target-state render-target-drawables] (render render-target-state
                                                                                                       {:render (fn [state drawables x y width height gl]
                                                                                                                  [(assoc state :renderers (renderer/render-frame drawables
                                                                                                                                                                  gl
                                                                                                                                                                  (:renderers state)))
                                                                                                                   []])
                                                                                                        :child-drawables drawables
                                                                                                        :width width
                                                                                                        :height height
                                                                                                        :x 0
                                                                                                        :y 0}
                                                                                                       gl)]
                                             render-target-state))))))

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
