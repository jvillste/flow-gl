(ns flow-gl.tools.layoutable-inspector
  (:require (flow-gl.gui [event-queue :as event-queue]
                         [layout :as layout]
                         [drawable :as drawable]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer])
            [flow-gl.opengl.math :as math]
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]
                              [native-buffer :as native-buffer]))

  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color])

  (:use clojure.test))


(defn show-layoutable [layoutable]
  (let [width (min (layoutable/preferred-width layoutable)
                   2000)
        height (min (layoutable/preferred-height layoutable)
                    2000)
        window (window/create width height :profile :gl3)
        layout (-> layoutable
                   (layout/layout width height)
                   (assoc :x 0 :y 0))]

    (try
      (window/with-gl window gl
        (opengl/initialize gl)
        (-> (quad-view/create gl)
            (quad-view/draw-layout layout
                                   width
                                   height
                                   gl)))


      (catch Exception e
        (window/close window)
        (throw e)))))


(defn text [content & {:keys [color size] :or {color [1 1 1 1] size 14}}]
  (drawable/->Text content
                   (font/create "LiberationSans-Regular.ttf" size)
                   color))

(defn layoutable-name [layoutable]
  (-> (clojure.reflect/typename (type layoutable))
      (clojure.string/replace  "flow_gl.gui.layout." "")
      (clojure.string/replace  "flow_gl.gui.drawable." "")))

(defn draw-layoutable [layoutable]
  (layout/->VerticalStack [(if (:state-path-part layoutable)
                             (text (str (:state-path-part layoutable)) :size 12 :color [0.6 0.6 0.6 1.0])
                             (drawable/->Empty 0 0))
                           (text (layoutable-name layoutable))
                           (layout/->Margin 0 0 0 10 [(layout/->VerticalStack (map draw-layoutable (:children layoutable)))])]))

(defn inspect-layoutable [layoutable]
  (show-layoutable (draw-layoutable layoutable)))

