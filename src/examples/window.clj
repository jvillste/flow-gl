(ns flow-gl.examples.window
  (:require [clojure.core.async :as async]
            [flow-gl.opengl.jogl.window :as jogl-window]
            [flow-gl.opengl.jogl.opengl :as opengl]

            [datomic.api :as d]
            (flow-gl.gui [drawable :as drawable]
                         [renderer :as renderer]
                         [window :as window]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            [flow-gl.debug :as debug])
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable]
           [java.nio ByteBuffer])
  (:use flow-gl.utils
        clojure.test))


(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))

(defn event-loop [initial-state app]
  (loop [state initial-state]

    (if (:close-requested state)
      (window/close (:window state))

      (recur (app state
                  (csp/drain (window/event-channel (:window state))
                             (:sleep-time state)))))))

(defn wrap-with-close-window-on-exception [app]
  (fn [state events]
    (try
      (app state events)
      (catch Exception e
        (window/close (:window state))
        (throw e)))))

(defn wrap-with-separate-events [app]
  (fn [state events]
    (reduce app
            state
            events)))

(defn add-window [state]
  (assoc state :window (jogl-window/create 300
                                           400
                                           :profile :gl3
                                           :init opengl/initialize
                                           :reshape opengl/resize)))

(defn close-when-requested-beforehand [app]
  (fn [state event]
    (if (= (:type event) :close-requested)
      (assoc state :close-requested true)
      (app state event))))

(defn drawables-for-layout
  ([layout]
     (drawables-for-layout layout 0 0 0 []))

  ([layout parent-x parent-y parent-z drawables]
     (if (:children layout)
       (let [parent-x (+ parent-x (:x layout))
             parent-y (+ parent-y (:y layout))
             parent-z (+ parent-z (or (:z layout) 0))]
         (loop [drawables drawables
                children (:children layout)]
           (if-let [child (first children)]
             (let [drawables (drawables-for-layout child parent-x parent-y parent-z drawables)]
               (recur drawables
                      (rest children)))
             drawables)))
       (conj drawables
             (assoc layout
               :x (+ parent-x (:x layout))
               :y (+ parent-y (:y layout))
               :z (+ parent-z (or (:z layout) 0)))))))

(defn add-drawables-for-layout-afterwards [app]
  (fn [state events]
    (let [state (app state events)]
      (assoc state :drawables (drawables-for-layout (:layout state))))))

(defn render-drawables-afterwards [app]
  (fn [state events]
    (let [state (if (:renderers state)
                  state
                  (assoc state :renderers (window/with-gl (:window state) gl
                                            [(renderer/create-quad-view-renderer gl)
                                             (renderer/create-nanovg-renderer)])))
          state (app state events)]

      (window/render-constantly (:window state) gl
                                (opengl/clear gl 0 0 0 1)
                                (renderer/render-frame (:drawables state)
                                                       gl
                                                       (:renderers state)))
      state)))



(defn add-layout-afterwards [app]
  (fn [state event]
    (let [state (app state event)
          width (window/width (:window state))
          height (window/height (:window state))
          [state layout] (layout/layout (:layoutable state)
                                        state
                                        width
                                        height)
          layout (-> layout
                     (assoc :x 0
                            :y 0
                            :width width
                            :height height)
                     (layout/add-out-of-layout-hints))]
      (assoc state :layout layout))))


(defn add-layout-paths-under-mouse-beforehand [app]
  (fn [state event]
    (if (and (:layout state)
             (= (:source event)
                :mouse))
      (-> state
          (assoc :layout-paths-under-mouse (reverse (layout/layout-paths-in-coordinates (:layout state) (:x event) (:y event))))
          (app event))
      (app state event))))

(defn apply-layout-event-handlers-beforehand [app]
  (fn [state event]
    (let [state (if (and (:layout state)
                         (= (:source event)
                            :mouse))
                  (gui/apply-layout-event-handlers-3 state (:layout state) (:layout-paths-under-mouse state) :handle-mouse-event-2 event)
                  state)]
      (app state event))))

(defn start-app [app]
  (-> {}
      (add-window)
      (event-loop (-> app
                      (add-layout-afterwards)
                      (apply-layout-event-handlers-beforehand)
                      (add-layout-paths-under-mouse-beforehand)
                      (close-when-requested-beforehand)
                      (wrap-with-separate-events)
                      (wrap-with-close-window-on-exception)
                      (add-drawables-for-layout-afterwards)
                      (render-drawables-afterwards)))))

(defn app [window state events]
  (assoc state
    :drawables [(assoc (drawable/->Rectangle 100 100 [255 255 255 255])
                  :x 0 :y 0)]))

(defn layout-app [state event]
  (let [view-state (or (:view-state state)
                       {:count 0})]
    (assoc state
      :view-state view-state
      :layoutable (l/vertically (drawable/->Rectangle 100 100 [255 255 255 255])
                                (text (:count view-state))
                                (text (str "Layout paths" (:layout-paths-under-mouse state)))
                                (gui/add-mouse-event-handler (drawable/->Rectangle 100 100 [0 255 255 255])
                                                             (fn [state event]
                                                               (update-in state [:view-state :count] inc)))))))

(defn start []
  #_(start-app (-> app
                   close-when-requested-beforehand
                   render-drawables-afterwards))

  (start-app layout-app))






