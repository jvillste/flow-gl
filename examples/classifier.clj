(ns examples.classifier
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            [flow-gl.opengl.jogl.quad :as quad]
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font])
            [clojure.string :as string])
  (:import [java.io File])
  (:use flow-gl.utils
        clojure.test))



(defn text [value]
  (layouts/->Margin 2 2 0 0
                    [(drawable/->Text (str value)
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      [255 255 255 255])]))


(defn scroll-panel-view [view-context state]
  (transformer/with-transformers
    (transformer/->Filter :fade1
                          quad/alpha-fragment-shader-source
                          [:1f "alpha" 1])
    (layouts/->SizeDependent (fn [child requested-width requested-height]
                               (let [{preferred-width :width preferred-height :height} (layoutable/preferred-size child requested-width requested-height)
                                     maximum-x-scroll (- preferred-width requested-width)
                                     maximum-y-scroll (- preferred-height requested-height)
                                     scroll-bar-width 5
                                     scroll-bar-color [255 255 255 120]]
                                 (-> (l/superimpose (layouts/->Margin (- (:scroll-position-y state)) 0 0 (- (:scroll-position-x state))
                                                                      [(l/preferred child)])
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
                                                                                       (do (println "mouse-enter")
                                                                                           (assoc state :mouse-over true))

                                                                                       (= (:type event)
                                                                                          :mouse-leave)
                                                                                       (do (println "mouse leave")
                                                                                           (assoc state :mouse-over false))

                                                                                       :default state))))))
                             [(:content state)])))

(defn scroll-panel [view-context]
  {:local-state {:scroll-position-x 0
                 :scroll-position-y 0}
   :view #'scroll-panel-view})

(defn fix-file-name [file-name]
  (-> file-name
      (string/replace "Ñ" "ä")
      (string/replace "î" "ö")
      (string/replace "Ü" "å")
      (string/replace "è" "Å")
      (string/replace "ô" "Ö")))

(defn barless-root-view [view-context state]
  (gui/call-view scroll-panel
                 :scroll-panel-1
                 {:content (l/vertically (for [file (.listFiles (File. "/Users/jukka/src/publisher/files"))]
                                           (let [name (.getName file)]
                                             (l/horizontally (-> (controls/text " Koulu " (if (= :koulu (get (:classes state) name))
                                                                                              [255 255 255 255]
                                                                                              [155 155 155 255]))
                                                                 (gui/on-mouse-clicked-with-view-context view-context (fn [state event]
                                                                                                                        (assoc-in state [:classes name] :koulu))))
                                                             
                                                             (-> (controls/text " Päiväkoti " (if (= :päiväkoti (get (:classes state) name))
                                                                                              [255 255 255 255]
                                                                                              [155 155 155 255]))
                                                                 (gui/on-mouse-clicked-with-view-context view-context (fn [state event]
                                                                                                                        (assoc-in state [:classes name] :päiväkoti))))

                                                             (-> (controls/text " Muu " (if (= nil (get (:classes state) name))
                                                                                              [255 255 255 255]
                                                                                              [155 155 155 255]))
                                                                 (gui/on-mouse-clicked-with-view-context view-context (fn [state event]
                                                                                                                        (let [state (update-in state [:classes] dissoc name)]
                                                                                                                          (spit "classes.clj" (:classes state))
                                                                                                                          state))))
                                                             
                                                             (controls/text (fix-file-name name))))))}))

(defn barless-root [view-context]
  {:local-state {:classes (read-string (slurp "classes.clj"))}
   :view #'barless-root-view})

#_(flow-gl.debug/set-active-channels :all)

(defn start []
  #_(.start (Thread. (fn []
                       (trace/trace-ns 'flow-gl.gui.gui)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))

  (.start (Thread. (fn []
                     (gui/start-control barless-root))))

  #_(profiler/with-profiler (gui/start-control barless-root)))



#_(run-tests)

;; flow-gl.debug/debug-timed
