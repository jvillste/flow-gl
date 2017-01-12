(ns examples.table
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [application :as application]
                   [handler :as handler]
                   [cache :as cache]
                   [atom-registry :as atom-registry]
                   [callable :as callable])
            [flow-gl.profiling :as profiling]
            [flow-gl.tools.trace :as trace]
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [tiled-renderer :as tiled-renderer]
                         [animation :as animation]
                         [layouts :as layouts]
                         [scene-graph :as scene-graph]
                         [stateful :as stateful]
                         [keyboard :as keyboard]
                         [events :as events])
            
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [quad :as quad]
                                 [render-target :as render-target])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])))



(def font (font/create "LiberationSans-Regular.ttf" 20))

(defn text-box [color text]
  (layouts/box 5
               (visuals/rectangle color
                                  0
                                  0)
               #_(layouts/with-minimum-size 30 1 (visuals/rectangle color
                                                                    0
                                                                    0)) 
               (visuals/text [0 0 0 255]
                             font
                             text)))

(defn handle-text-editor-keyboard-event [state text on-change event]
  (cond
    (= :focus-gained
       (:type event))
    (assoc state :has-focus true)

    (= :focus-lost
       (:type event))
    (assoc state :has-focus false)
    
    (events/key-pressed? event :back-space)
    (do (on-change (apply str (drop-last text)))
        state)

    (and (:character event)
         (not= (:key event)
               :enter)
         (= (:type event)
            :key-pressed))
    (do (on-change (str text
                        (:character event)))
        state)))

(def text-editor-atom-specification
  {:create (fn [] {})})

(defn render-text-editor [id text on-change state-atom]
  (println "render-text-editor")
  (let [state (atom-registry/deref! state-atom)]
    (layouts/with-minimum-size 50 0
      (-> (assoc (text-box (if (:has-focus state)
                             [255 255 255 255]
                             [155 155 155 255])
                           (or text
                               ""))
                 :id id
                 :mouse-event-handler keyboard/set-focus-on-mouse-clicked!)
          (keyboard/update-nodes-event-handler! (fn [event]
                                                  (swap! state-atom
                                                         handle-text-editor-keyboard-event
                                                         text
                                                         on-change
                                                         event)))))))

(defn text-editor [id text on-change]
  (cache/call! render-text-editor
               id
               text
               on-change
               (atom-registry/get! id text-editor-atom-specification)))

(defonce state-atom (atom {[0 0] "foo"}))

(defn table [rows]
  (let [rows-with-sizes (map (fn [row]
                               (map (fn [node]
                                      (assoc node
                                             :size (layout/size (layout/do-layout node))))
                                    row))
                             rows)
        columns-with-sizes (for [index (range (count (first rows-with-sizes)))]
                             (map (fn [row]
                                    (nth row index))
                                  rows-with-sizes))
        column-widths (for [column columns-with-sizes]
                        (apply max (map (fn [node]
                                          (get-in node [:size :width]))
                                        column)))]
    (apply layouts/vertically (for [row rows]
                                (apply layouts/horizontally (for [[node width] (map vector row column-widths)]
                                                              (layouts/with-margins 1 1 1 1
                                                                (layouts/with-minimum-size width 0 node))))))))


(handler/def-handler-creator create-text-change-handler [state-atom x y] [new-text]
  (swap! state-atom assoc [x y] new-text))

#_(.id handle-text-change-implementation)

#_(defn create-on-text-change [state-atom x y]
    (fn [new-text]
      (swap! state-atom assoc [x y] new-text)))


(defn create-scene-graph [width height]
  (trace/log "create-scene-graph")
  #_(animation/swap-state! animation/set-wake-up 1000) ;; TODO: remove this
  
  (layout/do-layout {:children [(layouts/vertically (table (for [x (range 3)]
                                                             (for [y (range 2)]
                                                               #_(assoc (visuals/rectangle [255 255 255 255]
                                                                                           0 0)
                                                                        :width (+ 10 (int (rand 50)))
                                                                        :height 20)

                                                               (text-editor [:editor x y] 
                                                                            (or (get @state-atom [x y])
                                                                                "")
                                                                            
                                                                            (create-text-change-handler state-atom x y))))) 
                                                    
                                                    (visuals/text [255 255 255 255]
                                                                  font
                                                                  (prn-str @state-atom)))]
                     :available-width width
                     :available-height height
                     :x 10
                     :y 10}))


#_(with-bindings (application/create-event-handling-state)
    (create-scene-graph 100 100))

#_(layout/do-layout (layouts/with-minimum-size 100 100
                      (assoc layouts/box
                             :margin 5
                             :children [(layouts/with-minimum-size 50 50
                                          {:width 10
                                           :height 20})
                                        
                                        {:width 10
                                         :height 10}])))


(defn start []
  (trace/trace-ns 'flow-gl.gui.layouts)
  #_(trace/untrace-var 'flow-gl.gui.layout/do-layout)

  #_(profiling/unprofile-ns 'flow-gl.gui.layouts)
  (do (spec-test/instrument)
      (spec/check-asserts true))
  
  #_(do (spec-test/unstrument)
        (spec/check-asserts false))

  (do #_trace/with-trace
      (application/start-window (fn [width height]
                                  (trace/trace-var #'create-scene-graph)
                                  (#'create-scene-graph width height)) 
                                :target-frame-rate 30)))
