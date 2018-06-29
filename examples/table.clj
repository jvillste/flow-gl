(ns examples.table
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [taoensso.timbre :as timbre]
            (fungl [application :as application]
                   [handler :as handler]
                   [cache :as cache]
                   [atom-registry :as atom-registry]
                   [value-registry :as value-registry]
                   [callable :as callable]
                   [layout :as layout]
                   [layouts :as layouts])
            [flow-gl.profiling :as profiling]
            [flow-gl.tools.trace :as trace]
            (flow-gl.gui 
             [visuals :as visuals]
             [quad-renderer :as quad-renderer]
             [tiled-renderer :as tiled-renderer]
             [animation :as animation]
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
        state)
    
    :default
    state))

(def text-editor-atom-specification
  {:create (fn [] {})})


(handler/def-handler-creator create-text-editor-keyboard-event-handler [state-atom text on-change] [event]
  (trace/log "keyb handler" text)
  (swap! state-atom
         handle-text-editor-keyboard-event
         text
         on-change
         event))

(defn render-text-editor [id text on-change state-atom]
  (let [state (atom-registry/deref! state-atom)]
    (layouts/with-minimum-size 50 0
      (assoc (text-box (if (:has-focus state)
                             [255 255 255 255]
                             [155 155 155 255])
                           (or text
                               ""))
                 :id id
                 :mouse-event-handler keyboard/set-focus-on-mouse-clicked!
                 :keyboard-event-handler (create-text-editor-keyboard-event-handler state-atom text on-change))
      #_(-> 
          (keyboard/update-nodes-event-handler! #_(value-registry/get! [:keyboard-event-handler id text on-change]
                                                                       {:create (fn [] (fn [event]
                                                                                         (swap! state-atom
                                                                                                handle-text-editor-keyboard-event
                                                                                                text
                                                                                                on-change
                                                                                                event)))})
                                                (create-text-editor-keyboard-event-handler state-atom text on-change))))))

(defn text-editor [id text on-change]
  (cache/call! render-text-editor
               id
               text
               on-change
               (atom-registry/get! id text-editor-atom-specification)))

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
(defn render-editor-table [state-atom]
  (layouts/vertically (table (for [x (range 10)]
                               (for [y (range 20)]
                                 #_(assoc (visuals/rectangle [255 255 255 255]
                                                             0 0)
                                          :width (+ 10 (int (rand 50)))
                                          :height 20)

                                 (text-editor [:editor x y] 
                                              (or (get (atom-registry/deref! state-atom) [x y])
                                                  "")
                                              
                                              (create-text-change-handler state-atom x y))))) 
                      
                      (visuals/text [255 255 255 255]
                                    font
                                    (prn-str @state-atom))))


(comment
  
  (with-bindings (application/create-event-handling-state)
    (do ;;taoensso.timbre.profiling/profile :info :create-scene-graph
      (let [state-atom (atom {})
            scene-graph (time (cache/call! editor-table :editor))]
        
        (time (application/do-layout scene-graph
                                     100 100))
        (time (application/do-layout scene-graph
                                     100 100))
        #_(application/do-layout (cache/call! editor-table :editor)
                                 100 100)
        
        #_(render-editor-table state-atom)
        #_(render-editor-table state-atom)
        #_(render-editor-table state-atom))
      #_(application/do-layout (text-box [255 255 255 255]
                                         "haa")
                               100 100)
      #_(application/do-layout (text-box [255 255 255 255]
                                         "haa")
                               100 100))
    nil)

  (with-bindings (application/create-event-handling-state)
    (let [state-atom (atom {})]
      (nth (clojure.data/diff (render-editor-table state-atom)
                              (render-editor-table state-atom))
           0)))

  (with-bindings (application/create-event-handling-state)
    (let [state-atom (atom {})]
      (= (render-editor-table state-atom)
         (render-editor-table state-atom))))


  (with-bindings (application/create-event-handling-state)
    (= (value-registry/get! :keyboard-event-handler
                            {:create (fn [] (fn []))})
       (value-registry/get! :keyboard-event-handler
                            {:create (fn [] (fn []))})))

  (with-bindings (application/create-event-handling-state)
    (let [state-atom (atom {})]
      (=  (application/do-layout (cache/call! editor-table :editor)
                                 100 100)
          (application/do-layout (cache/call! editor-table :editor)
                                 100 100)))))




(defn editor-table [id]
  (cache/call! render-editor-table
               (atom-registry/get! id {:create (fn [] {[0 0] "foo"})})))

(defn create-scene-graph [width height]
  (do ;;taoensso.timbre.profiling/profile :info :create-scene-graph
    #_(trace/log "create-scene-graph")
    #_(animation/swap-state! animation/set-wake-up 1000) ;; TODO: remove this
    (assoc (do #_time (application/do-layout ( #_cache/call! editor-table :editor)
                                        width height))
           :render (fn [scene-graph gl]
                     (opengl/clear gl 0 0 0 1)

                     (do #_time (let [quad-renderer-atom (atom-registry/get! :root-renderer (quad-renderer/atom-specification gl))]
                                  (quad-renderer/render quad-renderer-atom gl scene-graph)))))

    #_(application/do-layout (text-box [255 255 255 255]
                                       "haa")
                             width height)
    )
  )


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

(trace/untrace-ns 'examples.table)
(defn start []
  (trace/untrace-ns 'examples.table)
  (trace/untrace-var 'fungl.cache/call-with-cache)
  (trace/untrace-ns 'fungl.layouts)
  (trace/untrace-ns 'fungl.layout)
  
  #_(trace/untrace-var 'fungl/do-layout)

  (profiling/unprofile-ns 'fungl.layouts)
  (profiling/unprofile-ns 'fungl.layouts)
  (profiling/unprofile-ns 'fungl.layout)
  (profiling/unprofile-ns 'examples.table)
  (profiling/profile-ns 'fungl.application)
  
  #_(do (spec-test/instrument)
        (spec/check-asserts true))
  
  (do (spec-test/unstrument)
      (spec/check-asserts false))

  (do #_trace/with-trace
    (application/start-window (fn [width height]
                                (trace/trace-var #'create-scene-graph)
                                (#'create-scene-graph width height)) 
                              :target-frame-rate 30)))
