(ns examples.devtools
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [drawable :as drawable]
                         [layouts :as layouts]
                         [layoutable :as layoutable])
            (flow-gl.graphics [font :as font])
            [clojure.string :as string]
            [clojure.java.io :as io]
            [flow-gl.utils :as utils]

            [flow-gl.gui.layout-dsl :as l]))

(def files (atom {}))

(defn  read-file [file-name]
  (when (not (contains? @files file-name))
    (swap! files assoc file-name (slurp file-name)))
  (get @files file-name))

(defn ^:trace code-view [view-context state]
  (if-let [text (get (:files state) (:file state))]
    (gui/call-view controls/scroll-panel
                   :code-scroll-panel
                   {:content (l/vertically (for [[index line]  (utils/indexed (string/split-lines text))]
                                             (controls/text line (if (= index (dec (:line state)))
                                                                   [255 255 55 255]
                                                                   [255 255 255 255])
                                                            (font/create "LiberationMono-Regular.ttf" 15))))
                    :scroll-position-y (- (* (:line state)
                                             (:height (layoutable/preferred-size (controls/text "X" [255 255 255 255]
                                                                                                (font/create "LiberationMono-Regular.ttf" 15))
                                                                                 java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE)))
                                          100)})

    (when (:file state)
      (if (.exists (io/as-file (:file state)))
        (do (gui/send-local-state-transformation view-context
                                                 update-in [:files] assoc (:file state) (slurp (:file state)))
            
            (controls/text "loading..."))
        (controls/text "File not found: " (:file state))))))

(defn code [view-context]
  {:local-state {:files {}}
   :view #'code-view})

(defn button [view-context text-value handler]
  (layouts/->Box 10 [(->  (drawable/->Rectangle 0
                                                0
                                                [130 130 230 255])
                          (gui/on-mouse-clicked-with-view-context view-context
                                                                  (fn [state event]
                                                                    (handler state))))
                     (l/center
                      (controls/text text-value
                                     [0 0 0 255]))]))

(defn counter-view [view-context state]
  (l/vertically
   (l/preferred
    (l/vertically
     (l/center
      (l/margin 10 10 10 10
                (l/horizontally (controls/text (:count state))
                                (when (even? (:count state))
                                  (l/margin 0 0 0 10
                                            (controls/text "Even!"
                                                           [0 255 0 255]))))))
     (gui/call-and-bind view-context
                        state
                        :message
                        :text
                        controls/text-editor
                        :editor)
     
     (button view-context
             "Add one"
             (fn [state]
               (update-in state [:count] inc)))))
   
   (controls/text state)))

(defn counter [view-context]
  {:local-state {:count 1
                 :message ""}
   :view #'counter-view})

(defn devtool-view [view-context state]
  (trace/log (:selected-layoutable state))
  (l/horizontally
   
   (l/superimpose (-> (gui/call-view counter :view)
                      (gui/on-mouse-clicked (fn [global-state event]
                                              (gui/apply-to-local-state global-state view-context
                                                                        (fn [local-state]
                                                                          (if (:selecting-layoutable local-state)
                                                                            (let [layoutables (->> (gui/layout-paths-to-layoutables (:layout-paths-under-mouse global-state)
                                                                                                                                    (:layout global-state))
                                                                                                   (filter #(satisfies? layoutable/Layoutable %)))]
                                                                              (assoc local-state
                                                                                     :layoutables layoutables
                                                                                     :selected-layoutable (first layoutables)))
                                                                            
                                                                            local-state))))))
                  (when-let [selected-layoutable (:selected-layoutable state)]
                    (l/absolute (assoc (drawable/->Rectangle (:width selected-layoutable) (:height selected-layoutable) [255 0 0 100])
                                       :x (:global-x selected-layoutable)
                                       :y (:global-y selected-layoutable)))))

   (drawable/->Rectangle 10 0 [255 255 255 255])
   
   (l/vertically (l/margin 2 2 2 2
                           (l/horizontally (controls/check-box state view-context :selecting-layoutable)
                                           (controls/text "select element")))
                 (for [layoutable (:layoutables state)]
                   (-> (controls/text (str (layoutable/layoutable-name layoutable) 
                                           " "
                                           (if (:file (meta layoutable))
                                             "(S)"
                                             ""))
                                      (if (= (:selected-layoutable state)
                                             layoutable)
                                        [255 255 100 255]
                                        [255 255 255 255]))
                       (gui/on-mouse-clicked-with-view-context view-context
                                                               (fn [local-state event]
                                                                 (assoc local-state :selected-layoutable layoutable)))))
                 (let [{:keys [file column line]} (meta (:selected-layoutable state))]
                   (when file
                     (gui/call-view code :code-view {:file file :column column :line line}))))))

(defn devtool [view-context]
  {:local-state {}
   :view #'devtool-view})

(defonce event-channel (atom nil))

(trace/trace-some-from-ns 'examples.devtools)
#_(trace/trace-ns 'flow-gl.gui.layouts)
#_(trace/trace-var 'flow-gl.gui.gui/render-drawables-afterwards)
#_(trace/trace-var 'flow-gl.gui.gui/apply-global-state-handler)
#_(trace/untrace-var 'flow-gl.gui.controls/button)

(defn start []

  (reset! event-channel
          #_(gui/start-control devtool)
          
          (trace/with-trace
            (gui/start-control devtool))))


(when @event-channel
  (gui/redraw-app @event-channel))


;; tommi laitila, erkki pullinainen, andrei modeiros
;; cycle.js, rx.js, bacon.js

;; austin browser repl
;; phantom.js
;; reconsiler

;; reframe, reagent, realy
;; flux pattern
;; - storeissa sovelluksen tila
;; - actionit puhtaita funktioita



