(ns examples.devtools
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [drawable :as drawable]
                         [layouts :as layouts]
                         [layoutable :as layoutable])
            [clojure.string :as string]
            [clojure.java.io :as io]

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
                   {:content (l/vertically (for [line (string/split-lines text)]
                                             (controls/text line)))})

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

  (l/horizontally
   
   (-> (gui/call-view counter :view)
       (gui/on-mouse-clicked (fn [global-state event]
                               (trace/log "mouse clicked" global-state)
                               (gui/apply-to-local-state global-state view-context
                                                         (fn [local-state]
                                                           (if (:selecting-element local-state)
                                                             (assoc local-state
                                                                    :layouts (->> (gui/layout-paths-to-layoutables (:layout-paths-under-mouse global-state)
                                                                                                                   (:layout global-state))
                                                                                  (filter #(satisfies? layoutable/Layoutable %)))
                                                                    :selecting-element false)
                                                             local-state))))))

   (drawable/->Rectangle 10 0 [255 255 255 255])
   
   (l/vertically (l/preferred (controls/button view-context
                                               "select element"
                                               (:selecting-element state)
                                               (fn [local-state]
                                                 (trace/log "start selecting")
                                                 (assoc local-state :selecting-element true))))
                 (for [layout (:layouts state)]
                   (controls/text  (str (type layout)
                                        " "
                                        (meta layout))))
                 (when-let [{:keys [file column line]} (meta (first (filter meta (:layouts state))))]
                   (gui/call-view code :code-view {:file file :column column :line line})))))

(defn global-state-handler [local-state global-state]
  (if (:selecting-element local-state)
    (if (= (:type (:event global-state))
           :mouse-clicked)
      (assoc local-state
             :layouts (->> (gui/layout-paths-to-layoutables (->> (:layout-paths-under-mouse global-state))
                                                            (:layout global-state))
                           (filter #(satisfies? layoutable/Layoutable %)))
             :selecting-element false)
      local-state)
    local-state))

(defn devtool [view-context]
  {:local-state {}
   ;;   :global-state-handler #'global-state-handler
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



