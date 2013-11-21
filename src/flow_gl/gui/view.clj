(ns flow-gl.gui.view
  (:require  (flow-gl.graphics [command :as command])
             (flow-gl.graphics.command [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview]
                                       [translate :as translate])
             (flow-gl.gui [layout :as layout]
                          [layoutable :as layoutable]
                          [drawable :as drawable]
                          [input :as input]
                          [mouse :as mouse])
             (flow-gl.opengl [window :as window])
             (flow-gl [opengl :as opengl]
                      [debug :as debug])
             [flow-gl.dataflow.dataflow :as dataflow]
             [flow-gl.dataflow.triple-dataflow :as triple-dataflow]
             [clojure.data.priority-map :as priority-map])

  (:use clojure.test
        flow-gl.threading))

;; DEBUG

(defn describe-gpu-state [gpu-state]
  (for [key (keys (:view-part-command-runners gpu-state))]
    (str key " = " (vec (get-in gpu-state [:view-part-command-runners key])))))


;; VIEW PARTS

(defrecord ViewPartCall [view-part-layout-path layer]
  command/Command
  (create-runner [view-part-call] view-part-call)
  command/CommandRunner
  (delete [view-part-call])
  (run [view-part-call]))

(defn element-path-to-layout-path [element-path]
  (vec (concat [:layout] (rest element-path))))

(defrecord ViewPart [local-id view-id]
  drawable/Drawable
  (drawing-commands [view-part]
    [(->ViewPartCall (element-path-to-layout-path root-element-path) 0)])

  layoutable/Layoutable
  (preferred-width [view-part] (dataflow/property root-element-path [:preferred-width]))
  (preferred-height [view-part] (dataflow/property root-element-path [:preferred-height]))

  layout/Layout
  (layout [view-part requested-width requested-height]
    (dataflow/initialize (:local-id view-part)
                         #(layout/set-dimensions-and-layout (dataflow/get-global-value (:root-element-path view-part))
                                                            0
                                                            0
                                                            (:global-x view-part)
                                                            (:global-y view-part)
                                                            requested-width
                                                            requested-height))
    view-part)

  (children-in-coordinates [this view-state x y]
    (let [layout (get view-state (element-path-to-layout-path (:root-element-path this)))]
      (concat [layout] (layout/children-in-coordinates layout view-state x y))))

  Object
  (toString [_] (str "(->ViewPart " view-id)))


(defn loaded-view-parts [gpu-state]
  (keys (:view-part-command-runners gpu-state)))

(defn view-part-is-loaded? [gpu-state view-part-layout-path]
  (contains? (:view-part-command-runners gpu-state) view-part-layout-path))

(defn unload-view-part [gpu-state layout-path]
  (dorun (map command/delete (get-in gpu-state [:view-part-command-runners layout-path])))
  (update-in gpu-state [:view-part-command-runners] dissoc layout-path))

(defn load-view-part [gpu-state view-state layout-path]
  (unload-view-part gpu-state layout-path)

  (debug/do-debug :view-update "loading " layout-path)

  (if (dataflow/is-defined? view-state layout-path)
    (let [drawing-commands (if-let [layout (get view-state layout-path)]
                             (drawable/drawing-commands layout)
                             [])
          gpu-state (reduce (fn [gpu-state layout-path]
                              (load-view-part gpu-state view-state layout-path))
                            gpu-state
                            (->> (filter #(and (instance? ViewPartCall %)
                                               (not (view-part-is-loaded? gpu-state (:view-part-layout-path %))))
                                         drawing-commands)
                                 (map :view-part-layout-path)))]

      (assoc-in gpu-state [:view-part-command-runners layout-path] (command/command-runners-for-commands drawing-commands)))
    gpu-state))

(defn update-view-part [gpu-state view-state layout-path]
  (if (dataflow/is-defined? view-state layout-path)
    (load-view-part gpu-state view-state layout-path)
    (unload-view-part gpu-state layout-path)))


(defn init-and-call [parent-view identifiers view & parameters]
  (let [key (keyword (str "child-view-" identifiers))
        child-view-id (if (contains? parent-view key)
                        (:triple-dataflow/entity-id (key parent-view))
                        (triple-dataflow/create-entity-id))]
    (do (when (not (contains? parent-view key))
          (apply-delayed (fn [parent-view]
                           (let [child-view (-> (triple-dataflow/switch-entity parent-view child-view-id)
                                                (as-> child-view
                                                      ((apply partial view parameters) child-view)
                                                      #_(apply view child-view parameters))
                                                (assoc-with-this :preferred-width #(layoutable/preferred-width (:view %))
                                                                 :preferred-height #(layoutable/preferred-height (:view %))))]
                             (-> (triple-dataflow/switch-entity child-view parent-view)
                                 (assoc key child-view))))))

        (->ViewPart key child-view-id))))


;; RENDERING

(defn draw-view-part [gpu-state layout-path]
  (debug/do-debug :render "draw-view-part " layout-path)

  (doseq [command-runner (get-in gpu-state [:view-part-command-runners layout-path])]
    (if (instance? ViewPartCall command-runner)
      (draw-view-part gpu-state (:view-part-layout-path command-runner))
      (debug/debug-drop-last :render "running" (type command-runner)
                             (command/run command-runner)))))

(defn render [gpu-state]
  (opengl/clear 0 0 0 0)
  (draw-view-part gpu-state [:layout]))



;; TIME

(defn update-time
  ([view]
     (update-time view (System/nanoTime)))

  ([view time]
     (-> view
         (dataflow/define-to :time time)
         (dataflow/propagate-changes))))


(defn is-time-dependant? [view]
  (not (empty? (dataflow/dependants view [:time]))))

;; EVENT HANDLING

(defn call-event-handler [view-state event]
  (binding [dataflow/current-path [:elements]]
    ((:event-handler view-state)
     view-state
     event)))

(defn handle-event [view-state event]
  (debug/debug :events "handle event " event)
  (let [view-state (-> view-state
                       (assoc :event-handled false)
                       (update-time #_(:time event)))]
    (cond (= (:source event) :mouse)
          (handle-mouse-event view-state event)

          (= (:type event) :resize-requested)
          (dataflow/define-to view-state
            :width (:width event)
            :height (:height event))

          (= (:type event) :close-requested)
          (-> view-state
              (assoc :closing true)
              (call-event-handler event))

          :default
          (call-event-handler view-state event))))

(defn handle-events [view events]
  (let [events (->> events
                    (trim-mouse-movements)
                    (map (partial invert-mouse-y (get view [:height]))))]

    (reduce handle-event view events)))


(defn update-gpu [view-state]
  (let [changes-to-be-processed (dataflow/changes view-state)
        resized (some #{[:width] [:height]} changes-to-be-processed)
        changed-view-part-layout-paths (filter #(view-part-is-loaded? @(:gpu-state view-state) %)
                                               changes-to-be-processed)]

    ;;(debug/do-debug :view-update "New view state:")
    ;;(debug/debug-all :view-update (dataflow/describe-dataflow view-state))
    (when resized
      (window/resize (get view-state [:width])
                     (get view-state [:height])))
    (when (not (empty? changed-view-part-layout-paths))
      (-> (swap! (:gpu-state view-state)
                 (fn [gpu-state]
                   (-> (reduce (fn [gpu-state layout-path]
                                 (update-view-part gpu-state view-state layout-path))
                               gpu-state
                               changed-view-part-layout-paths)
                       ((fn [gpu-state]
                          (debug/do-debug :view-update "New gpu state:")
                          (debug/debug-all :view-update (describe-gpu-state gpu-state))
                          gpu-state)))))
          (render)))))



;; UPDATE

(defn update [view-atom events]
  (swap! view-atom
         (fn [view]
           (-> view
               (handle-events events)
               (dataflow/propagate-changes)
               ;;(update-time)
               ;;(update-fps)
               ))))


;; INITIALIZATION

(defn initialize-view-state [width height root-element-constructor]
  (-> (dataflow/create)
      (dataflow/define-to
        :width width
        :height height
        :mouse-x 0
        :mouse-y 0
        :fps 0
        :elements root-element-constructor
        :layout #(layout/set-dimensions-and-layout (dataflow/get-global-value :elements)
                                                   0
                                                   0
                                                   0
                                                   0
                                                   (dataflow/get-global-value :width)
                                                   (dataflow/get-global-value :height)))))

(defn create [width height event-handler root-element-constructor]
  (-> (initialize-view-state width
                             height
                             root-element-constructor)
      (assoc
          :fpss []
          :last-update-time (System/nanoTime)
          :mouse-event-handlers-under-mouse []
          :gpu-state (atom {:view-part-command-runners {}})
          :event-handler event-handler)))

(defn initialize-gpu-state [view-state]
  (swap! (:gpu-state view-state) load-view-part
         view-state [:layout]))

(defn set-view [state view]
  (-> state
      (dataflow/define-to [:elements] view)
      (dataflow/propagate-changes)))
