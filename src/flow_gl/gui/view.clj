(ns flow-gl.gui.view
  (:require  (flow-gl.graphics [command :as command])
             (flow-gl.graphics.command [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview]
                                       [translate :as translate])
             (flow-gl.gui [layout :as layout]
                          [layoutable :as layoutable]
                          [drawable :as drawable]
                          [input :as input]
                          [mouse :as mouse]
                          [drawable :as drawable])
             (flow-gl.opengl [window :as window])
             (flow-gl [opengl :as opengl]
                      [debug :as debug])
             (flow-gl.dataflow [dataflow :as dataflow]
                               [triple-dataflow :as triple-dataflow]
                               [base-dataflow :as base-dataflow])


             [clojure.data.priority-map :as priority-map])

  (:use clojure.test
        midje.sweet
        flow-gl.threading))

;; DEBUG

(defn describe-gpu-state [gpu-state]
  (for [key (keys (:view-part-command-runners gpu-state))]
    (str key " = " (vec (get-in gpu-state [:view-part-command-runners key])))))


;; VIEW PARTS

(defrecord ChildViewCall [view-id]
  command/Command
  (create-runner [view-part-call] view-part-call)
  command/CommandRunner
  (delete [view-part-call])
  (run [view-part-call]))

#_(defn element-path-to-layout-path [element-path]
    (vec (concat [:layout] (rest element-path))))

(def ^:dynamic view-being-laid-out)

(defrecord ViewPart [local-id view-id]
  drawable/Drawable
  (drawing-commands [view-part]
    [(->ChildViewCall view-id)])

  layoutable/Layoutable
  (preferred-width [view-part] (triple-dataflow/get-value base-dataflow/current-dataflow view-id :preferred-width))
  (preferred-height [view-part] (triple-dataflow/get-value base-dataflow/current-dataflow view-id :preferred-height))

  layout/Layout
  (layout [view-part requested-width requested-height global-x global-y]
    (base-dataflow/apply-to-dataflow (fn [dataflow]
                                       (-> (triple-dataflow/create-entity dataflow
                                                                          view-id)

                                           (assoc :requested-width requested-width
                                                  :requested-height requested-height
                                                  :global-x global-x
                                                  :global-y global-y)

                                           :tripple-dataflow/dataflow)))
    view-part)

  (children [this]
    (layout/children (triple-dataflow/get-value base-dataflow/current-dataflow view-id :layout)))

  Object
  (toString [_] (str "(->ViewPart " view-id)))

(defn loaded-view-parts [gpu-state]
  (keys (:view-part-command-runners gpu-state)))

(defn view-part-is-loaded? [gpu-state view-part-layout-path]
  (contains? (:view-part-command-runners gpu-state) view-part-layout-path))

(defn unload-view-part [gpu-state view-id]
  (dorun (map command/delete (get-in gpu-state [:view-part-command-runners view-id])))
  (update-in gpu-state [:view-part-command-runners] dissoc view-id))

(defn load-view-part [gpu-state view-state view-id]
  (unload-view-part gpu-state view-id)

  (debug/do-debug :view-update "loading " view-id)

  (if (triple-dataflow/is-defined? view-state view-id :layout)
    (let [drawing-commands (if-let [layout (triple-dataflow/get-value view-state view-id :layout)]
                             (drawable/drawing-commands layout)
                             [])
          gpu-state (reduce (fn [gpu-state view-id]
                              (load-view-part gpu-state view-state view-id))
                            gpu-state
                            (->> (filter #(and (instance? ChildViewCall %)
                                               (not (view-part-is-loaded? gpu-state (:view-id %))))
                                         drawing-commands)
                                 (map :view-id)))]

      (assoc-in gpu-state [:view-part-command-runners view-id] (command/command-runners-for-commands drawing-commands)))
    gpu-state))

(defn update-view-part [gpu-state view-state view-id]
  (if (triple-dataflow/is-defined? view-state view-id :layout)
    (load-view-part gpu-state view-state view-id)
    (unload-view-part gpu-state view-id)))

(defmacro view [parameters view-expression]
  (let [state-symbol (first parameters)]
    `(fn ~parameters
       (triple-dataflow/assoc-with-this ~state-symbol
                                        :view (fn [~state-symbol]
                                                ~view-expression)

                                        :layout (fn [state#]
                                                  (layout/layout (:view state#)
                                                                 (:requested-width state#)
                                                                 (:requested-height state#)
                                                                 (:global-x state#)
                                                                 (:global-y state#)))

                                        :preferred-width (fn [state#]
                                                           (layoutable/preferred-width (:view state#)))))))

(defn defview [name parameters view-expression]
  '(def ~name (view parameters view-expression)))

(defn child-view-key [& identifiers]
  (keyword (str "child-view-" identifiers)))

(defn call-child-view [parent-view identifiers view & parameters]
  (let [key (apply child-view-key identifiers)
        child-view-id (if (contains? parent-view key)
                        (::entity-id (key parent-view))
                        (triple-dataflow/create-entity-id))]

    (do (when (not (contains? parent-view key))
          (base-dataflow/apply-to-dataflow (fn [dataflow]
                                             (-> (apply view
                                                        (triple-dataflow/create-entity dataflow child-view-id)
                                                        parameters)
                                                 (triple-dataflow/switch-entity  parent-view)
                                                 (assoc key (triple-dataflow/create-entity-reference-for-id child-view-id))
                                                 ::dataflow))))
        (->ChildViewCall child-view-id))))

#_(defn init-and-call [parent-view identifiers view & parameters]
    (let [key (keyword (str "child-view-" identifiers))
          child-view-id (if (contains? parent-view key)
                          (:triple-dataflow/entity-id (key parent-view))
                          (triple-dataflow/create-entity-id))]
      (do (when (not (contains? parent-view key))
            (apply-delayed (fn [parent-view]
                             (let [child-view (-> (triple-dataflow/switch-entity parent-view child-view-id)
                                                  (as-> child-view
                                                        (apply view child-view parameters))
                                                  (triple-dataflow/assoc-with-this :preferred-width #(layoutable/preferred-width (:view %))
                                                                                   :preferred-height #(layoutable/preferred-height (:view %))
                                                                                   :layout #(layout/set-dimensions-and-layout (:view %)
                                                                                                                              0
                                                                                                                              0
                                                                                                                              (:global-x %)
                                                                                                                              (:global-y %)
                                                                                                                              requested-width
                                                                                                                              requested-height)))]
                               (-> (triple-dataflow/switch-entity child-view parent-view)
                                   (assoc key child-view))))))

          (->ViewPart key child-view-id))))

;; RENDERING

(defn draw-view-part [gpu-state view-id]
  (debug/do-debug :render "draw-view-part " view-id)

  (doseq [command-runner (get-in gpu-state [:view-part-command-runners view-id])]
    (if (instance? ChildViewCall command-runner)
      (draw-view-part gpu-state (:view-id command-runner))
      (debug/debug-drop-last :render "running" (type command-runner)
                             (command/run command-runner)))))

(defn render [gpu-state]
  (opengl/clear 0 0 0 0)
  (draw-view-part gpu-state :root-view))


;; TIME

(defn update-time
  ([view]
     (update-time view (System/nanoTime)))

  ([view time]
     (-> view
         (triple-dataflow/set-value :globals :time time)
         (dataflow/propagate-changes))))

(defn is-time-dependant? [view]
  (not (empty? (triple-dataflow/dependents view :globals :time))))


;; EVENT HANDLING

(defn call-event-handler [view-state event]
  ((:event-handler view-state)
   (triple-dataflow/create-entity view-state :root-view)
   event))

(defn handle-event [view-state event]
  (debug/debug :events "handle event " event)
  (let [view-state (-> view-state
                       (assoc :event-handled false)
                       (update-time #_(:time event)))]
    (cond (= (:source event) :mouse)
          (mouse/handle-mouse-event view-state event)

          (= (:type event) :resize-requested)
          (-> view-state
              (triple-dataflow/set-value :globals :width (:width event))
              (triple-dataflow/set-value :globals :height (:height event)))

          (= (:type event) :close-requested)
          (-> view-state
              (assoc :closing true)
              (call-event-handler event))

          :default
          (call-event-handler view-state event))))

(defn handle-events [view events]
  (let [events (->> events
                    (mouse/trim-mouse-movements)
                    (map (partial mouse/invert-mouse-y (triple-dataflow/get-value view :globals :height))))]

    (reduce handle-event view events)))


;; UPDATE

(defn update-gpu [view-state]
  (let [changes-to-be-processed (dataflow/changes view-state)
        resized (some #{[:globals :width] [:globals :height]} changes-to-be-processed)
        changed-view-ids (filter #(view-part-is-loaded? @(:gpu-state view-state) %)
                                 (map second changes-to-be-processed))]

    ;;(debug/do-debug :view-update "New view state:")
    ;;(debug/debug-all :view-update (dataflow/describe-dataflow view-state))
    (when resized
      (window/resize (triple-dataflow/get-value view-state :globals :width)
                     (triple-dataflow/get-value view-state :globals :height)))
    (when (not (empty? changed-view-ids))
      (-> (swap! (:gpu-state view-state)
                 (fn [gpu-state]
                   (-> (reduce (fn [gpu-state view-id]
                                 (update-view-part gpu-state view-state view-id))
                               gpu-state
                               changed-view-ids)
                       ((fn [gpu-state]
                          (debug/do-debug :view-update "New gpu state:")
                          (debug/debug-all :view-update (describe-gpu-state gpu-state))
                          gpu-state)))))
          (render)))))

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

(defn initialize-view-state [width height root-view]
  (-> (base-dataflow/create)
      (triple-dataflow/create-entity :globals)
      (assoc :width width
             :height height
             :mouse-x 0
             :mouse-y 0
             :fps 0)
      (triple-dataflow/initialize-new-entity :root-view root-view)
      :root-view
      (assoc :global-x 0
             :global-y 0
             :requested-width (fn [dataflow] (triple-dataflow/get-value :globals :width))
             :requested-height (fn [dataflow] (triple-dataflow/get-value :globals :height)))
      :triple-dataflow/dataflow))

(fact (initialize-view-state 100 100 (view [state] (drawable/->Rectangle 100 100 [0 0 1 1])))
      => nil)

(defn create [width height event-handler root-view]
  (-> (initialize-view-state width
                             height
                             root-view)
      (assoc
          :fpss []
          :last-update-time (System/nanoTime)
          :mouse-event-handlers-under-mouse []
          :gpu-state (atom {:view-part-command-runners {}})
          :event-handler event-handler)))

(defn initialize-gpu-state [view-state]
  (swap! (:gpu-state view-state) load-view-part
         view-state :root-view))

(defn set-view [view-state view]
  (-> view-state
      (triple-dataflow/create-entity :globals)
      (triple-dataflow/initialize-new-entity :root-view view)
      :triple-dataflow/dataflow
      (dataflow/propagate-changes)))
