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
             (flow-gl.dataflow [dataflow :as dataflow]
                               [triple-dataflow :as triple-dataflow]
                               [base-dataflow :as base-dataflow])


             [clojure.data.priority-map :as priority-map])

  (:use clojure.test
        flow-gl.threading))

;; DEBUG

(defn describe-gpu-state [gpu-state]
  (for [key (keys (:view-part-command-runners gpu-state))]
    (str key " = " (vec (get-in gpu-state [:view-part-command-runners key])))))


;; VIEW PARTS

(defrecord ViewPartCall [view-id]
  command/Command
  (create-runner [view-part-call] view-part-call)
  command/CommandRunner
  (delete [view-part-call])
  (run [view-part-call]))

(defn element-path-to-layout-path [element-path]
  (vec (concat [:layout] (rest element-path))))

(def ^:dynamic view-being-laid-out)

(defrecord ViewPart [local-id view-id]
  drawable/Drawable
  (drawing-commands [view-part]
    [(->ViewPartCall view-id)])

  layoutable/Layoutable
  (preferred-width [view-part] (triple-dataflow/get-value base-dataflow/current-dataflow view-id :preferred-width))
  (preferred-height [view-part] (triple-dataflow/get-value base-dataflow/current-dataflow view-id :preferred-height))

  layout/Layout
  (layout [view-part requested-width requested-height global-x global-y]
    (do (base-dataflow/apply-to-dataflow (fn [dataflow]
                                           (-> (triple-dataflow/create-entity dataflow
                                                                              view-id)

                                               (assoc :requested-width requested-width
                                                      :requested-height requested-height
                                                      :global-x global-x
                                                      :global-y global-y)

                                               :tripple-dataflow/dataflow)))
        layoutable)
    view-part)

  (children-in-coordinates [this view-state x y]
    (layout/children-in-coordinates (triple-dataflow/get-value base-dataflow/current-dataflow view-id :layout)
                                    view-state
                                    x
                                    y))

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
                            (->> (filter #(and (instance? ViewPartCall %)
                                               (not (view-part-is-loaded? gpu-state (:view-part-layout-path %))))
                                         drawing-commands)
                                 (map :view-part-layout-path)))]

      (assoc-in gpu-state [:view-part-command-runners view-id] (command/command-runners-for-commands drawing-commands)))
    gpu-state))

(defn update-view-part [gpu-state view-state view-id]
  (if (triple-dataflow/is-defined? view-state view-id :layout)
    (load-view-part gpu-state view-state view-id)
    (unload-view-part gpu-state view-id)))


(defmacro view [parameters view-expression]
  (let [state-symbol (first parameters)]
    `(fn ~parameters
       (assoc-with-this ~state-symbol
                        :view (fn [~state-symbol]
                                ~view-expression)

                        :layout (fn [state#]
                                  (layout (:view state#)
                                          (:requested-width state#)
                                          (:global-x state#)))

                        :preferred-width (fn [state#]
                                           (preferred-width (:view state#)))))))

(defn defview [name parameters view-expression]
  '(def ~name (view parameters view-expression)))

(defn call-child-view [parent-view identifiers view & parameters]
  (let [key (apply child-view-key identifiers)
        child-view-id (if (contains? parent-view key)
                        (::entity-id (key parent-view))
                        (create-entity-id))]

    (do (when (not (contains? parent-view key))
          (base-dataflow/apply-to-dataflow (fn [dataflow]
                                             (-> (apply view
                                                        (create-entity dataflow child-view-id)
                                                        parameters)
                                                 (switch-entity  parent-view)
                                                 (assoc key (create-entity-reference-for-id child-view-id))
                                                 ::dataflow))))
        {:type :child-view-call
         :kwey key
         :child-view-id child-view-id})))

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
    (if (instance? ViewPartCall command-runner)
      (draw-view-part gpu-state (:view-id command-runner))
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
         (triple-dataflow/set-value :globals :time time)
         (dataflow/propagate-changes))))

(defn is-time-dependant? [view]
  (not (empty? (triple-dataflow/dependents view [:time]))))


;; EVENT HANDLING

(defn call-event-handler [view-state event]
  #_(binding [dataflow/current-path [:elements]]
      ((:event-handler view-state)
       view-state
       event)))



(defn handle-event [view-state event]
  (debug/debug :events "handle event " event)
  #_(let [view-state (-> view-state
                         (assoc :event-handled false)
                         (update-time #_(:time event)))]
      (cond (= (:source event) :mouse)
            (mouse/handle-mouse-event view-state event)

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
                    (mouse/trim-mouse-movements)
                    (map (partial mouse/invert-mouse-y (get view [:height]))))]

    (reduce handle-event view events)))


;; UPDATE

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
  #_(-> (dataflow/create)
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
  #_(-> state
        (dataflow/define-to [:elements] view)
        (dataflow/propagate-changes)))
