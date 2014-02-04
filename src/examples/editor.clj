(ns examples.editor
  (:require (flow-gl.gui [input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow])
            (flow-gl.graphics [font :as font])))
(comment 

  (defn handle-editing-event [state editor event]
    (cond
     (input/key-pressed? event input/escape)
     (-> state
         (dataflow/define-to (concat editor [:cursor-position]) 0)
         (dataflow/define-to (concat editor [:editing]) false)
         (dataflow/define-to (concat editor [:edited-value]) (get state (concat editor [:value])))
         (assoc :event-handled true))

     (input/key-pressed? event input/left)
     (dataflow/apply-to-value state (concat editor [:cursor-position]) dec)

     (input/key-pressed? event input/right)
     (dataflow/apply-to-value state (concat editor [:cursor-position]) inc)

     (not (nil? (:character event)))
     (-> state
         (dataflow/apply-to-value (concat editor [:edited-value]) (fn [edited-value]
                                                                    (-> (StringBuffer. edited-value)
                                                                        (.insert (get state (concat editor [:cursor-position])) (:character event))
                                                                        (.toString))))

         (dataflow/apply-to-value  (concat editor [:cursor-position]) inc)
         (assoc :event-handled true))



     :default state))

  (defn handle-editor-event [state editor event]
    (cond

     (input/key-pressed? event input/enter)
     (if (dataflow/property-from state editor :editing)
       ((dataflow/property-from state editor :change-listener)
        (dataflow/define-to state
          (concat editor [:value]) (dataflow/property-from state editor :edited-value)
          (concat editor [:cursor-position]) 0
          (concat editor [:editing]) false)
        (dataflow/property-from state editor :edited-value))
       (dataflow/define-to state (concat editor [:editing]) true))

     :default (if (get state (concat editor [:editing]))
                (handle-editing-event state editor event)
                state)))


  (defn cursor [editor font]
    (let [text (dataflow/property editor :edited-value)
          cursor-position (dataflow/property editor :cursor-position)
          width 1
          height (font/height font)]
      (layout/->Translation (font/width font (subs text 0 cursor-position))
                            0
                            (drawable/->Rectangle width
                                                  height
                                                  #_(let [duration (* 1.5 1e9)
                                                          value (-> (dataflow/get-global-value :time)
                                                                    (mod duration)
                                                                    (/ duration)
                                                                    (* 2 Math/PI)
                                                                    (Math/sin)
                                                                    (/ 2)
                                                                    (+ 0.5))]
                                                      [1 value value 1])
                                                  [1 0 0 1]
                                                  ))))


  (defn editor [value change-listener]
    (let [font (font/create "LiberationSans-Regular.ttf" 15)
          editor-path (dataflow/absolute-path [])]
      (dataflow/initialize
       :value value
       :edited-value value
       :editing false
       :cursor-position 0
       :change-listener (fn [] change-listener))

      (view/initialize-view-part :cursor #(cursor editor-path
                                                  font))

      (let [text (if (dataflow/get-value :editing)
                   (dataflow/get-value :edited-value)
                   (dataflow/get-value :value))]
        (layout/->Box 2
                      (drawable/->Rectangle 0
                                            0
                                            [1 1 1 1])

                      (layout/->Stack (concat (if (dataflow/get-value :editing)
                                                [(view/call-view-part :cursor)]
                                                [])
                                              [(drawable/->Text text
                                                                font
                                                                [0 0 0 1])]))))))



  (defn view []
    (editor "Foo" (fn [state new-value]
                    (println new-value)
                    state)))

  (defn initialize [state state-atom]
    state)

  (defn start []
    (application/start view
                       :handle-event handle-editor-event
                       :initialize initialize
                       :framerate 160))

  (defn refresh []
    (when @application/state-atom-atom
      (swap! @application/state-atom-atom
             view/set-view
             view)))

  (refresh)

  (defn start-async []
    (.start (Thread. start)))

  (comment
    (start-async)
    (start)
    )
  )
