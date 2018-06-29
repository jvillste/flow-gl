(ns examples.tree-table
  (:require (flow-gl.gui [input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [animation :as animation]
                         [events :as events]
                         [application :as application]
                         [focus :as focus])
            (flow-gl.graphics [font :as font])
            (flow-gl [dataflow :as dataflow]))
  (:use clojure.test))

(comment 
  (defn tree-table [root & columns]
    (drawable/->Text (dataflow/get-global-value (dataflow/path root (first columns))) (font/create "LiberationSans-Regular.ttf" 20) [1 1 1 1]))

  (defn view []
    (view/init-and-call :tree-table (partial tree-table [:person-1] :name :boss)))


  (defn handle-event [state event]
    (cond (input/key-pressed? event input/esc)
          (do (application/request-close)
              state)

          :default
          state))

  (defn initialize [state state-atom]
    (dataflow/define-to state
      [:person-1 :name] "Jack"
      [:person-2 :name] "Joe"
      [:person-1 :boss] :person-2))

  (defn start []
    (application/start view
                       :handle-event handle-event
                       :initialize initialize
                       :framerate 60))


  (comment
    (.start (Thread. start))
    (start)
    )
  )
