(ns examples.property-view
  (:require (flow-gl.gui [awt-input :as input]
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

(defn property-view [view-definition]
  (println "property-view")

  (let [font (font/create "LiberationSans-Regular.ttf" 20)
        color [1 1 1 1]
        root (dataflow/property view-definition :root)
        columns (dataflow/property view-definition :columns)]

    (layout/->HorizontalStack [(drawable/->Text (str root) font color)
                               (drawable/->Empty 10 0)
                               (layout/->VerticalStack (vec (for [column columns]
                                                              (let [key (dataflow/property column :key)]
                                                                (layout/->HorizontalStack [(drawable/->Text (str key) font color)
                                                                                           (drawable/->Empty 10 0)
                                                                                           (drawable/->Text (str (dataflow/get-global-value (dataflow/path root key))) font color)])))))])))

(defn view []
  (view/init-and-call :property-view (partial property-view [:view-definition])))


(defn handle-event [state event]
  (println "handling " event)
  (cond (input/key-pressed? event input/esc)
        (do (application/request-close)
            state)

        (input/key-pressed? event input/space)
        (dataflow/define-to-global state [:name-column-definition :key] :boss)

        :default
        state))

(def state-atom-atom (atom nil))

(defn initialize [state state-atom]
  (reset! state-atom-atom state-atom)
  (dataflow/define-to state
    [:name-column-definition :key] :name
    [:boss-column-definition :key] :boss

    [:view-definition :root] [:person-1]
    [:view-definition :columns] [[:name-column-definition] [:boss-column-definition]]

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
