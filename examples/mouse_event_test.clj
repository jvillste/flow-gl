(ns mouse-event-test
  (:require [argumentica.db.client :as client]
            [argumentica.db.client-db :as client-db]
            [argumentica.db.server-btree-db :as server-btree-db]
            [clojure.java.io :as io]
            [examples.crud-server :as crud-server]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.font :as font]
            [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.atom-registry :as atom-registry]
            [fungl.callable :as callable]
            [fungl.component.text-area :as text-area]
            [fungl.layouts :as layouts]
            [argumentica.db.server-api :as server-api]
            [argumentica.db.server-btree-index :as server-btree-index]
            [argumentica.index :as index]
            [argumentica.comparator :as comparator]
            [argumentica.db.branch :as branch]
            [argumentica.db.common :as common]
            [clojure.test :refer :all]
            [argumentica.transaction-log :as transaction-log]
            [argumentica.transaction-log :as transaction-log]
            [argumentica.db.server-transaction-log :as server-transaction-log]
            [argumentica.db.db :as db]
            [flow-gl.opengl.jogl.window :as jogl-window]
            [argumentica.sorted-set-index :as sorted-set-index]
            [argumentica.sorted-map-transaction-log :as sorted-map-transaction-log]
            [clojure.test :refer [deftest]]
            [flatland.useful.map :as map]
            [argumentica.map-to-transaction :as map-to-transaction]))

(def state (atom {:x 0
                  :y 0}))

(defn print-coordinates [node event]
  (prn (select-keys event
                    [:x :y :local-x :local-y]))
  event)

(defn add-print-coordinates-handler [node]
  (assoc node
         :mouse-event-handler [print-coordinates]))

(defn create-scene-graph [width height]
  (-> (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                           (assoc (visuals/rectangle-2 :fill-color [155 255 255 255])
                                  :x 100
                                  :y 100
                                  :width 100
                                  :height 100)
                           (assoc (visuals/rectangle-2 :fill-color [155 155 255 255])
                                  :x (- (:x @state)
                                        5)
                                  :y (- (:y @state)
                                        5)
                                  :width 10
                                  :height 10))
      (assoc :mouse-event-handler (fn [node event]
                                    (swap! state
                                           assoc
                                           :x (:x #_:local-x event)
                                           :y (:y #_:local-y event))
                                    (prn (select-keys event
                                                      [:x :y :local-x :local-y]))
                                    event))
      (application/do-layout width height)))

(defn start []
  (application/start-window #'create-scene-graph
                            :window
                            (jogl-window/create 600 600
                                                :close-automatically true)))
