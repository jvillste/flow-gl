(ns examples.size-group
  (:require (flow-gl.gui [input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                                        ;[animation :as animation]
                         [events :as events]
                         [application :as application]
                         [reset-view :as reset-view]

                                        ;[focus :as focus]
                         )
            (flow-gl.graphics [font :as font])
            (flow-gl [dataflow :as dataflow]))
  (:use flow-gl.utils
        flow-gl.gui.layout-dsl))


(comment 
  (def data [{:foo "asdfasdfsdfdsfdsf"
              :bar "sdf"}
             {:foo "asdfa"
              :bar "sdfsdfsf"}
             {:foo "asdfasf"
              :bar "sdf"}
             {:foo "asdfasd"
              :bar "sdsdfdfsdsdff"}])

  (defn row [size-group row-data]
    (let [font (font/create "LiberationSans-Regular.ttf" 15)]
      (hs (layout/size-group-member size-group
                                    :width
                                    (drawable/->Text (:foo row-data)
                                                     font
                                                     [1 1 1 1]))

          (drawable/->Text (:bar row-data)
                           font
                           [1 1 1 1]))))

  (defn view []
    (let [size-group (layout/create-size-group)]
      (apply vs (for-all [row-data data]
                        (row size-group row-data)))))


  (defn start []
    (application/start view
                       :initialize reset-view/initialize
                       :handle-event events/close-on-esc
                       :framerate 60))

  (reset-view/reset-view view)

  (comment
    (.start (Thread. start))
    (start)
    )
  )
