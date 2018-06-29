(ns examples.fast-update
  (:require [flow-gl.utils :as utils]

            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events])

            (flow-gl.graphics [command :as command]
                              [font :as font]
                              [buffered-image :as buffered-image]
                              [text :as graphics-text])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [texture :as texture]
                                 [textured-quad :as textured-quad])
            clojure.data)
  (:import [com.jogamp.opengl.util.texture.awt AWTTextureIO]
           [com.jogamp.opengl GLProfile])
  (:use flow-gl.utils
        midje.sweet))


[[0 [1 2]]]

(defn differences [old new]
  (if (identical? old new)
    nil
    (map differences
         (:children old)
         (:children new))))

(facts (let [old {:children [{:text "1"}
                             {:text "2"
                              :children [{:text "2.1"}
                                         {:text "2.2"}]}
                             {:text "3"}]}]
         (fact (differences old
                            (update-in old [:children 1 :children 1 :text] str "X"))
               => nil)

         (fact (differences old
                            (update-in old [:children 1 :children] conj {:text "2.3"}))
               => nil)))

(defn start-view [view event-handler initial-state]
  (let [event-queue (event-queue/create)
        window (window/create 300
                              300
                              opengl/initialize
                              opengl/resize
                              event-queue)
        tile (buffered-image/create 300 300)
        textured-quad-atom (atom nil)]

    (window/render window gl
                   (named-time "creating quad" (let [texture (texture/create-for-buffered-image tile gl)
                                                     textured-quad (textured-quad/create texture gl)]
                                                 (reset! textured-quad-atom textured-quad))))

    (try
      (loop [state initial-state
             previous-layout nil]
        (println) (println)
        (let [layoutable (named-time "view" (view state))
              layout (named-time "layout" (-> (layout/layout layoutable
                                                             (window/width window)
                                                             (window/height window))
                                              (layout/add-global-coordinates 0 0)))]
          ;; (println "layout " (str layout))
          (named-time "difference" (let [difference (vec (time (clojure.data/diff layout previous-layout)))]
                                     (println "layout difference " (str (first difference)))
                                     (println "layout difference " (str (second difference)))))

          (named-time "clear tile" (buffered-image/clear tile))
          (named-time "draw layout " (drawable/draw layout (buffered-image/get-graphics tile)))

          (window/render window gl
                         (opengl/clear gl 0 0 0 1)
                         #_(.updateSubImage (:texture (:texture @textured-quad-atom))
                                            gl
                                            (AWTTextureIO/newTextureData (.getGLProfile gl) tile false)
                                            (int 0)  ;mipmap level
                                            (int 0)(int 0)(int 0)(int 0) (int 300) (int 300)
                                            ;; int dstx, int dsty, int srcx, int srcy, int width, int height
                                            )
                         #_(println "type " (.getGL gl) )
                         #_(println (:members (clojure.reflect/reflect (:texture (:texture @textured-quad-atom)) )))

                         #_(.updateSubImage (:texture (:texture @textured-quad-atom))
                                            gl
                                            (AWTTextureIO/newTextureData (.getGLProfile gl) tile false)
                                            (int 0)  ;mipmap level
                                            (int 0))

                         (named-time "updating texture data" (.updateImage (:texture (:texture @textured-quad-atom))
                                                                           gl
                                                                           (AWTTextureIO/newTextureData (.getGLProfile gl) tile false)))

                         (named-time "blitting tile" (textured-quad/render @textured-quad-atom gl))
                         #_(textured-quad/delete textured-quad gl))

          (let [event (event-queue/dequeue-event-or-wait event-queue)]
            (if (= (:type event)
                   :close-requested)
              (window/close window)

              (recur (event-handler state event)
                     layout)))))

      (catch Exception e
        (window/close window)
        (throw e)))))


(defn memoize
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn text-view [text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 20)
                   [1 1 1 1]))

(defn task-view [task]
  (layout/->VerticalStack [(text-view (:task task))
                           (layout/->Margin 0 0 0 10
                                            [(layout/->VerticalStack (map task-view (:children task)))])]))

(def data [{:task "1"
            :children [{:task "1.1"}
                       {:task "1.2"}]}
           {:task "2"
            :children [{:task "2.1"}
                       {:task "2.2"}
                       {:task "2.3"}]}
           {:task "3"
            :children [{:task "3.1"}
                       {:task "3.2"}]}])

(defn view [state]
  (layout/->VerticalStack (map task-view state)))

(defn handle-event [state event]
  (cond
   (events/key-pressed? event :enter)
   (update-in state [0 :children 1 :task] str "X")

   :default state))

(defn start []
  (start-view view
              handle-event
              data))

;;(start)
