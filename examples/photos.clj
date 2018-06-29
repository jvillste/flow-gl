(ns examples.photos
  (:require [clojure.core.async :as async]
            [com.climate.claypoole :as claypoole]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            [flow-gl.debug :as debug])
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable])
  (:use flow-gl.utils
        clojure.test))


(defn files-in-directory [directory-path]
  (.listFiles (File. directory-path)))

(defn years [archive-path]
  (->> (files-in-directory archive-path)
       (map #(.getName %))
       (filter #(number? (read-string %)))))

(defn day-directory-name? [value]
  (-> (string/split value #"-")
      (count)
      (= 3)))

(defn read-two-digit-number [text]
  (if (= \0 (first text))
    (read-string (apply str (rest text)))
    (read-string text)))

(defn month [day-directory-name]
  (->> (string/split day-directory-name #"-")
       (second)
       (read-two-digit-number)))

(defn day [day-directory-name]
  (-> (string/split day-directory-name #"-")
      (nth 2)
      (read-two-digit-number)))

(defn day-directory-names [year-path]
  (->> (files-in-directory year-path)
       (map #(.getName %))
       (filter day-directory-name?)))

(defn months [year-path]
  (->> (day-directory-names year-path)
       (map month)
       (apply hash-set)
       (sort)
       (vec)))

(defn days [year-path month-number]
  (->> (day-directory-names year-path)
       (filter #(= month-number (month %)))
       (map day)))

(defn images [day-path]
  (->> (files-in-directory day-path)
       (map #(.getPath %))
       (filter #(.endsWith % ".jpg"))))

(gui/def-control image
  ([view-context control-channel file-name thread-pool]
     (let [buffered-image-channel (async/chan)
           buffered-image-future (.execute thread-pool
                                           #(do
                                              (flow-gl.debug/debug-timed "starting to load " file-name)
                                              (let [buffered-image (buffered-image/create-resized-from-file file-name
                                                                                                            100
                                                                                                            100)]
                                                (flow-gl.debug/debug-timed "ready " file-name)
                                                (async/put! buffered-image-channel buffered-image)
                                                (flow-gl.debug/debug-timed "sent buffered image " file-name))))]

       (async/go (async/alt! control-channel ([_]
                                                (flow-gl.debug/debug-timed "canceling " file-name)
                                                (.cancel buffered-image-future true))

                             buffered-image-channel ([buffered-image]
                                                       (flow-gl.debug/debug-timed "got buffered image for " file-name)
                                                       (gui/apply-to-state view-context (fn [state]
                                                                                          (flow-gl.debug/debug-timed "applying " file-name)
                                                                                          (assoc state :buffered-image buffered-image)))))))

     {:file-name file-name})

  ([view-context {:keys [buffered-image file-name]}]
     (if buffered-image
       (drawable/->Image buffered-image)
       (drawable/->Rectangle 100 100 [255 255 255 255]))))

(gui/def-control date-box
  ([view-context control-channel]
     {})

  ([view-context {:keys [text selected mouse-over]}]
     (controls/text text (if selected
                           [0.3 0.8 0.4 1]
                           (if mouse-over
                             [1 0 0 1]
                             [0.8 0.8 0.8 1])))))


(defn date-navigation [view-context selected-year selected-month selected-day archive-path]
  (let [selected-color [0.3 0.8 0.4 1]]
    (l/horizontally
     (apply l/vertically (for [year (years archive-path)]
                           (-> (controls/text year (if (= selected-year
                                                          year)
                                                     selected-color
                                                     [0.8 0.8 0.8 1]))

                               (gui/on-mouse-clicked view-context
                                                     (fn [state time]
                                                       (assoc state
                                                         :selected-year year
                                                         :month (first (months (str archive-path "/" year))))) ))))

     (l/margin 0 0 0 10 (apply l/vertically (doall (for [month (months (str archive-path "/" selected-year))]
                                                     (-> (date-box {:text month
                                                                    :selected (= selected-month
                                                                                 month)})
                                                         (gui/on-mouse-clicked view-context
                                                                               (fn [state time]
                                                                                 (assoc state
                                                                                   :selected-month month
                                                                                   :selected-day (first (days (str archive-path "/" selected-year) month)))) ))))))

     (l/margin 0 0 0 10 (apply l/vertically (doall (for [day (days (str archive-path "/" selected-year) selected-month)]
                                                     (-> (controls/text day (if (= selected-day
                                                                                   day)
                                                                              selected-color
                                                                              [0.8 0.8 0.8 1]))
                                                         (gui/on-mouse-clicked view-context
                                                                               (fn [state time]
                                                                                 (assoc state :selected-day day)))))))))))

(gui/def-control photo-archive-browser
  ([view-context control-channel]
     (let [thread-pool (Executors/newFixedThreadPool (.. Runtime getRuntime availableProcessors))]
       (async/go (<! control-channel)
                 (flow-gl.debug/debug-timed "shutting down the threadpool")
                 (.shutdown thread-pool))

       (let [archive-path "/Users/jukka/Pictures/arkisto_mini"
             year (first (years archive-path))
             year-path (str archive-path "/" year)
             month (first (months year-path))]
         {:archive-path archive-path
          :selected-year year
          :selected-month month
          :selected-day (first (days year-path month))
          :thread-pool thread-pool})))

  ([view-context {:keys [selected-year selected-month selected-day archive-path thread-pool]}]
     (flow-gl.debug/debug-timed "view")

     #_(gui/call-named-view image-view
                            create-image
                            :image
                            ["/Users/jukka/Pictures/arkisto_mini/2011/2011-12-28/2011-12-28.19.44.39_ac66709411ae6c2948d95bd90199cccc.jpg"]
                            [])

     (layouts/->FloatLeft (date-navigation view-context selected-year selected-month selected-day archive-path)
                          (l/margin 0 0 0 10
                                    (layouts/->Flow
                                     (doall (for [image-file-name (images (str archive-path
                                                                               "/" selected-year
                                                                               "/" selected-year
                                                                               "-" (format "%02d" selected-month)
                                                                               "-" (format "%02d" selected-day)))]

                                              (l/margin 2 2 2 2 (image image-file-name
                                                                       {}
                                                                       [image-file-name thread-pool])))))))))

(def log (atom []))

(defn start []
  (debug/with-log log
    (gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view))

  #_(.start (Thread. (fn []
                       (gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view)))))

(gui/redraw-last-started-view)


#_(let [file-name "/Users/jukka/Pictures/arkisto_mini/2011/2011-12-28/2011-12-28.19.44.39_ac66709411ae6c2948d95bd90199cccc.jpg"
        control-channel (async/chan)
        event-channel (async/chan)
        pool (Executors/newFixedThreadPool 1)]
    (flow-gl.debug/reset-log)
    (dotimes [i 10]
      (create-image {:state-path []
                     :event-channel event-channel}
                    control-channel
                    file-name
                    pool))

    (async/go-loop []
      (async/alt! control-channel ([])
                  event-channel ([message]
                                   (println message)
                                   (recur))))

    (Thread/sleep 4000)
    (.shutdown pool)
    (async/close! control-channel)
    (async/close! event-channel)
    (flow-gl.debug/write-timed-log)

    #_(let [pool (Executors/newFixedThreadPool 1)
            futures (doall (for [i (range 30)]
                             (.submit pool (cast Runnable #(do (buffered-image/create-resized-from-file file-name
                                                                                                        100
                                                                                                        100)
                                                               (println "ready" i))))))]
        (Thread/sleep 1000)
        (doseq [f futures]
          (.cancel f true))

        (.shutdown pool))

    #_(let [futures (doall (for [i (range 5)]
                             (future (buffered-image/create-resized-from-file file-name
                                                                              100
                                                                              100)
                                     (println "ready" i))))]

        (doseq [fut futures] (deref fut)))

    #_(claypoole/with-shutdown! [pool (claypoole/threadpool 4)]

        (dotimes [i 5]
          (claypoole/upvalues pool
                              (do (buffered-image/create-resized-from-file file-name
                                                                           100
                                                                           100)
                                  (println i)))

          #_(async/thread (buffered-image/create-resized-from-file file-name
                                                                   100
                                                                   100)
                          (println i)))))
