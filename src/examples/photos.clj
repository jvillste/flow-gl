(ns examples.registration-form-2
  (:require [clojure.core.async :as async]
            [com.climate.claypoole :as claypoole]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [java.io File]
           [java.util.concurrent Executors])
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
       (vec)))

(defn days [year-path month-number]
  (->> (day-directory-names year-path)
       (filter #(= month-number (month %)))
       (map day)))

(defn images [day-path]
  (->> (files-in-directory day-path)
       (map #(.getPath %))
       (filter #(.endsWith % ".jpg"))))

(quad-gui/def-control image
  ([view-context control-channel file-name thread-pool]
     (async/go (let [buffered-image (async/<! (let [channel (async/chan)]
                                                (.execute thread-pool
                                                          #(do
                                                             (println "starting to load " file-name)
                                                             (async/put! channel
                                                                         (buffered-image/create-resized-from-file file-name
                                                                                                                  100
                                                                                                                  100))
                                                             (println "ready " file-name)))
                                                channel))]
                 (quad-gui/apply-to-state view-context assoc
                                          :buffered-image buffered-image)))
     {})

  ([view-context {:keys [buffered-image]}]
     (if buffered-image
       (drawable/->Image buffered-image)
       (drawable/->Rectangle 100 100 [1 1 1 1]))))

(defn date-navigation [selected-year selected-month selected-day archive-path]
  (let [selected-color [0.3 0.8 0.4 1]]
    (l/horizontally
     (apply l/vertically (for [year (years archive-path)]
                           (-> (controls/text year (if (= selected-year
                                                          year)
                                                     selected-color
                                                     [0.8 0.8 0.8 1]))

                               (quad-gui/on-mouse-clicked assoc
                                                          :selected-year year
                                                          :month (first (months (str archive-path "/" year)))))))

     (l/margin 0 0 0 10 (apply l/vertically (doall (for [month (months (str archive-path "/" selected-year))]
                                                     (-> (controls/text month (if (= selected-month
                                                                                     month)
                                                                                selected-color
                                                                                [0.8 0.8 0.8 1]))
                                                         (quad-gui/on-mouse-clicked assoc
                                                                                    :selected-month month
                                                                                    :selected-day (first (days (str archive-path "/" selected-year) month))))))))

     (l/margin 0 0 0 10 (apply l/vertically (doall (for [day (days (str archive-path "/" selected-year) selected-month)]
                                                     (-> (controls/text day (if (= selected-day
                                                                                   day)
                                                                              selected-color
                                                                              [0.8 0.8 0.8 1]))
                                                         (quad-gui/on-mouse-clicked assoc :selected-day day)))))))))

(quad-gui/def-control photo-archive-browser
  ([view-context control-channel]
     (let [thread-pool (Executors/newFixedThreadPool (.. Runtime getRuntime availableProcessors))]
       (async/go (<! control-channel)
                 (println "shutting down the threadpool")
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

     #_(quad-gui/call-named-view image-view
                                 create-image
                                 :image
                                 ["/Users/jukka/Pictures/arkisto_mini/2011/2011-12-28/2011-12-28.19.44.39_ac66709411ae6c2948d95bd90199cccc.jpg"]
                                 [])

     (layouts/->FloatLeft (date-navigation selected-year selected-month selected-day archive-path)
                          (l/margin 0 0 0 10
                                    (layouts/->Flow
                                     (doall (for [image-file-name (images (str archive-path
                                                                               "/" selected-year
                                                                               "/" selected-year
                                                                               "-" (format "%02d" selected-month)
                                                                               "-" (format "%02d" selected-day)))]

                                              #_(drawable/->Image (buffered-image/create-from-file image))
                                              #_(l/margin 2 2 2 2 (drawable/->Rectangle 100 100 [1 1 1 1]))
                                              (l/margin 2 2 2 2 (quad-gui/call-named-view image-view
                                                                                          create-image
                                                                                          image-file-name
                                                                                          [image-file-name thread-pool]
                                                                                          []))
                                              #_(controls/text image))))))))

(defn start []
  (.start (Thread. (fn [] (quad-gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view)))))

(when-let [last-event-channel-atom @quad-gui/last-event-channel-atom]
  (async/put! last-event-channel-atom {:type :request-redraw}))

#_(println)
#_(let [file-name "/Users/jukka/Pictures/arkisto_mini/2011/2011-12-28/2011-12-28.19.44.39_ac66709411ae6c2948d95bd90199cccc.jpg"]
    (let [pool (Executors/newFixedThreadPool 4)]
      (dotimes [i 30]
        (.execute pool #(do (buffered-image/create-resized-from-file file-name
                                                                     100
                                                                     100)
                            (println "ready" i))))

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
