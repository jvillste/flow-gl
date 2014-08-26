(ns examples.registration-form-2
  (:require [clojure.core.async :as async]
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
            (flow-gl.graphics [font :as font]))
  (:import [java.io File])
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

(println (images "/Users/jukka/Downloads/arkisto_mini/2011/2011-12-28"))

(def archive-path "/Users/jukka/Downloads/arkisto_mini/")


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
     (let [archive-path "/Users/jukka/Downloads/arkisto_mini/"
           year (first (years archive-path))
           year-path (str archive-path "/" year)
           month (first (months year-path))
           day (first (days year-path month))]
       {:archive-path archive-path
        :selected-year year
        :selected-month month
        :selected-day day}))

  ([view-context {:keys [selected-year selected-month selected-day archive-path]}]
     (layouts/->FloatLeft (date-navigation selected-year selected-month selected-day archive-path)
                          (l/margin 0 0 0 10 (layouts/->Flow (doall (for [image (images (str archive-path "/" selected-year "/" selected-year "-" (format "%02d" selected-month) "-" (format "%02d" selected-day)))]
                                                 #_(drawable/->Image image)
                                                 (l/margin 5 5 5 5 (drawable/->Rectangle 100 100 [1 1 1 1]))
                                                 #_(controls/text image))))))))

(defn start []
  (.start (Thread. (fn [] (quad-gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view)))))

(when-let [last-event-channel-atom @quad-gui/last-event-channel-atom]
  (async/put! last-event-channel-atom {:type :request-redraw}))

#_(println (months (str archive-path "/2014")))
