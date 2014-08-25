(ns examples.registration-form-2
  (:require [clojure.core.async :as async]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]))
  (:import [java.io File])
  (:use flow-gl.utils
        clojure.test))


(defn files-in-directory [directory-path]
  (println "file in " directory-path)
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

(def archive-path "/Users/jukka/Downloads/arkisto_mini/")

(quad-gui/def-control foo
  ([view-context control-channel]
     {})

  ([view-context state]
     (controls/text "foo")))

(quad-gui/def-control photo-archive-browser
  ([view-context control-channel]
     (let [archive-path "/Users/jukka/Downloads/arkisto_mini/"
           year (first (years archive-path))
           year-path (str archive-path "/" year)
           month (first (months year-path))]
       {:archive-path archive-path
        :selected-year year
        :selected-month month}))

  ([view-context {:keys [selected-year archive-path]}]

     (layouts/->VerticalStack (doall (for [year (years archive-path)]
                                       (controls/text year (if (= selected-year
                                                                  year)
                                                             [1 1 1 1]
                                                             (vec (map (partial * 0.8)
                                                                       [1 1 1 1])))))))))


(defn start []
  (quad-gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view))

#_(println (months (str archive-path "/2014")))
