(ns dependencies
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [clojure.tools.namespace.dependency :as dependency]
            [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.parse :as parse])
  (:import java.io.File))

(def kept-name-spaces ['flow-gl.tools.trace])

(def painta-deps (parse/deps-from-ns-decl '(ns painta.core
                                             (:require
                                              [clojure.java.io :as io]
                                              [clojure.test :refer :all]
                                              [flow-gl.graphics.buffered-image :as buffered-image]
                                              [flow-gl.gui.keyboard :as keyboard]
                                              [flow-gl.gui.quad-renderer :as quad-renderer]
                                              [flow-gl.gui.render-target-renderer :as render-target-renderer]
                                              [flow-gl.gui.scene-graph :as scene-graph]
                                              [flow-gl.gui.visuals :as visuals]
                                              [flow-gl.opengl.jogl.opengl :as opengl]
                                              [flow-gl.opengl.jogl.quad :as quad]
                                              [flow-gl.opengl.jogl.render-target :as render-target]
                                              [flow-gl.opengl.jogl.texture :as texture]
                                              [flow-gl.opengl.jogl.window :as window]
                                              [flow-gl.profiling :as profiling]
                                              [fungl.application :as application]
                                              [fungl.atom-registry :as atom-registry]
                                              [fungl.cache :as cache]
                                              [fungl.callable :as callable]
                                              [fungl.layouts :as layouts])
                                             (:import java.awt.event.KeyEvent javax.swing.JFileChooser))))

(defn dependency-graph-for-directory [directory-name]
  (reduce (fn [graph ns-declaration]
            (reduce (fn [graph dependency]
                      (dependency/depend graph
                                         (second ns-declaration)
                                         dependency))
                    graph
                    (parse/deps-from-ns-decl ns-declaration)))
          (dependency/graph)
          (find/find-ns-decls-in-dir (File. directory-name))))

(def dependency-graph (dependency-graph-for-directory "src"))

(defn namespaces-in-directory [directory-name]
  (set (map second (find/find-ns-decls-in-dir (File. directory-name)))))

(def transitive-painta-deps (set (mapcat (partial dependency/transitive-dependencies dependency-graph)
                                         (concat painta-deps
                                                 kept-name-spaces))))

(def unused-namespaces (set/difference (namespaces-in-directory "src")
                                       transitive-painta-deps))

(defn ns-file-name [ns]
  (str (-> (name ns)
           (string/replace #"-" "_")
           (string/replace #"\." "/"))
       ".clj"))

(deftest test-ns-file-name
  (is (= "flow_gl/data/zipper_list.clj"
         (ns-file-name 'flow-gl.data.zipper-list))))

(defn source-file-name-to-archive-file-name [source-directory-name
                                             archive-directory-name
                                             source-file-name]
  (str archive-directory-name
       "/"
       (subs source-file-name
             (inc (count source-directory-name)))))

(deftest test-source-file-name-to-archive-file-name
  (is (= "old/flow-gl/flow_gl/data/zipper_list.clj"
         (source-file-name-to-archive-file-name "src"
                                                "old"
                                                "src/flow-gl/flow_gl/data/zipper_list.clj")))

  (is (= "clj-archive/flow-gl/flow_gl/data/zipper_list.clj"
         (source-file-name-to-archive-file-name "src/clj"
                                                "clj-archive"
                                                "src/clj/flow-gl/flow_gl/data/zipper_list.clj"))))

(defn archive-name-space [source-directory-name
                          archive-directory-name
                          source-name-space]
  (let [source-file-name (str source-directory-name
                              "/"
                              (ns-file-name source-name-space))
        source-file (io/file source-file-name)
        archive-file-name (source-file-name-to-archive-file-name source-directory-name
                                                                 archive-directory-name
                                                                 source-file-name)]

    (println "Archiving " source-file-name " to " archive-file-name)

    (if (and (.exists source-file)
             (not (.exists (io/file archive-file-name))))
      (do
        (io/make-parents archive-file-name)
        (io/copy (io/file source-file-name)
                 (io/file archive-file-name))
        (io/delete-file (io/file source-file-name)))
      (println (str "No need to archive " source-file-name)))))

(comment
  (doall (map (partial archive-name-space
                       "src"
                       "old")
              unused-namespaces))

  (doall (map (partial archive-name-space
                       "old"
                       "src")
              (namespaces-in-directory "old")))

  (archive-name-space "old"
                      "src"
                      'flow-gl.gui.layoutable)

  (archive-name-space "src"
                      "old"
                      'flow-gl.gui.layoutable)

  (namespaces-in-directory "old")

  (def source-namespace 'flow-gl.data.zipper-list)
  (def source-file-name (ns-file-name source-namespace))
  (def archive-file-name (source-file-name-to-archive-file-name source-file-name))
  (io/make-parents archive-file-name)
  (io/copy (io/file source-file-name)
           (io/file archive-file-name))
  (io/delete-file (io/file source-file-name))
  
  (map ns-file-name unused-namespaces))
