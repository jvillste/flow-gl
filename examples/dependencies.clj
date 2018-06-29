(ns dependencies
  (:require [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.parse :as parse]
            [clojure.tools.namespace.dependency :as dependency]
            [clojure.tools.namespace.track :as track]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import java.io.File))

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

(def dependency-graph (reduce (fn [graph ns-declaration]
                                (reduce (fn [graph dependency]
                                          (dependency/depend graph
                                                             (second ns-declaration)
                                                             dependency))
                                        graph
                                        (parse/deps-from-ns-decl ns-declaration)))
                              (dependency/graph)
                              (find/find-ns-decls-in-dir (File. "src"))))

(def all-ns-in-fungl (set (map second (find/find-ns-decls-in-dir (File. "src")))))

(def transitive-painta-deps (set (mapcat (partial dependency/transitive-dependencies dependency-graph)
                                         painta-deps)))

(def unused-namespaces (set/difference all-ns-in-fungl
                                       transitive-painta-deps))

(defn ns-path [ns]
  (-> (name ns)
      (string/replace #"-" "_")
      (string/replace #"\." "/")
      (str ".clj")))

(comment
  (map ns-path unused-namespaces))
