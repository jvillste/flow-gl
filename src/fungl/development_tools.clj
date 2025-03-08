(ns fungl.development-tools
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer [deftest is]]
   [flow-gl.csp :as csp]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.mouse :as mouse]
   [flow-gl.gui.scene-graph :as scene-graph]
   [flow-gl.gui.stateful :as stateful]
   [flow-gl.gui.window :as window]
   [flow-gl.swing.window :as swing-window]
   [flow-gl.tools.trace :as trace]
   [fungl.cache :as cache]
   [fungl.component :as component]
   [fungl.depend :as depend]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.id-comparator :as id-comparator]
   [fungl.layout :as layout]
   [fungl.node-image-cache :as node-image-cache]
   [fungl.renderer :as renderer]
   [fungl.swing.root-renderer :as swing-root-renderer]
   [fungl.util :as util]
   [fungl.view-compiler :as view-compiler]
   [logga.core :as logga]
   [taoensso.tufte :as tufte]
   [fungl.layouts :as layouts]
   [fungl.component.text-area :as text-area]

   [flow-gl.graphics.font :as font]
   [flow-gl.gui.visuals :as visuals]
   [clojure.string :as string]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(def font (font/create-by-name "CourierNewPSMT" 40))
(def bold-font (font/create-by-name "CourierNewPS-BoldMT" 40))

(defn text [string & [{:keys [font color] :or {font font
                                               color [255 255 255 255]}}]]
  (text-area/text (str string)
                  color
                  font))

(defn property [label value]
  (layouts/horizontally-2 {:margin 20}
                          (text label
                                {:font bold-font
                                 :color [100 200 100 255]})
                          (text value)))

(defn development-tools-view [scene-graph]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color [50 50 50 220])
               (layouts/vertically-2 {:margin 20}
                                     (property "view functions:"
                                               (string/join " " (map util/function-name (remove nil? (map :view-function (scene-graph/path-to scene-graph
                                                                                                                                                            (:focused-node-id @keyboard/state-atom)))))))
                                     (property "command sets:"
                                               (string/join " " (map pr-str (remove nil? (map :name (map :command-set (scene-graph/path-to scene-graph
                                                                                                                                           (:focused-node-id @keyboard/state-atom))))))))
                                     (property "focused node id:"
                                               (:focused-node-id @keyboard/state-atom))

                                     (property "focused keyboard event handler:"
                                               (util/function-name (first (:keyboard-event-handler (:focused-node @keyboard/state-atom))))))))

(defn add-development-tools [application-loop-state-atom scene-graph]
  (if false #_(:show-development-tools? @application-loop-state-atom) ;; TODO: development tools should be added to the scene graph before the layout to make layout cache work
      (let [development-tools-layout (layout/layout-scene-graph (view-compiler/compile-view-calls (development-tools-view scene-graph))
                                                                (:window-width @application-loop-state-atom)
                                                                (:window-height @application-loop-state-atom))]
        {:children [scene-graph
                    (assoc development-tools-layout
                           :y (- (:window-height @application-loop-state-atom)
                                 (:height development-tools-layout)))]
         :x 0
         :y 0
         :width (:window-width @application-loop-state-atom)
         :height (:window-height @application-loop-state-atom)})
      scene-graph))
