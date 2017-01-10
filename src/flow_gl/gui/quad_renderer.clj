(ns flow-gl.gui.quad-renderer
  (:require (flow-gl.gui [event-queue :as event-queue]
                         [layout :as layout]
                         [drawable :as drawable]
                         [layoutable :as layoutable]
                         [scene-graph :as scene-graph])
            (fungl [cache :as cache])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer]
                                 [quad-batch :as quad-batch])
            [flow-gl.opengl.math :as math]
            [clojure.data.priority-map :as priority-map]
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]
                              [native-buffer :as native-buffer]))
  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color])

  (:use clojure.test))

(defn keys-to-seq [source-map keys]
  (reduce (fn [result key]
            (conj result (key source-map)))
          []
          keys))

(defn image-function-parameters [quad]
  (-> []
      (cond-> (:width-dependent quad)
        (conj (:width quad)))
      (cond-> (:height-dependent quad)
        (conj (:height quad)))
      (concat (if-let [image-function-parameters (:image-function-parameter-keys quad)]
                (keys-to-seq quad
                             image-function-parameters)
                []))))

(defn texture-key [quad]
  (if-let [texture-hash (:texture-hash quad)]
    texture-hash
    (hash (concat [(:image-function quad)]
                  (image-function-parameters quad)))))

(defn unused-drawable-textures [drawable-textures quads]
  (reduce dissoc drawable-textures (map texture-key quads)))

(defn has-texture? [quad-renderer quad]
  (contains? (:drawable-textures quad-renderer)
             (:texture-key quad)))

(defn set-texture [drawable-textures texture-key texture-id]
  (assoc drawable-textures texture-key texture-id))

(defn unset-texture [drawable-textures key]
  (dissoc drawable-textures key))

(defn texture [drawable-textures quad]
  (get drawable-textures (texture-key quad)))

(defn new-quads [quad-renderer quads]
  (loop [quads quads
         new-quads []
         new-keys #{}]
    (if-let [quad (first quads)]
      (if (and (not (has-texture? quad-renderer
                                  quad))
               (not (contains? new-keys
                               (:texture-key quad))))
        (recur (rest quads)
               (conj new-quads quad)
               (conj new-keys
                     (:texture-key quad)))
        (recur (rest quads)
               new-quads
               new-keys))
      new-quads)))

(defn assert-size-is-not-infinite [quad]
  (when (= (:width quad)
           java.lang.Integer/MAX_VALUE)
    (throw (ex-info "Quad has infinite width." {:quad quad})))
  
  (when (= (:height quad)
           java.lang.Integer/MAX_VALUE)
    (throw (ex-info "Quad has infinite height." {:quad quad}))))

(defn create-textures [quads]
  (map (fn [quad]

         (-> (cond (:texture-id quad) (do (assert-size-is-not-infinite quad)
                                          (select-keys quad [:texture-id :width :height]))
                   (:image-function quad) (do (assert-size-is-not-infinite quad)
                                              {:image (apply (:image-function quad)
                                                             (image-function-parameters quad))}) 
                   :default nil)
             (assoc :texture-key (:texture-key quad))))
       quads))

(defn add-new-textures [drawable-textures texture-keys first-texture-id]
  (loop [texture-id first-texture-id
         drawable-textures drawable-textures
         texture-keys texture-keys]
    (if-let [texture-key (first texture-keys)]
      (recur (inc texture-id)
             (set-texture drawable-textures texture-key texture-id)
             (rest texture-keys))
      drawable-textures)))

(defn add-texture-keys [quads]
  (map (fn [quad]
         (assoc quad
                :texture-key (texture-key quad)))
       quads))

(defn load-new-textures [quad-renderer quads gl]
  (let [first-texture-id (:next-free-texture-id (:quad-batch quad-renderer))
        quads (add-texture-keys quads)
        quads (new-quads quad-renderer quads)
        new-textures (filter #(not (nil? %)) (create-textures quads))
        new-gl-textures (filter :texture-id new-textures)
        new-image-textures (filter :image new-textures)]

    (if (empty? new-textures)
      quad-renderer
      (assoc quad-renderer
             :quad-batch (-> (:quad-batch quad-renderer)
                             (cond-> (not (empty? new-image-textures))
                               (quad-batch/add-textures gl (map :image new-image-textures)))
                             (cond-> (not (empty? new-gl-textures))
                               (quad-batch/add-textures-from-gl-textures gl new-gl-textures)))
             :drawable-textures (add-new-textures (:drawable-textures quad-renderer)
                                                  (map :texture-key
                                                       (concat new-image-textures
                                                               new-gl-textures))
                                                  (:next-free-texture-id (:quad-batch quad-renderer)))))))


(defn add-gl-texture [quad-renderer quad texture-id width height gl]
  (assoc quad-renderer
         :quad-batch (quad-batch/add-textures-from-gl-textures (:quad-batch quad-renderer)
                                                               gl
                                                               [{:texture-id texture-id
                                                                 :width (int width)
                                                                 :height (int height)}])
         :drawable-textures (add-new-textures (:drawable-textures quad-renderer)
                                              [quad]
                                              (:next-free-texture-id (:quad-batch quad-renderer)))))

(defn add-texture-ids [quads drawable-textures]
  (map (fn [quad]
         (assoc quad
                :texture-id (texture drawable-textures
                                     quad)))
       quads))

(defn unload-unused-textures [quad-renderer]

  (let [unused-drawable-textures (unused-drawable-textures (:drawable-textures quad-renderer)
                                                           (:drawn-drawables quad-renderer))
        
        new-quad-batch (reduce quad-batch/remove-texture
                               (:quad-batch quad-renderer)
                               (vals unused-drawable-textures))
        
        new-drawable-textures (reduce unset-texture
                                      (:drawable-textures quad-renderer)
                                      (keys unused-drawable-textures))]


    (assoc quad-renderer
           :draws-after-garbage-collection 0
           :drawn-drawables []
           :quad-batch new-quad-batch
           :drawable-textures new-drawable-textures)))

(defn draw [quad-renderer quads width height gl]
  (let [quad-renderer (load-new-textures quad-renderer
                                         quads
                                         gl)
        quad-renderer (assoc quad-renderer
                             :drawn-drawables (concat (:drawn-drawables quad-renderer)
                                                      quads)

                             :quad-batch (quad-batch/draw-quads (:quad-batch quad-renderer)
                                                                gl
                                                                (filter :texture-id
                                                                        (add-texture-ids quads
                                                                                         (:drawable-textures quad-renderer)))
                                                                width height))
        quad-renderer (if (= (:draws-after-garbage-collection quad-renderer)
                             10)
                        (unload-unused-textures quad-renderer)
                        quad-renderer)]
    
    (-> quad-renderer
        (update-in [:draws-after-garbage-collection] (fnil inc 0))))
  #_(taoensso.timbre.profiling/p :draw
                                 ))

(defn draw-scene-graph [state gl scene-graph]
  #_(opengl/clear gl 0 0 0 0)

  (let [{:keys [width height]} (opengl/size gl)]
    (draw state
          (filter (fn [node]
                    (scene-graph/intersects? {:x 0 :y 0 :width width :height height}
                                             node))
                  (cache/call! scene-graph/leaf-nodes scene-graph))
          width
          height
          gl)))

(defn render [state-atom gl scene-graph]
  (swap! state-atom
         draw-scene-graph gl scene-graph)
  {})

(defn initialize-state [gl]
  (println "creating quad renderer")
  {:drawable-textures {}
   :drawn-drawables []
   :quad-batch (quad-batch/create gl)})


(defn stateful [gl]
  {:initialize-state (partial initialize-state gl)

   :delete-state (fn [state]
                   (quad-batch/delete (:quad-batch state) gl))})

(defn atom-specification [gl]
  {:create (partial initialize-state gl)
   :delete (fn [state-atom]
             (println "deleting quad renderer")
             (quad-batch/delete (:quad-batch @state-atom) gl))})

;; dynamic state

(def ^:dynamic state-atom)

(defn state-bindings [gl]
  {#'state-atom (atom (initialize-state gl))})

(defn draw! [quads width height gl]
  (swap! state-atom draw quads width height gl))


