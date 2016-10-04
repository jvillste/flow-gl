(ns flow-gl.gui.quad-renderer
  (:require (flow-gl.gui [event-queue :as event-queue]
                         [layout :as layout]
                         [drawable :as drawable]
                         [layoutable :as layoutable])
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
      (concat (if-let [image-function-parameters (:image-function-parameters quad)]
                (keys-to-seq quad
                             image-function-parameters)
                []))))

(defn texture-key [quad]
  (hash (concat [(:image-function quad)]
                (image-function-parameters quad))))

(defn unused-drawable-textures [drawable-textures quads]
  (reduce dissoc drawable-textures (map texture-key quads)))

(defn has-texture? [quad-renderer quad]
  (contains? (:drawable-textures quad-renderer)
             (:texture-key quad)))

(defn set-texture [drawable-textures quad texture-id]
  (assoc drawable-textures (texture-key quad) texture-id))

(defn unset-texture [drawable-textures quad]
  (dissoc drawable-textures (texture-key quad)))

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

(defn create-textures [quads]
  (map (fn [quad]
         (when (= (:width quad)
                  java.lang.Integer/MAX_VALUE)
           (throw (ex-info "Quad has infinite width." {:quad quad})))
         
         (when (= (:height quad)
                  java.lang.Integer/MAX_VALUE)
           (throw (ex-info "Quad has infinite height." {:quad quad})))
         
         (apply (:image-function quad)
                (image-function-parameters quad)))
       quads))

(defn add-new-textures [drawable-textures quads first-texture-id]
  (loop [texture-id first-texture-id
         drawable-textures drawable-textures
         quads quads]
    (if-let [quad (first quads)]
      (recur (inc texture-id)
             (set-texture drawable-textures quad texture-id)
             (rest quads))
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
        new-textures (create-textures quads)]
    (if (empty? new-textures)
      quad-renderer
      (assoc quad-renderer
             :quad-batch (quad-batch/add-textures (:quad-batch quad-renderer) gl new-textures)
             :drawable-textures (add-new-textures (:drawable-textures quad-renderer)
                                                  quads
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
           :drawn-drawables []
           :quad-batch new-quad-batch
           :drawable-textures new-drawable-textures)))

(defn draw [quad-renderer quads width height gl]
  (let [quad-renderer (load-new-textures quad-renderer
                                         quads
                                         gl)]

    (assoc quad-renderer
           :drawn-drawables (concat (:drawn-drawables quad-renderer)
                                    quads)

           :quad-batch (quad-batch/draw-quads (:quad-batch quad-renderer)
                                              gl
                                              (add-texture-ids quads
                                                               (:drawable-textures quad-renderer))
                                              width height))))



(defn create [gl]
  {:drawable-textures {}
   :drawn-drawables []
   :quad-batch (quad-batch/create gl)})

