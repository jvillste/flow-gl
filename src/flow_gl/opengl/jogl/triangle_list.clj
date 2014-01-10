(ns flow-gl.opengl.jogl.triangle-list
  (:require (flow-gl.opengl.jogl [shader :as shader]
                                 [buffer :as buffer]))
  (:import [javax.media.opengl GL2]
           [com.jogamp.common.nio Buffers]))

(defrecord TriangleList [mode
                         number-of-triangles
                         shader-program
                         vertex-coordinate-attribute-index
                         ertex-color-attribute-index
                         vertex-coordinate-buffer-id
                         vertex-coordinate-buffer
                         vertex-color-buffer-id
                         vertex-color-buffer])

(def vertex-shader-source "
#version 120

attribute vec2 vertex_coordinate_attribute;
attribute vec4 vertex_color_attribute;

varying vec4 color;

void main() {
    gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
    color = vertex_color_attribute;
}

")

(def fragment-shader-source "
#version 120

varying vec4 color;

void main() {
    gl_FragColor = color;
    

}
")

(defn update [triangle-list gl coordinates colors]
  (println "updating triangle list")
  (buffer/load-buffer gl
                      (:vertex-coordinate-buffer-id triangle-list)
                      :float
                      coordinates)

  (buffer/load-buffer gl
                      (:vertex-color-buffer-id triangle-list)
                      :float
                      colors)

  (assoc triangle-list
    :number-of-triangles (/ (count coordinates)
                            2
                            3)))

(def shader-program-atom (atom nil))

(defn create-shared-resources [gl]
  (reset! shader-program-atom (shader/compile-program gl
                                                      vertex-shader-source
                                                      fragment-shader-source)))

(defn delete-shared-resources [gl]
  (shader/delete-program gl @shader-program-atom)
  (reset! shader-program-atom nil))

(defn create [gl mode]
  (println "creating triangle list")
  (map->TriangleList {:mode mode
                      :vertex-coordinate-attribute-index (.glGetAttribLocation gl @shader-program-atom "vertex_coordinate_attribute")
                      :vertex-color-attribute-index (.glGetAttribLocation gl @shader-program-atom "vertex_color_attribute")
                      :vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)
                      :vertex-color-buffer-id (buffer/create-gl-buffer gl)}))

(defn create-for-coordinates [gl mode coordinates colors]
  (-> (create gl mode)
      (update gl
              coordinates
              colors)))

(defn delete [gl triangle-list]
  (buffer/delete gl (:vertex-coordinate-buffer-id triangle-list))
  (buffer/delete gl (:vertex-color-buffer-id triangle-list))
  triangle-list)


(defn render [gl triangle-list]
  (println "rendering " triangle-list "shader program " @shader-program-atom)
  (shader/enable-program gl @shader-program-atom)

  ;; gl2.glEnableClientState( GL2.GL_VERTEX_ARRAY );
  ;; gl2.glEnableClientState( GL2.GL_COLOR_ARRAY );
  ;; gl2.glVertexPointer( 3, GL.GL_FLOAT, 6 * Buffers.SIZEOF_FLOAT, 0 );
  ;; gl2.glColorPointer( 3, GL.GL_FLOAT, 6 * Buffers.SIZEOF_FLOAT, 3 * Buffers.SIZEOF_FLOAT );
  ;; gl2.glPolygonMode( GL.GL_FRONT, GL2.GL_FILL );
  ;; gl2.glDrawArrays( GL2.GL_QUADS, 0, aiNumOfVertices[0] );

  ;; // disable arrays once we're done
  ;; gl2.glBindBuffer( GL.GL_ARRAY_BUFFER, 0 );
  ;; gl2.glDisableClientState( GL2.GL_VERTEX_ARRAY );
  ;; gl2.glDisableClientState( GL2.GL_COLOR_ARRAY );


  ;;  (.glEnableClientState gl GL2/GL_VERTEX_ARRAY)
  ;;  (.glEnableClientState gl GL2/GL_COLOR_ARRAY)
  ;;  (.glVertexPointer gl (int 2) (int GL/GL_FLOAT) (int (* 2 Buffers/SIZEOF_FLOAT)) (long 0))

  (buffer/bind-buffer gl (:vertex-coordinate-buffer-id triangle-list))
  (.glVertexAttribPointer gl
                          (int (:vertex-coordinate-attribute-index triangle-list))
                          (int 2)
                          (int GL2/GL_FLOAT)
                          (boolean GL2/GL_FALSE)
                          (int 0)
                          (long 0))
  (.glEnableVertexAttribArray gl (:vertex-coordinate-attribute-index triangle-list))

  (buffer/bind-buffer gl (:vertex-color-buffer-id triangle-list))
  (.glVertexAttribPointer gl
                          (int (:vertex-color-attribute-index triangle-list))
                          (int 4)
                          (int GL2/GL_FLOAT)
                          (boolean GL2/GL_FALSE)
                          (int 0)
                          (long 0))
  (.glEnableVertexAttribArray gl (:vertex-color-attribute-index triangle-list))

  (case (:mode triangle-list)
    :triangles (.glDrawArrays gl GL2/GL_TRIANGLES 0 (* 3 (:number-of-triangles triangle-list)))
    :triangle-strip (.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 (+ 2 (:number-of-triangles triangle-list)))
    :triangle-fan (.glDrawArrays gl GL2/GL_TRIANGLE_FAN 0 (:number-of-triangles triangle-list)))

  (.glDisableVertexAttribArray gl (:vertex-coordinate-attribute-index triangle-list))
  (.glDisableVertexAttribArray gl (:vertex-color-attribute-index triangle-list))

  ;;(.glDisableClientState gl GL2/GL_VERTEX_ARRAY)
  ;;(.glDisableClientState gl GL2/GL_COLOR_ARRAY)

  triangle-list)
