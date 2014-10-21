(ns flow-gl.opengl.jogl.render-target
  (:require [flow-gl.gui.event-queue :as event-queue]
            (flow-gl.opengl.jogl [frame-buffer :as frame-buffer]
                                 [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer]
                                 [quad-batch :as quad-batch]
                                 [vertex-array-object :as vertex-array-object])
            [flow-gl.opengl.math :as math]
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]
                              [native-buffer :as native-buffer]))

  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color]))


  (def vertex-shader-source "
  #version 140
  uniform mat4 projection_matrix;

  in vec2 vertex_coordinate_attribute;

  out vec2 texture_coordinate;

  void main() {
  gl_Position = projection_matrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);

  switch(gl_VertexID) {
  case 0:
  texture_coordinate = vec2(0.0, 0.0);
  break;
  case 1:
  texture_coordinate = vec2(0.0, 1.0);
  break;
  case 2:
  texture_coordinate = vec2(1.0, 0.0);
  break;
  case 3:
  texture_coordinate = vec2(1.0, 1.0);
  break;
  }

  }

")

  (def fragment-shader-source "
  #version 140

  uniform sampler2D texture;
  in vec2 texture_coordinate;
  out vec4 outColor;

  void main() {
  outColor = texture(texture, texture_coordinate);
  //outColor = texture(texture, vec2(texture_coordinate[0], texture_coordinate[1]));
  //outColor = vec4(texture(texture, texture_coordinate)[2], 0.0, 0.0, 1.0);
  //outColor = texture(texture, vec2(0.5, 0.5));
  
  }
")

  

(defn text-image [text]
  (text/create-buffered-image [1 1 1 1]
                              (font/create "LiberationSans-Regular.ttf" 14)
                              text))

(defn quad [width height]
  [0   0
   0   height
   width 0
   width height])

(defn quad-2 [from to]
  [from   from
   from   to
   to from
   to to])

(defn quad-3 [x1 y1 x2 y2]
  [x1 y1
   x1 y2
   x2 y1
   x2 y2])

(defn draw-quad [gl texture quad-width quad-height frame-buffer-width frame-buffer-height]
  (let [shader-program (shader/compile-program gl
                                               vertex-shader-source
                                               fragment-shader-source)
        vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")
        vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)]

    (shader/enable-program gl
                           shader-program)

    (.glActiveTexture gl GL2/GL_TEXTURE0)
    (.glBindTexture gl GL2/GL_TEXTURE_2D texture)
    (.glUniform1i gl (.glGetUniformLocation gl shader-program "texture") 0)

    (shader/set-float4-matrix-uniform gl
                                      shader-program
                                      "projection_matrix"
                                      (math/projection-matrix-2d frame-buffer-width
                                                                 frame-buffer-height))

    (buffer/load-vertex-array-buffer gl
                                     vertex-coordinate-buffer-id
                                     :float
                                     #_(map float (quad quad-width
                                                        quad-height))
                                     (map float (quad-3 0 0 quad-width quad-height)))


    (let [vertex-array-object (vertex-array-object/create gl)]
      (vertex-array-object/bind gl vertex-array-object)

      (.glBindBuffer gl GL2/GL_ARRAY_BUFFER vertex-coordinate-buffer-id)
      (.glEnableVertexAttribArray gl vertex-coordinate-attribute-index)
      (.glVertexAttribPointer gl
                              (int vertex-coordinate-attribute-index)
                              (int 2)
                              (int GL2/GL_FLOAT)
                              (boolean GL2/GL_FALSE)
                              (int 0)
                              (long 0))

      (.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 4)

      (vertex-array-object/bind gl 0)
      (vertex-array-object/delete gl vertex-array-object))

    (.glBindTexture gl GL2/GL_TEXTURE_2D 0)
    (buffer/delete gl vertex-coordinate-buffer-id)
    (shader/delete-program gl shader-program)))


(defn create-texture [gl]
  (let [texture (texture/create-gl-texture gl)]
    (.glBindTexture gl GL2/GL_TEXTURE_2D texture)

    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)

    texture))

(defn load-texture [gl texture width height data]
  (.glBindTexture gl GL2/GL_TEXTURE_2D texture)
  (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA8 width height 0 GL2/GL_BGRA GL2/GL_UNSIGNED_BYTE data))

(defn load-texture-from-buffered-image [gl texture image]
  (load-texture gl texture  (.getWidth image) (.getHeight image)
                (native-buffer/native-buffer-with-values :int (-> image (.getRaster) (.getDataBuffer) (.getData)))))

(defn create-checker-texture [gl]
  (let [texture (create-texture gl)
        data (native-buffer/native-buffer-with-values :byte [0 255 0 255 0 255 0 255
                                                             255 0 255 0 255 0 255 0
                                                             0 255 0 255 0 255 0 255
                                                             255 0 255 0 255 0 255 0
                                                             0 255 0 255 0 255 0 255
                                                             255 0 255 0 255 0 255 0
                                                             0 255 0 255 0 255 0 255
                                                             255 0 255 0 255 0 255 0])]

    (.glBindTexture gl GL2/GL_TEXTURE_2D texture)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)
    (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_R8 8 8 0 GL2/GL_RED GL2/GL_UNSIGNED_BYTE data)
    
    texture))

(defn create-rgba-texture [gl]
  (let [texture (texture/create-gl-texture gl)
        data (native-buffer/native-buffer-with-values :byte [0 255 0 255
                                                             255 0 0 255])]
    
    (.glBindTexture gl GL2/GL_TEXTURE_2D texture)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_LINEAR)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)
    (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA 2 1 0 GL2/GL_RGBA GL2/GL_UNSIGNED_BYTE data)
    
    texture))


(defn create [width height gl]
  (let [vertex-array-object (vertex-array-object/create gl)
        frame-buffer (frame-buffer/create gl)
        frame-buffer-texture (texture/create-gl-texture gl)]

    (.glBindTexture gl GL2/GL_TEXTURE_2D frame-buffer-texture)

    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_LINEAR)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_LINEAR)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)

    (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA (int width) (int height) 0 GL2/GL_RGBA GL2/GL_UNSIGNED_BYTE nil)


    (frame-buffer/bind frame-buffer gl)

    (frame-buffer/bind-texture frame-buffer-texture
                               gl)

    (.glDrawBuffers gl 1 (int-array [GL2/GL_COLOR_ATTACHMENT0]) 0)

    (assert GL2/GL_FRAMEBUFFER_COMPLETE
            (.glCheckFramebufferStatus gl GL2/GL_FRAMEBUFFER))

    (frame-buffer/bind 0 gl)

    {:frame-buffer frame-buffer
     :texture frame-buffer-texture
     :width width
     :height height}))

(defn start-rendering [render-target gl]
  (frame-buffer/bind (:frame-buffer render-target)
                     gl))

(defn end-rendering [render-target gl]
  (frame-buffer/bind 0
                     gl))

(defn draw [render-target width height gl]
  #_(draw-single-color-quad gl 10 10 width height)
  (draw-quad gl
             (:texture render-target)
             (:width render-target)
             (:height render-target)
             width
             height))

(defn delete [render-target gl]
  (frame-buffer/delete (:frame-buffer render-target) gl)
  (texture/delete-gl-texture (:texture render-target) gl))

#_(defn start []
  (let [window (window/create 600
                              600
                              :profile :gl3
                              :close-automatically true
                              :init opengl/initialize
                              )]

    (try
      (window/set-display window gl
                          (println "drawing")
                          (let [{:keys [width height]} (opengl/size gl)
                                ;;image (buffered-image/create-from-file "pumpkin.png")
                                ;;image-texture (texture/create-gl-texture gl) #_(create-texture (.getWidth image) (.getHeight image) gl)

                                render-target-width 500 #_128
                                render-target-height 500 #_128
                                #_render-target #_(create render-target-width
                                                      render-target-height
                                                      gl)
                                #_quad-batch #_(-> (quad-batch/create gl)
                                                   (quad-batch/add-textures gl [(buffered-image/create-from-file "pumpkin.png")]))
                                checker-texture #_(create-checker-texture gl) (create-rgba-texture gl)]

                            #_(load-texture-from-buffered-image gl image-texture  image)
                            #_(load-texture gl image-texture
                                            (.getWidth image)
                                            (.getHeight image)
                                            (native-buffer/native-buffer-with-values :byte
                                                                                     (repeat (* (.getWidth image)
                                                                                                (.getHeight image)
                                                                                                4) 200)))

                            #_(start-rendering render-target gl)

                            (opengl/clear gl 0 1 1 1)

                            #_(quad-batch/draw-quads quad-batch
                                                     gl
                                                     [{:x 0
                                                       :y 0
                                                       :texture-id 0}]
                                                     width
                                                     height)
                            #_(draw-quad gl
                                         image-texture
                                         (.getWidth image)
                                         (.getHeight image)
                                         width height
                                         #_(:width render-target)
                                         #_(:height render-target))

                            (draw-quad gl
                                       checker-texture
                                       80 80
                                       width height
                                       #_(:width render-target)
                                       #_(:height render-target))

                            #_(draw-single-color-quad gl 0 0 render-target-width render-target-height)

                            #_(end-rendering render-target gl)

                            #_(opengl/clear gl 0 0 0 1)

                            #_(draw render-target width height gl)

                            (delete render-target gl)))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))

(defn start []
  (let [window (window/create 600
                              600
                              :profile :gl3
                              :close-automatically true
                              :init opengl/initialize
                              )]

    (try
      (window/set-display window gl

                          (let [{:keys [width height]} (opengl/size gl)
                                image (buffered-image/create-from-file "pumpkin.png")
                               texture (create-texture gl)]
                            (load-texture gl texture (.getWidth image) (.getHeight image)
                                          (native-buffer/native-buffer-with-values :int (-> image (.getRaster) (.getDataBuffer) (.getData))))

                            (opengl/clear gl 0 1 1 1)

                            (draw-quad gl
                                       texture
                                       100 100
                                       width height)))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))
