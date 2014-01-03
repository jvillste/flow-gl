(ns flow-gl.opengl.jogl.shader
  (:import [javax.media.opengl GL2]))

(defn compile-errors [gl shader-id]
  (.glGetInfoLog gl shader-id 1000))

(defn compile-shader [gl shader-id source]
  (.glShaderSource gl shader-id source)
  (.glCompileShader gl shader-id)
  (when (> (count (compile-errors gl shader-id))
           0)
    (throw (Exception. (compile-errors gl shader-id)))))

(defn create-vertex-shader [gl]
  (.glCreateShaderObject gl GL2/GL_VERTEX_SHADER))

(defn create-fragment-shader [gl]
  (.glCreateShaderObject gl GL2/GL_FRAGMENT_SHADER))

(defn create-program [gl vertex-shader-id fragment-shader-id]
  (let [program-id (.glCreateProgramObject gl)]
    (.glAttachObject gl program-id vertex-shader-id)
    (.glAttachObject gl program-id fragment-shader-id)
    (.glLinkProgram gl program-id)
    (.glValidateProgram gl program-id)
    (when (> (count (compile-errors gl program-id))
             0)
      (throw (Exception. (str "Error when creating shader program: " (compile-errors program-id)))))
    program-id))

(defn get-uniform-location [gl program name]
  (.glGetUniformLocation gl program name))

(defn set-float-uniform [gl program name value]
  (.glUniform1f gl (get-uniform-location gl program
                                         name)
                value))

(defn set-float3-uniform [gl program name value1 value2 value3]
  (.glUniform3f gl (get-uniform-location gl
                                         program
                                         name)
                value1
                value2
                value3))

(defn compile-program [gl vertex-shader-source fragment-shader-source]
  (let [vertex-shader-id (create-vertex-shader gl)
        fragment-shader-id (create-fragment-shader gl)]

    (try (compile-shader gl vertex-shader-id vertex-shader-source)
         (catch Exception exception
           (throw (Exception. (str "Error when compiling vertex shader: " exception)))))

    (try (compile-shader gl fragment-shader-id fragment-shader-source)
         (catch Exception exception
           (throw (Exception. (str "Error when compiling fragment shader: " exception)))))

    (create-program gl
                    vertex-shader-id
                    fragment-shader-id)))

(defn enable-program [gl program-id]
  (.glUseProgramObject gl program-id))

(defn disable-program [gl]
  (enable-program gl 0))

(defn delete-program [gl program-id]
  (.glDeleteObject gl program-id))
