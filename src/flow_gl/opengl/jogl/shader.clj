(ns flow-gl.opengl.jogl.shader
  (:require [flow-gl.graphics.native-buffer :as native-buffer])
  (:import [javax.media.opengl GL2]))

;; gl.glGetProgramiv(shaderprogram, GL.GL_LINK_STATUS,intBuffer);
;;       if (intBuffer.get(0)!=1){
;;          gl.glGetProgramiv(shaderprogram, GL.GL_INFO_LOG_LENGTH,intBuffer);
;;          int size = intBuffer.get(0);
;;          System.err.println("Program link error: ");
;;          if (size>0){
;;             ByteBuffer byteBuffer = ByteBuffer.allocate(size);
;;             gl.glGetProgramInfoLog(shaderprogram, size, intBuffer, byteBuffer);
;;             for (byte b:byteBuffer.array()){
;;                System.err.print((char)b);
;;             }
;;          } else {
;;             System.out.println("Unknown");
;;          }
;;          System.exit(1);
;;       }
;;       gl.glUseProgram(shaderprogram);

(defn compile-errors [gl shader-id]
    (let [result-buffer (native-buffer/native-buffer :int 1)]
      (.glGetShaderiv gl (int shader-id) (int GL2/GL_INFO_LOG_LENGTH) result-buffer)
      (if (> (.get result-buffer 0)
             0)
        (let [byte-result-buffer (byte-array (.get result-buffer 0))]
          (.glGetShaderInfoLog gl shader-id (.get result-buffer 0) nil  0 byte-result-buffer 0)
          (String. byte-result-buffer))
        "")))

(defn compile-shader [gl shader-id source]
  (.glShaderSource gl shader-id 1 (into-array [source]) nil)
  (.glCompileShader gl shader-id)
  (when (> (count (compile-errors gl shader-id))
             0)
      (throw (Exception. (compile-errors gl shader-id)))))

(defn create-vertex-shader [gl]
  (.glCreateShader gl GL2/GL_VERTEX_SHADER))

(defn create-fragment-shader [gl]
  (.glCreateShader gl GL2/GL_FRAGMENT_SHADER))

(defn create-program [gl vertex-shader-id fragment-shader-id]
  (let [program-id (.glCreateProgram gl)]
    (.glAttachShader gl program-id vertex-shader-id)
    (.glAttachShader gl program-id fragment-shader-id)
    (.glLinkProgram gl program-id)
    (.glValidateProgram gl program-id)
    #_(when (> (count (compile-errors gl program-id))
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
  (.glUseProgram gl program-id))

(defn disable-program [gl]
  (enable-program gl 0))

(defn delete-program [gl program-id]
  (.glDeleteShader gl program-id))
