(ns flow-gl.opengl.jogl.jogl
  (:import [com.jogamp.newt.event WindowAdapter WindowEvent]
           [com.jogamp.newt.opengl GLWindow]
           [javax.media.opengl GLCapabilities GLProfile]))


(let [gl-profile (GLProfile/getDefault)
      gl-capabilities (GLCapabilities. gl-profile)
      window (GLWindow/create gl-capabilities)]
  (.setSize window 300 300)
  (.setVisible window true)
  (.addWindowListener window (proxy [WindowAdapter] []
                        (windowDestroyNotify [e]
                          (println "close")))))

;; GLProfile glp = GLProfile.getDefault();
;;        GLCapabilities caps = new GLCapabilities(glp);

;;        GLWindow window = GLWindow.create(caps);
;;        window.setSize(300, 300);
;;        window.setVisible(true);
;;        window.setTitle("NEWT Window Test");

;;        window.addWindowListener(new WindowAdapter() {
;;            public void windowDestroyNotify(WindowEvent arg0) {
;;                System.exit(0);
;;            };
;;        });
