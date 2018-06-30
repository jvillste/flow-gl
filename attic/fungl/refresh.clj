(ns fungl.refresh
  (:require [clojure.tools.namespace.repl :as repl]))

(defn refresh []
  (repl/set-refresh-dirs "src/fungl"
                         "src/flow_gl/gui/tiled_renderer.clj"
                         "src/flow_gl/gui/quad_renderer.clj"
                         "src/flow_gl/gui/renderer.clj"
                         "src/flow_gl/gui/stateful.clj"
                         "src/examples/hi_tiled_rendering.clj")
  (repl/refresh))

(defn start []
  (refresh))

