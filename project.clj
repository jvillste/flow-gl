(defproject flow-gl "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix "0.33.2"]
                 [com.taoensso/timbre "3.3.1"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/core.async "0.2.391"]
                 [clj-http "2.2.0"]
                 [org.clojars.jvillste/jogl-all "2.1.5"]])
