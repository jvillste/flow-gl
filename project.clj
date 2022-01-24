(defproject flow-gl "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :resource-paths ["resources"]
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [net.mikera/core.matrix "0.33.2"]
                 [com.taoensso/timbre "4.7.3"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [org.clojure/core.async "1.3.610"]
                 [clj-http "3.7.0"]
                 [org.clojars.jvillste/jogl-all "2.3.2"]
                 [com.google.guava/guava "20.0"]
                 [org.clojure/tools.namespace "1.1.0"]
                 [logga "0.1.0-SNAPSHOT"]
                 [slamhound "1.5.5"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}
  :test-paths ["src" "test"])
