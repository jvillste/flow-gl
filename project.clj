(defproject flow-gl "3"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :resource-paths ["resources"]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [medley "1.3.0"]
                 [net.mikera/core.matrix "0.33.2"]
                 [com.taoensso/tufte "2.3.0-RC1"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/core.async "1.5.648"]
                 [clj-http "3.7.0"]
                 #_[org.clojars.jvillste/jogl-all "2.3.2"]
                 [com.google.guava/guava "20.0"]
                 #_[org.clojure/tools.namespace "1.1.0"]
                 [logga "1"]
                 [slamhound "1.5.5"]
                 [org.clojure/test.check "1.1.1"]
                 [com.clojure-goes-fast/clj-async-profiler "1.1.1"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}
  :test-paths ["src" "test"])
