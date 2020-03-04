(defproject flow-gl "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :resource-paths ["resources"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.openjfx/javafx-controls "13.0.2"]
                 [org.openjfx/javafx-swing    "13.0.2"]
                 [org.openjfx/javafx-media    "13.0.2"]
                 [org.openjfx/javafx-fxml     "13.0.2"]
                 [org.openjfx/javafx-web      "13.0.2"]
                 [org.openjfx/javafx-base      "13.0.2"]
                 [org.openjfx/javafx-graphics "13.0.2"]
                 [net.mikera/core.matrix "0.33.2"]
                 [com.taoensso/timbre "4.7.3"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/core.async "0.4.474"]
                 [clj-http "2.2.0"]
                 [org.clojars.jvillste/jogl-all "2.4.0-SNAPSHOT"]
                 [com.google.guava/guava "20.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [logga "0.1.0-SNAPSHOT"]
                 [slamhound "1.5.5"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}
  :test-paths ["src" "test"])
