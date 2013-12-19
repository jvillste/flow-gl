(defproject flow-gl "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.lwjgl/lwjgl "2.7.1"]
                 [org.lwjgl/lwjgl-util "2.7.1"]
                 [org.lwjgl/lwjgl-native-platform "2.7.1"]
                 [slingshot "0.10.3"]
                 [org.clojure/data.priority-map "0.0.2"]
                 [com.datomic/datomic-free "0.8.4020.24"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [org.clojure/tools.namespace "0.2.4"]
                 [midje "1.5.1"]
                 [org.clojars.toxi/jogl "2.0.0-rc11"]]

  ;; :java-cmd "/usr/lib/jvm/java-6-sun/bin/java"
  :java-opts ["-Xmx60m"
              "-Xms40m"
              "-client"
              ;; "-XX:MinHeapFreeRatio=20"
              "-XX:MaxHeapFreeRatio=50"]
  :plugins [[lein-midje "3.0.0"]])
