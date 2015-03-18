(defproject flow-gl "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :dependencies [[net.mikera/core.matrix "0.33.2"]
                 [com.taoensso/timbre "3.3.1"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.trace "0.7.6"]
                 [org.lwjgl/lwjgl "2.7.1"]
                 [org.lwjgl/lwjgl-util "2.7.1"]
                 [org.lwjgl/lwjgl-native-platform "2.7.1"]
                 [slingshot "0.10.3"]
                 [org.clojure/data.priority-map "0.0.2"]
                 [com.datomic/datomic-free "0.8.4020.24"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [org.clojure/tools.namespace "0.2.4"]
                 [midje "1.6.3"]
                 [clj-http "0.9.2"]
                 ;;[org.clojars.toxi/jogl "2.0.0-rc11"]
                 ;;[org.clojars.jvillste/jogl-all "2.1.3-rc-20131212"]
                 [org.clojars.jvillste/jogl-all "2.1.5"]
                 ;;[gui-diff "0.6.6"]
                 [com.climate/claypoole "0.3"]
                 [org.clojars.jvillste/nanovg "1.0.0-SNAPSHOT"]]

  ;; :java-cmd "/usr/lib/jvm/java-6-sun/bin/java"
   ;;/Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java
  ;;:javac-target "1.7"
  #_:java-opts #_["-Xmx60m"
              "-Xms40m"
              "-client"
              ;; "-XX:MinHeapFreeRatio=20"
              "-XX:MaxHeapFreeRatio=50"
              "-Dsun.awt.noerasebackground=true"]
  :plugins [[lein-midje "3.0.0"]])
