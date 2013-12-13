(defproject org.craigandera/dynne "0.4.0-SNAPSHOT"
  :description "A library for working with audio"
  :url "https://github.com/candera/dynne"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [incanter "1.5.2"]
                 [com.googlecode.soundlibs/mp3spi "1.9.5-1"]
                 [primitive-math "0.1.3"]
                 [hiphip-aot "0.1.1"]]
  :java-source-paths ["src/java"]
  :source-paths ["src/clj"]
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/tools.namespace "0.2.4"]]
              ;;               :jvm-opts ^:replace ["-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=9900"]
              :jvm-opts ^:replace []
              }})
