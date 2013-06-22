(defproject org.craigandera/dynne "0.2.0"
  :description "A library for working with audio"
  :url "https://github.com/candera/dynne"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter "1.4.0"]
                 [com.googlecode.soundlibs/mp3spi "1.9.5-1"]
                 [primitive-math "0.1.2"]]
  :java-source-paths ["src/java"]
  :source-paths ["src/clj"]
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/tools.namespace "0.2.3"]]
              :jvm-opts ^:replace ["-Xdebug"
                                   "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=9900"]}})
