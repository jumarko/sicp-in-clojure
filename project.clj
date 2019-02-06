(defproject sicp-in-clojure "0.1.0-SNAPSHOT"
  :description "Examples for the Structure and Interpreation of Computer Programs book written in Clojure"
  :url "https://github.com/jumarko/sicp-in-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot sicp-in-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
