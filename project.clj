(defproject market-simulation "0.1.0-SNAPSHOT"
  :description "Simple market simulation"
  :url "https://github.com/Guthur/market-simulation"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.9.0"]
                 [org.clojure/test.check "0.7.0"]]
  :main ^:skip-aot market-simulation.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
