(defproject crypto "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "2.3.0"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [net.mikera/core.matrix "0.57.0"]
                 [net.mikera/vectorz-clj "0.45.0"]]


  :profiles {:dev {:dependencies [[proto-repl "0.3.1"]]}})

