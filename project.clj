(defproject com.phronemophobic/treemap-clj "0.2.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.764"]
                 [com.phronemophobic/membrane "0.9.11-beta"]
                 [com.github.davidmoten/rtree "0.8.7"]]
  :main ^:skip-aot treemap-clj.core
  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild {:builds
              [{:id "explore"
                :source-paths ["src"]
                :figwheel {:on-jsload "membrane.explore/on-js-reload"}
                :compiler {:main treemap-clj.core
                           :asset-path "js/compiled/out.explore"
                           :output-to "resources/public/js/compiled/explore.js"
                           :output-dir "resources/public/js/compiled/out.explore"
                           :source-map-timestamp true
                           ;; :optimizations :advanced
                           :externs ["resources/public/js/opentype.externs.js"]
                           }}]}
  ;; :java-cmd "/Library/Java/JavaVirtualMachines/jdk1.8.0_192.jdk/Contents/Home/bin/java"
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/data.json "1.0.0"]
                                  [com.vladsch.flexmark/flexmark-all "0.62.2"]
                                  [hiccup "1.0.5"]
                                  [org.clojure/tools.namespace "1.0.0"]]}
             :provided {:dependencies [[org.clojure/test.check "0.9.0"]
                                       ]}})
