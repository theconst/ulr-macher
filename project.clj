(defproject url-matcher "0.1.0-SNAPSHOT"
  :description "Test program which does something useful. GLHF"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]

                 [clj-antlr "0.2.5"]

                 [org.clojure/tools.cli "0.4.2"]

                 [org.clojure/tools.logging "0.5.0"]
                 [ch.qos.logback/logback-classic "1.2.3"]]
  :main ^:skip-aot url-matcher.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[proto-repl "0.3.1"]
                                  [proto-repl-charts "0.3.1"]]}}
  :global-vars {*warn-on-reflection* true}
  :plugins [[lein-cljfmt "0.6.4"]
            [jonase/eastwood "0.3.5"]
            [lein-codox "0.10.7"]])
