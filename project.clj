(defproject csi "0.1.9"
 :description "ClojureScript interface to Erlang"
 :url "https://github.com/suprematic/csi.client"
 :license {:name "Eclipse Public License"
           :url "http://www.eclipse.org/legal/epl-v10.html"}

 :plugins [[lein-cljsbuild "1.0.3"]
           [lein-ancient "0.6.7"]]

 :hooks  [leiningen.cljsbuild]

 :dependencies [[org.clojure/clojure "1.7.0"]
                [org.clojure/clojurescript "1.7.122"]
                [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                [shodan "0.4.2"]
                [jarohen/chord "0.6.0"]]


 :source-paths  ["src/cljs"]

 :cljsbuild {:builds  [{:id "dev"
                        :source-paths ["src/cljs"]
                        :compiler {:output-dir "static/js/compiled"
                                   :output-to  "static/js/compiled/csi.js"
                                   :source-map "static/js/compiled/csi.js.map"
                                   :optimizations :none
                                   :pretty-print true}}]})
