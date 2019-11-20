(defproject krulak "0.9.0-SNAPSHOT"
  :description "krulak is a web utility library for Clojure."
  :url "https://github.com/john-shaffer/krulak"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.cache "0.8.2"]
                 [org.clojure/core.memoize "0.8.2"]
                 [cheshire "5.9.0"]
                 [clj-http "3.10.0"]
                 [clj-time "0.15.2"]
                 [medley "1.2.0"]
                 [ring/ring-anti-forgery "1.3.0"]
                 [ring/ring-codec "1.1.2"]
                 [ring "1.7.1"]
                 [ring-middleware-format "0.7.4"]]
  :deploy-repositories [["releases" :clojars
                         "snapshots" :clojars]]
  :repl-options {:init-ns krulak})
