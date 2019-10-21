(defproject krulak "0.7.0-SNAPSHOT"
  :description "krulak is a web utility library for Clojure."
  :url "https://github.com/john-shaffer/krulak"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [cheshire "5.9.0"]
                 [clj-http "3.10.0"]
                 [ring/ring-codec "1.1.2"]]
  :deploy-repositories [["releases" :clojars
                         "snapshots" :clojars]]
  :repl-options {:init-ns krulak})
