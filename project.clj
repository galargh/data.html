(defproject org.clojars.gfjalar/data.html "0.1.1"
  :description "Library for HTML data tokenization."
  :url "https://github.com/gfjalar/data.html/"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.namespace "1.1.0"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.1.0"]]
  :plugins [[lein-zprint "1.1.1"]]
  :zprint {:old? false}
  :repl-options {:init-ns data.html})
