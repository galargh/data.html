(defproject data.html "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.namespace "1.1.0"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.1.0"]]
  :plugins [[lein-zprint "1.1.1"]]
  :zprint {:old? false}
  :repl-options {:init-ns data.html})
