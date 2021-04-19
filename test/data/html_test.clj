(ns data.html-test
  (:require [clojure.test :refer :all]
            [data.html :refer :all]))

(deftest simple
  (testing "Simple HTML tokenization"
           (is (= '({:type :start-tag, :data "html"}
                    {:type :end-tag, :data "html"}
                    {:type :EOF})
                  (tokenize "<html></html>")))))
