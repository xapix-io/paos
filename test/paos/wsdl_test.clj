(ns paos.wsdl-test
  (:require [paos.wsdl :as sut]
            [clojure.test :as t]
            [clojure.java.io :as io]))

(t/deftest parse-from-jar
  (t/is (map? (sut/parse (io/resource "hello.wsdl")))
        "should be able to parse from URL to classpath JAR"))
