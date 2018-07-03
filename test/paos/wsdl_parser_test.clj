(ns paos.wsdl-parser-test
  (:require [clojure.test :as t]
            [paos.wsdl-parser :as wsdl-parser]))

(t/deftest parse-comment

  (t/testing "should throw a NullPointerException when comment doesn't have a content"
    (t/are [x] (thrown? NullPointerException (wsdl-parser/parse-comment x))
      nil
      {}
      {:content nil}
      ""))

  (t/testing "should return nil if comment shouldn't be parsed"
    (t/is (= (wsdl-parser/parse-comment {:content "somefailstring"})
             nil)))

  (t/testing "should return a map with :optional keyword if content has optional mark"
    (t/is (= (wsdl-parser/parse-comment {:content "Optional:  "})
             {:optional true})))

  (t/testing "should return a map with :min-occurs or :max-occurs or both keywords"
    (t/are [x y] (= (wsdl-parser/parse-comment {:content x}) y)
      "Zero or more repetitions:" {:min-occurs 0}
      "1 or more repetitions:"    {:min-occurs 1}
      "2 or more repetitions:"    {:min-occurs 2}
      "2 to 4 repetitions:"       {:min-occurs 2 :max-occurs 4}))

  (t/testing "should return a map with :type keyword"
    (t/is (= (wsdl-parser/parse-comment {:content "type: string"})
             {:type "string"}))
    (t/is (= (wsdl-parser/parse-comment {:content "anonymous type - enumeration: [FOO,BAR]"})
             {:type "anonymous type"
              :enumeration ["FOO" "BAR"]}))))
