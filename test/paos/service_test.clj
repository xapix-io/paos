(ns paos.service-test
  (:require [paos.service :as sut]
            [clojure.test :as t]
            [clojure.java.io :as io]))

(def service (-> "sample_message.xml"
                 io/resource
                 slurp))

(def service-response (-> "sample_message_response.xml"
                          io/resource
                          slurp))

(t/deftest xml->element

  (let [element (sut/xml->element service)]

    (t/testing "should return object with methods for working with xml as a map:"

      (t/is (satisfies? sut/Element element))

      (t/testing "get-original returns original xml"

        (t/is (= service (sut/get-original element))))

      (t/testing "get-tag returns tag from current root"

        (t/is (= :soapenv:Envelope (sut/get-tag element)))
        (t/is (= '(:soapenv:Header :soapenv:Body)
                 (-> element
                     sut/get-fields
                     (->> (map sut/get-tag))))))

      (t/testing "get-fields returns list of child nodes for current root"

        (t/is (= 2 (count (sut/get-fields element))))

        (t/is (every? #(satisfies? sut/Element %)
                      (sut/get-fields element)))

        (let [deep-nested-children (-> element
                                       sut/get-fields
                                       second
                                       sut/get-fields)]
          (t/is (every? #(satisfies? sut/Element %)
                        deep-nested-children))))

      (t/testing "get-paths returns list of all paths to leaf values in nested object"

        (let [paths (sut/get-paths element)]
          (t/is (= (inc (count (filter #(= % \?)
                                       service)))
                   (count paths)))

          (t/is (= {:path [:soapenv:Envelope :soapenv:Header :__value]}
                   (first paths)))

          (t/testing "walking throw nested objects"

            (t/is (= {:path [:soapenv:Envelope :soapenv:Body :book:SimpleObjectWithComplexTag :Field1 :__value]}
                     (second paths))))

          (t/testing "walking throw nested arrays by marking itself with 0"

            (t/is (= {:path [:soapenv:Envelope :soapenv:Body :book:ArrayOneOrMoreRepetition 0 :ArrayField :__value]}
                     (nth paths 3))))

          (t/testing "walking throw nested objects to attributes"

            (t/is (= {:path [:soapenv:Envelope :soapenv:Body :book:ComplexObjectWithComplexTag 0 :BookId :__attrs :BookType :__value]}
                     (nth paths 6))))))

      (t/testing "->mapping converts xml into map with arrays attributes and all nested structures. Tags converted into \"safe\" string representation"

        (let [mapping (sut/->mapping element)]

          (t/is (= "Envelope" (-> mapping keys first)))

          (t/testing "extra plural field added for array"

            (t/is (vector? (get-in mapping ["Envelope" "Body" "ArrayOneOrMoreRepetition" "ArrayFields"]))))

          (t/testing "extra key :__attrs added for node attributes"

            (t/is (= {:__value {:__type "string"}}
                     (get-in mapping ["Envelope" "Body" "ComplexObjectWithComplexTag" "BookIds" 0 "BookId" :__attrs "BookType"]))))

          (t/is (= {"Envelope"
                    {"Header" {}
                     "Body"
                     {"SimpleObjectWithComplexTag"
                      {"Field1" {:__value {:__type nil}}
                       "Field2" {:__value {:__type nil}}}
                      "ArrayOneOrMoreRepetition"
                      {"ArrayFields" [{"ArrayField" {:__value {:__type "string"}}}]}
                      "ArrayZeroOrMoreRepetition"
                      {"ArrayFields" [{"ArrayField" {:__value {:__type "integer"
                                                               :__optional? true}}}]}
                      "ArrayWithNestedArray"
                      {"ArrayFields"
                       [{"ArrayField"
                         {"NestedArrays"
                          [{"NestedArray" {:__value {:__type nil
                                                     :__optional? true}}}]}}]}
                      "ComplexObjectWithComplexTag"
                      {"BookIds"
                       [{"BookId"
                         {:__attrs {"BookType" {:__value {:__type "string"}}}
                          "ID" {:__value {:__type "string"
                                          :__enum ["FOO_ID" "BAR_ID"]
                                          :__optional? true}}
                          "Type" {"SubType" {:__value {:__type "integer"}}}}}]
                       "RequestId" {:__value {:__type "string"}}
                       "ArrayIds"
                       [{"ArrayId"
                         {:__attrs
                          {"x" {:__value {:__type "string"}}
                           "y" {:__value {:__type "string"}}}
                          :__value {:__type "string"}}}]}}}}
                   mapping))))

      (t/testing "->parse-fn return function able to parse xml strings according to the mapping attached to element"

        (let [parse-fn (sut/->parse-fn element)
              parsed-data (parse-fn service-response)]

          (t/testing "data parsed"

            (t/is (= parsed-data
                     {"Envelope"
                      {"Header" {"__value" ""}
                       "Body"
                       {"SimpleObjectWithComplexTag"
                        {"Field1" {"__value" "123"} "Field2" {"__value" "asdj"}}
                        "ArrayOneOrMoreRepetition"
                        {"ArrayFields"
                         [{"__value" "1"} {"__value" "2"} {"__value" "3"} {"__value" "4"}]}
                        "ArrayZeroOrMoreRepetition"
                        {"ArrayFields"
                         [{"__value" "123"}
                          {"__value" "123"}
                          {"__value" "123"}
                          {"__value" "123"}
                          {"__value" "123"}
                          {"__value" "123"}
                          {"__value" "123"}]}
                        "ArrayWithNestedArray"
                        {"ArrayFields"
                         [{"NestedArrays" [{"__value" "q"} {"__value" "w"} {"__value" "e"}]}
                          {"NestedArrays" [{"__value" "r"}]}
                          {"NestedArrays" [{"__value" "t"} {"__value" "y"}]}]}
                        "ComplexObjectWithComplexTag"
                        {"BookIds"
                         [{"__attrs" {"BookType" {"__value" "ewq"}}
                           "ID" {"__value" "0"}
                           "Type" {"SubType" {"__value" ".1"}}}
                          {"__attrs" {"BookType" {"__value" "qwe"}}
                           "ID" {"__value" "1"}
                           "Type" {"SubType" {"__value" ".2"}}}]
                         "RequestId" {"__value" "jshvjdshg"}
                         "ArrayIds"
                         [{"__attrs" {"x" {"__value" "x1"} "y" {"__value" "y1"}}
                           "__value" "1"}
                          {"__attrs" {"x" {"__value" "x2"} "y" {"__value" "y2"}}
                           "__value" "2"}
                          {"__attrs" {"x" {"__value" "x3"} "y" {"__value" "y3"}}
                           "__value" "3"}]}}}}))))))))
