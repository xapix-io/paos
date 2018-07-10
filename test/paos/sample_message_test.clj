(ns paos.sample-message-test
  (:require [paos.sample-message :as sut]
            [clojure.test :as t]
            [clojure.java.io :as io]))

(def sample-message (-> "sample_message.xml"
                        io/resource
                        slurp))

(t/deftest xml->element

  (let [element (sut/xml->element sample-message)]

    (t/testing "should return object with methods for working with xml as a map:"

      (t/is (satisfies? sut/Element element))

      (t/testing "get-original returns original xml"

        (t/is (= sample-message (sut/get-original element))))

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
                                       sample-message)))
                   (count paths)))

          (t/is (= {:path [:soapenv:Envelope :soapenv:Header]}
                   (first paths)))

          (t/testing "walking throw nested objects"

            (t/is (= {:path [:soapenv:Envelope :soapenv:Body :book:SimpleObjectWithComplexTag :Field1]}
                     (second paths))))

          (t/testing "walking throw nested arrays by marking itself with 0"

            (t/is (= {:path [:soapenv:Envelope :soapenv:Body :book:ArrayOneOrMoreRepetition 0 :ArrayField]}
                     (nth paths 3))))

          (t/testing "walking throw nested objects to attributes"

            (t/is (= {:path [:soapenv:Envelope :soapenv:Body :book:ComplexObjectWithComplexTag 0 :BookId :__attrs :BookType]}
                     (nth paths 6))))))

      (t/testing "->mapping converts xml into map with arrays attributes and all nested structures. Tags converted into \"safe\" string representation"

        (let [mapping (sut/->mapping element)]

          (t/is (= "Envelope" (-> mapping keys first)))

          (t/testing "extra plural field added for array"

            (t/is (vector? (get-in mapping ["Envelope" "Body" "ArrayOneOrMoreRepetition" "ArrayFields"]))))

          (t/testing "extra key :__attrs added for node attributes"

            (t/is (= {:__value nil :__type "string"}
                     (get-in mapping ["Envelope" "Body" "ComplexObjectWithComplexTag" "BookIds" 0 "BookId" :__attrs "BookType"]))))

          (t/is (= {"Envelope"
                    {"Header" {}
                     "Body"
                     {"SimpleObjectWithComplexTag"
                      {"Field1" {:__value nil :__type nil}
                       "Field2" {:__value nil :__type nil}}
                      "ArrayOneOrMoreRepetition"
                      {"ArrayFields" [{"ArrayField" {:__value nil :__type "string"}}]}
                      "ArrayZeroOrMoreRepetition"
                      {"ArrayFields" [{"ArrayField" {:__value nil :__type "integer"}}]}
                      "ArrayWithNestedArray"
                      {"ArrayFields"
                       [{"ArrayField"
                         {"NestedArrays"
                          [{"NestedArray" {:__value nil :__type nil}}]}}]}
                      "ComplexObjectWithComplexTag"
                      {"BookIds"
                       [{"BookId"
                         {:__attrs {"BookType" {:__value nil :__type "string"}}
                          "ID" {:__value nil :__type "string"}
                          "Type" {"SubType" {:__value nil :__type "integer"}}}}]
                       "RequestId" {:__value nil :__type "string"}
                       "ArrayIds"
                       [{"ArrayId"
                         {:__attrs
                          {"x" {:__value nil :__type "string"}
                           "y" {:__value nil :__type "string"}}
                          :__value nil
                          :__type "string"}}]}}}}
                   mapping)))))))
