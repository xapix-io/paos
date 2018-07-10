(ns paos.wsdl-parser
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [paos.sample-message :as sample-message])
  (:import [org.reficio.ws.builder.core SoapOperationImpl Wsdl]
           org.reficio.ws.SoapContext))

(defn ^SoapContext make-wsdl-context []
  (.build (doto (SoapContext/builder)
            (.exampleContent false)
            (.typeComments true)
            (.valueComments true)
            (.buildOptional true)
            (.alwaysBuildHeaders true))))

(defn ^java.net.URL make-wsdl-url [^String wsdl-path]
  (io/as-url (io/file wsdl-path)))

(defn ^Wsdl make-wsdl [wsdl-url]
  (Wsdl/parse wsdl-url))

(defn make-operation [^SoapContext ctx binding-builder ^SoapOperationImpl operation]
  (let [operation-name  (.getOperationName operation)
        soap-action     (.getSoapAction operation)
        input-template  (.buildInputMessage operation ctx)
        output-template (.buildOutputMessage operation ctx)
        input-element   (sample-message/xml->element input-template)
        output-element  (sample-message/xml->element output-template)]
    [operation-name {:soap-action     soap-action
                     :input-template  (sample-message/->template input-element)
                     :input-xml       (sample-message/get-original input-element)
                     :input-mapping   (sample-message/->mapping input-element)
                     :output-template (sample-message/->template output-element)
                     :output-xml      (sample-message/get-original output-element)
                     :output-mapping  (sample-message/->mapping output-element)}]))

(defn make-binding
  ([^Wsdl wsdl ^String binding-name]
   (make-binding wsdl binding-name (make-wsdl-context)))
  ([^Wsdl wsdl ^String binding-name ^SoapContext ctx]
   (let [binding-builder (.find (.localPart (.binding wsdl) binding-name))]
     [binding-name {:operations (into {}
                                      (map (partial make-operation ctx binding-builder)
                                           (.getOperations binding-builder)))
                    :url (first (.getServiceUrls binding-builder))}])))

(defn parse-wsdl [wsdl-path]
  (let [wsdl (-> wsdl-path
                 make-wsdl-url
                 make-wsdl)
        ctx (make-wsdl-context)]
    (into {}
          (map (fn [binding]
                 (make-binding wsdl
                               (.getLocalPart binding)
                               ctx))
               (.getBindings wsdl)))))

(comment

  ((paos.response-parser/parser
    (mapping/build-mapping
     (xml->map "<foo x=\"?\">
                  <!--Zero or more repetitions:-->
                  <bar y=\"?\">
                    <baz>?</baz>
                  </bar>
                </foo>")
     identity) )
   "<foo x=\"1\">
      <bar y=\"2\">
        <baz>4</baz>
      </bar>
      <bar y=\"3\">
        <baz>6</baz>
      </bar>
      <bar y=\"5\">
        <baz>8</baz>
      </bar>
      <bar y=\"3\">
        <baz>6</baz>
      </bar>
      <bar y=\"8\">
        <baz>619</baz>
      </bar>
      <bar y=\"12\">
        <baz>4312</baz>
      </bar>
    </foo>")

  (paos.response-parser/get-all-paths (mapping/build-mapping
                                       (xml->map "<foo x=\"?\">
                  <!--Zero or more repetitions:-->
                  <bar y=\"?\">
                    <baz>?</baz>
                  </bar>
                </foo>")
                                       identity)
                                      )

  (def x
    (parse-wsdl "/Users/delaguardo/Projects/xapix/soap-clj/resources/bookStore.wsdl")
    ;; (parse-wsdl "/Users/delaguardo/Downloads/Xapix/Account_Address.wsdl")
    ;; (parse-wsdl "/tmp/foo.wsdl")
    )

  (clojure.pprint/pprint x)
  (keys (get-in x ["SoftLayer_Account_Regional_Registry_Detail_Property_TypeBinding" :operations "getAllObjects"]))

  (clojure.pprint/pprint (mapping/build-mapping (assoc-in (-> x first second :operations first second :output)
                                                          [:soapenv:Envelope :soapenv:Body :v1:airlineByIcaoResponse (keyword "return[attr=:foo]")] nil) identity))

  (def root
    (-> "/Users/delaguardo/Projects/xapix/soap-clj/sample.xml"
        clojure.java.io/file
        clojure.xml/parse
        clojure.zip/xml-zip)
    )

  (clojure.data.zip.xml/xml1-> root :soapenv:Envelope)

  (def x
    (-> "/Users/delaguardo/Downloads/Xapix/Account_Regional_Registry_Detail_Property_Type.wsdl.txt"
        make-wsdl-url
        make-wsdl))

  (.saveWsdl x
             (-> "/Users/delaguardo/Downloads/Xapix/Account_Regional_Registry_Detail_Property_Type.wsdl.txt"
                 make-wsdl-url)
             (io/file "/tmp/foo")))
