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
