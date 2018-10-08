(ns paos.wsdl
  (:require [clojure.java.io :as io]
            [paos.service :as service])
  (:import java.net.MalformedURLException
           javax.wsdl.Binding
           javax.wsdl.extensions.soap.SOAPBinding
           javax.wsdl.extensions.soap12.SOAP12Binding
           javax.xml.namespace.QName
           [org.reficio.ws SoapBuilderException SoapContext]
           [org.reficio.ws.builder SoapBuilder]
           [org.reficio.ws.builder.core SoapOperationImpl Wsdl]))

(defn ^SoapContext make-wsdl-context []
  (.build (doto (SoapContext/builder)
            (.exampleContent false)
            (.typeComments true)
            (.valueComments true)
            (.buildOptional true)
            (.alwaysBuildHeaders true))))

(defn ^java.net.URL make-wsdl-url [^String wsdl-path]
  (io/as-url (io/file wsdl-path)))

(defn ^Wsdl make-wsdl [^java.net.URL wsdl-url]
  (Wsdl/parse wsdl-url))

(defn- binding-has-instance? [^SoapBuilder binding-builder c]
  (let [^Binding binding (.getBinding binding-builder)
        elements (.getExtensibilityElements binding)]
    (some (partial instance? c) elements)))

(defn soap-version [binding-builder]
  (cond
    (binding-has-instance? binding-builder SOAPBinding)
    :soap

    (binding-has-instance? binding-builder SOAP12Binding)
    :soap12

    :otherwise :soap))

(defn make-operation [^SoapContext ctx ^SoapBuilder binding-builder ^SoapOperationImpl operation]
  (let [operation-name  (.getOperationName operation)
        soap-action     (.getSoapAction operation)
        soap-version    (soap-version binding-builder)
        input-template  (.buildInputMessage operation ctx)
        output-template (.buildOutputMessage operation ctx)
        fault-template  (.buildEmptyFault operation ctx)
        service         (service/->service soap-action soap-version input-template output-template fault-template)]
    [operation-name service]))

(defn make-binding
  ([^Wsdl wsdl ^String binding-name]
   (make-binding wsdl binding-name (make-wsdl-context)))
  ([^Wsdl wsdl ^String binding-name ^SoapContext ctx]
   (let [binding-builder (.find (.localPart (.binding wsdl) binding-name))]
     [binding-name {:operations (into {}
                                      (map (partial make-operation ctx binding-builder)
                                           (.getOperations binding-builder)))
                    :url (first (.getServiceUrls binding-builder))}])))

(defn file->wsdl [file-name]
  (-> file-name
      make-wsdl-url
      make-wsdl))

(defn net-url->wsdl [net-url]
  (-> net-url
      (java.net.URL.)
      make-wsdl))

(defn wsdl-content->wsdl [wsdl-content]
  (let [file-name (java.io.File/createTempFile "service" ".wsdl")]
    (with-open [file (clojure.java.io/writer file-name)]
      (binding [*out* file]
        (println wsdl-content)))
    (-> file-name
        make-wsdl-url
        make-wsdl)))

(defn ->wsdl [path-or-content]
  (cond
    (.exists (io/file path-or-content))
    (file->wsdl path-or-content)

    (try
      (java.net.URL. path-or-content)
      (catch MalformedURLException e
        false))
    (net-url->wsdl path-or-content)

    :otherwise (wsdl-content->wsdl path-or-content)))

(defn parse [wsdl]
  (let [^Wsdl wsdl (->wsdl wsdl)
        ctx (make-wsdl-context)]
    (into {}
          (map (fn [^QName binding]
                 (try
                   (make-binding wsdl
                                 (.getLocalPart binding)
                                 ctx)
                   (catch SoapBuilderException e
                     [(.getLocalPart binding) nil])))
               (.getBindings wsdl)))))

(comment

  (parse "http://www.thomas-bayer.com/axis2/services/BLZService?wsdl")
  (parse (slurp "/Users/delaguardo/Downloads/BLZService.wsdl"))
  (parse "/Users/delaguardo/Downloads/BLZService.wsdl")

  (require '[clj-http.client :as client])
  (require '[paos.service :as service])
  (require '[paos.wsdl :as wsdl])

  (defn parse-response [{:keys [status body] :as response} body-parser fail-parser]
    (assoc response
           :body
           (case status
             200 (body-parser body)
             500 (fail-parser body))))

  (let [soap-service   (wsdl/parse "http://www.thomas-bayer.com/axis2/services/BLZService?wsdl")
        srv            (get-in soap-service ["BLZServiceSOAP11Binding" :operations "getBank"])
        soap-url       (get-in soap-service ["BLZServiceSOAP11Binding" :url])
        soap-headers   (service/soap-headers srv)
        content-type   (service/content-type srv)
        mapping        (service/request-mapping srv)
        context        (assoc-in mapping ["Envelope" "Body" "getBank" "blz" :__value] "28350000")
        body           (service/wrap-body srv context)
        resp-parser    (partial service/parse-response srv)
        fault-parser   (partial service/parse-fault srv)]
    (-> soap-url
        (client/post {:content-type content-type
                      :body         body
                      :headers      (merge {} soap-headers)
                      :do-not-throw true})
        (parse-response resp-parser fault-parser))))
