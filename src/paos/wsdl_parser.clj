(ns paos.wsdl-parser
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [paos.template :as template]
            [paos.mapping :as mapping])
  (:import [org.reficio.ws.builder.core SoapOperationImpl Wsdl]
           org.reficio.ws.SoapContext))

(defn parse-comment [{:keys [content]}]
  (cond
    (string/starts-with? content "Optional:")
    {:optional true}

    (re-matches #"type: ([a-zA-Z]+)$" content)
    {:type (string/trim (second (re-matches #"type: ([a-zA-Z]+)$" content)))}

    (string/starts-with? content "Zero or more repetitions:")
    {:min-occurs 0}

    (re-matches #"([\d]+) or more repetitions:" content)
    {:min-occurs (Integer/parseInt
                  (last (re-matches #"([\d]+) or more repetitions:" content)))}

    (re-matches #"([\d]+) to ([\d]+) repetitions:" content)
    (let [[_ min max] (re-matches #"([\d]+) to ([\d]+) repetitions:" content)]
      {:min-occurs (Integer/parseInt min)
       :max-occurs (Integer/parseInt max)})

    (re-matches #"(type: ([a-zA-Z]+)|anonymous type) - enumeration: \[(.*)\]" content)
    (let [matcher (re-matches #"(type: ([a-zA-Z]+)|anonymous type) - enumeration: \[(.*)\]" content)
          enumeration (mapv string/trim
                             (string/split (nth matcher 3)
                                          #","))
          type (string/trim (or (nth matcher 2) "anonymous type"))]
      {:enumeration enumeration
       :type type})

    :otherwise
    nil))

(defn xml->map*
  ([xml]
   (xml->map* xml {}))
  ([{:keys [tag attrs content] :or {content '()
                                    attrs {}}}
    comments]
   (let [content (if (string? content) (cons '() content) content)
         content (filter #(not (string/starts-with? % "\n")) content)]
     (merge {(or tag :comment)
             (when-not (= content '("?"))
               (cond
                 (empty? content) {}
                 (string? content) content
                 :otherwise (loop [acc {} comment-acc {} content content]
                              (if (empty? content)
                                acc
                                (let [item (first content)
                                      items (rest content)]
                                  (cond
                                    (nil? (:tag item))
                                    (recur acc (merge comment-acc (parse-comment item)) items)

                                    :otherwise
                                    (recur (merge acc (xml->map* item comment-acc)) {} items)))))))}
            (into {}
                  (map (fn [[k v]]
                         [(keyword (namespace tag) (str (name tag) "[attr=" k "]"))
                          (when-not (= v "?")
                            v)])
                       attrs))
            (into {}
                  (map (fn [[k v]]
                         [(keyword (namespace tag) (str (name tag) "[comment=" (name k) "]"))
                          v])
                       comments))))))

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

(defn xml->map [msg]
  (xml->map*
   (xml/parse (java.io.StringReader. msg)
              :namespace-aware false
              :include-node? #{:element :characters :comment})))

(defn make-operation [^SoapContext ctx binding-builder ^SoapOperationImpl operation]
  (let [operation-name (.getOperationName operation)
        soap-action (.getSoapAction operation)
        input-template (.buildInputMessage operation ctx)
        output-template (.buildOutputMessage operation ctx)
        input (xml->map input-template)
        output (xml->map output-template)]
    [operation-name {:soap-action soap-action
                     :input-template (template/build-template input)
                     :input-xml input-template
                     :input (xml->map input-template)
                     :input-mapping (mapping/build-mapping input template/sanitize-tag)
                     :output-template (template/build-template output)
                     :output-xml output-template
                     :output (xml->map output-template)
                     :output-mapping (mapping/build-mapping output identity)}]))

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

  (def x
    (parse-wsdl "/Users/delaguardo/Projects/xapix/soap-clj/resources/airlinesService.xml")
    ;; (parse-wsdl "/Users/delaguardo/Downloads/Xapix/Account_Address.wsdl")
    ;; (parse-wsdl "/tmp/foo.wsdl")
    )

  (clojure.pprint/pprint x)
  (keys (get-in x ["SoftLayer_Account_Regional_Registry_Detail_Property_TypeBinding" :operations "getAllObjects"]))

  (clojure.pprint/pprint (-> x first second :operations first second :input-mapping))

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
