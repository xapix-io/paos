(ns paos.core
  (:require [paos.wsdl :as wsdl]
            [paos.service :as service]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pprint]))

(def cli-options
  [["-w" "--wsdl WSDL" "Url or filesystem path or a content of wsdl. (Required)"
    :parse-fn str
    :validate [#(string? (not-empty %)) "Must be a non-empty string."]]
   ["-b" "--binding BINDING" "One of SOAP binding defined in WSDL."
    :parse-fn str]
   ["-o" "--operation OPERATION" "One of SOAP operation defined for given WSDL binding. Can not be used without binding."
    :parse-fn str]
   ["-h" "--help" "Display this help message"]])

(defn- help
  [args]
  (println "\nUSAGE:\n")
  (println "clj -m" (namespace `help) "<options>\n")
  (println (:summary args)))

(defn- print-table [bindings]
  (pprint/print-table
   (mapcat (fn [[binding {:keys [operations]}]]
             (map (fn [[operation _]]
                    {:binding binding
                     :operation operation})
                  operations))
           bindings)))

(defn -main [& args]
  (let [args (parse-opts args cli-options)]
    (if (:errors args)
      (do (doseq [e (:errors args)]
            (println e))
          (help args))
      (if (-> args :options :help)
        (help args)
        (try
          (let [wsdl (wsdl/parse (-> args :options :wsdl))]
            (if-let [binding (-> args :options :binding)]
              (if-let [operation (-> args :options :operation)]
                (let [srv (get-in wsdl [binding :operations operation])]
                  (do
                    (println "\nService url:\n")
                    (println (get-in wsdl [binding :url]))
                    (println "\nSOAP action:\n")
                    (println (service/soap-action srv))
                    (println "\nRequest message:\n")
                    (println (service/request-xml srv))
                    (println "\nResponse message:\n")
                    (println (service/response-xml srv))))
                (print-table (select-keys wsdl [binding])))
              (print-table wsdl))
            (System/exit 0))
          (finally
            ;; Only called if `parse` raises an exception
            (shutdown-agents)))))))
