(ns paos.core
  (:require [paos.wsdl-parser :as wsdl-parser]
            [cheshire.core :refer [generate-string]]))

(defn decompose-wsdl [wsdl-path]
  (wsdl-parser/parse-wsdl wsdl-path))

(defn -main [& args]
  (println (generate-string (wsdl-parser/parse-wsdl (first args)) {:pretty true})))

(comment

  (def x
    (wsdl-parser/parse-wsdl "resources/airlinesService.xml"))

  x

  )
