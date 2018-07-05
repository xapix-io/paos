(ns paos.core
  (:require [paos.wsdl-parser :as wsdl-parser]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [cheshire.core :as cheshire]
            [clojure.data.zip.xml :as zip-xml]))

(defn decompose-wsdl [wsdl-path]
  (wsdl-parser/parse-wsdl wsdl-path))

(defn -main [& args]
  (println (cheshire/generate-string (wsdl-parser/parse-wsdl (first args)) {:pretty true})))

(comment

  (def x
    (wsdl-parser/parse-wsdl "resources/airlinesService.xml"))

  x

  )
