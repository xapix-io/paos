(ns paos.core
  (:require [paos.wsdl :as wsdl]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [cheshire.core :as cheshire]
            [clojure.data.zip.xml :as zip-xml]))

(defn decompose-wsdl [wsdl-path]
  (wsdl/parse wsdl-path))

(defn -main [& args]
  (println (cheshire/generate-string (wsdl/parse (first args)) {:pretty true})))

(comment

  (def x
    (wsdl/parse "resources/airlinesService.xml"))

  x

  )
