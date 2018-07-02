(ns xapix.soap-clj.core
  (:require [xapix.soap-clj.wsdl-parser :as wsdl-parser]))

(defn decompose-wsdl [wsdl-path]
  (wsdl-parser/parse-wsdl wsdl-path))

(comment

  (def x
    (wsdl-parser/parse-wsdl "resources/airlinesService.xml"))

  x


  )
