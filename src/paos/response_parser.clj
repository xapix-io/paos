(ns paos.response-parser
  (:require [clojure.xml :as xml]
            [clojure.data.xml :as data-xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as data-zip-xml]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn make-parser [mapping]
  (fn [response]
    ))

(comment

  (def mapping {:soapenv:Envelope
                {:soapenv:Header nil,
                 :soapenv:Body
                 {:v1:airlineByIcaoResponse
                  {:return
                   [{:category nil,
                     :iata nil,
                     :name nil,
                     :icao nil,
                     :dateFrom nil,
                     :dateTo nil,
                     :active nil,
                     :phoneNumber nil,
                     :fs nil}]
                   :return|attrs {:offset nil}}}}})

  (def response (-> "sample-response.xml" io/resource io/file xml/parse zip/xml-zip))

  (defn node1-> [root node-mapping]
    )

  (-> {:root nil
       :root|attrs nil}
      keys
      (->> (filter #(-> %
                       name
                       (string/ends-with? "|attrs")
                       not)))
      first)

  (defn node->map [root node-mapping]
    )

  (defn node-> [root node-mapping]
    (let [tag (-> node-mapping
                  keys
                  (->> (filter #(-> %
                                   name
                                   (string/ends-with? "|attrs")
                                   not)))
                  first)])
    (-> root
        (data-zip-xml/xml1-> tag)
        ))

  (defn response-> [root mapping]
    (let [tags (keys mapping)
          nodes (map #(select-keys mapping [% (-> % name (str "|attrs") keyword)])
                     tags)]
      (map (fn [node]
             (let [tag (-> node
                           keys
                           (->> (sort-by #(-> % name count)))
                           first)
                   attrs ((-> tag name (str "|attrs") keyword) mapping)]
               (if (map? (tag node))
                 [tag (response-> (tag node) (tag mapping)) attrs]
                 [tag nil attrs])))
           nodes)))

  (response-> response mapping)

  (-> response
      (data-zip-xml/xml1-> :soapenv:Envelope :soapenv:Header)
      first
      :content)

  (defn parse [mapping loc path]
    )

  (defn entity->map [path-prefix entity-mapping root]
    (into {}
          (map (fn [tag]
                 [tag (data-zip-xml/text (apply data-zip-xml/xml-> root path-prefix tag))])
               (keys entity-mapping))))

  (entity->map [:soapenv:Body])

  (clojure.pprint/pprint response)

  (-> response
      (fn [res]
        {:soapenv:Envelope {:soapenv:Header (-> res
                                                (data-zip-xml/xml1-> :soapenv:Envelope :soapenv:Header)
                                                first
                                                :content)
                            :soapenv:Body (-> res
                                              (data-zip-xml/xml-> :soapenv:Envelope :soapenv:Body :v1:airlineByIcaoResponse :return)
                                              (fn [loc]
                                                {:v1:airlineByIcaoResponse (map (fn [loc]
                                                                                  {:return {:fs (-> loc
                                                                                                    (data-zip-xml/xml1-> :fs)
                                                                                                    (data-zip-xml/text))}
                                                                                   :return_offset (-> loc
                                                                                                      (data-zip-xml/attr :offset))})
                                                                                loc)
                                                 }))}}))


  (let [f (make-parser mapping)]
    (f response))

  )
