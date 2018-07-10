(ns paos.response-parser
  (:require [clojure.xml :as xml]
            [clojure.data.xml :as data-xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as data-zip-xml]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- in-array? [m]
  (first (filter #(vector? %) (vals m))))

(defn- path-attrs-fix [k]
  (let [[f s] (-> k
                  name
                  (string/split #"\|")
                  (->> (split-at 1)))]
    (concat f s)))

(defn- path-array-fix [k]
  (let [[f s] (-> k
                  name
                  (string/split #"\|")
                  (->> (split-at 1)))]
    (concat f [0] s)))

(defn- get-all-paths*
  ([m] (get-all-paths* [] m))
  ([path m]
   (map (fn [[k v]]
          (cond
            (map? v) (let [path (if (in-array? m)
                                  (vec (concat path (path-array-fix k)))
                                  (vec (concat path (path-attrs-fix k))))]
                       (get-all-paths* path v))
            (vector? v) (or (not-empty (map #(get-all-paths* (conj (vec (concat path (path-attrs-fix k))) 0) %) v))
                            {:path (conj (vec (concat path (path-attrs-fix k))) 0 :__text)})
            :otherwise {:path (vec (concat path (path-attrs-fix k)))}))
        m)))

(defn get-all-paths [m]
  (map (fn [{:keys [path]}]
         (vec (map (fn [path-part]
                     (cond
                       (string? path-part) (keyword path-part)
                       :otherwise path-part))
                   path)))
       (flatten (get-all-paths* m))))

(defn- path->attr [path]
  (let [attr-name (-> path last name (string/replace #"\|attrs$" "") keyword)]
    (concat (butlast path) [:__attrs attr-name])))

(declare map->)

(defn- map1-> [path x]
  (if (= :__attrs (->> path (take-last 2) first))
    (assoc-in {}
              path
              (data-zip-xml/attr (if (= 2 (count path))
                                   x
                                   (apply (partial data-zip-xml/xml1-> x)
                                          (drop-last 2 path)))
                                 (last path)))
    (if (= (last path) :__text)
      {:__text (data-zip-xml/text x)}
      (assoc-in {}
                path
                (data-zip-xml/text (apply (partial data-zip-xml/xml1-> x) path))))))

(defn- map*-> [path x]
  (let [[path _ & xs] (partition-by #(= % 0) path)]
    (assoc-in {}
              path
              (mapv (fn [loc]
                      (map-> (flatten xs) loc))
                    (apply data-zip-xml/xml-> x path)))))

(defn- map-> [path x]
  (if (> (.indexOf path 0) 0)
    (map*-> path x)
    (map1-> path x)))

(defn- deep-merge [x & xs]
  (if (empty? xs)
    x
    (let [x1 x
          x2 (first xs)
          res (cond
                (map? x1) (merge-with deep-merge x1 x2)
                (vector? x1) (vec (map-indexed (fn [i x1]
                                                 (merge-with deep-merge x1 (nth x2 i)))
                                               x1)))]
      (apply deep-merge res (rest xs)))))

(defn- string->stream
  ([s] (string->stream s "UTF-8"))
  ([s encoding]
   (-> s
       (.getBytes encoding)
       (java.io.ByteArrayInputStream.))))

(defn parser [mapping]
  (fn [string]
    (let [xml-string (-> string string->stream xml/parse zip/xml-zip)]
      (apply deep-merge (mapv #(map-> % xml-string)
                              (get-all-paths mapping))))))

(comment

  (require '[clojure.tools.trace :as trace])

  (trace/trace-ns 'paos.response-parser)

  (def mapping {:soapenv:Envelope
                {:soapenv:Header nil,
                 :soapenv:Body
                 {:v1:airlineByIcaoResponse
                  {:return
                   [{:category    {:foo nil},
                     :iata        nil,
                     :name        nil,
                     :icao        nil,
                     :dateFrom    nil,
                     :dateTo      nil,
                     :active      nil,
                     :phoneNumber nil,
                     :fs          nil
                     :offset|attr nil}]}}}})

  (def response (-> "sample-response.xml" io/resource io/file xml/parse zip/xml-zip))

  (get-all-paths {:foo {:bar [{}]
                        :bar|attrs {:y nil}}
                  :foo|attrs {:x nil}})

  (let [x (parser {:foo {:bar [{:y|attr nil}]
                         :x|attr nil}
                   :foo|attrs {:x nil}})]
    (x "<foo x=\"1\"><bar y=\"2\">4</bar><bar y=\"3\">6</bar></foo>"))

  )
