(ns paos.mapping
  (:require [clojure.string :as string]
            [paos.template :refer [sanitize-tag ->attr-name]]))

(declare make-mapping build-mapping)

(defn make-attributes [attrs tag tag-fix-fn]
  (filter identity
          (map (fn [[k v]]
                 (when (nil? v)
                   [(->attr-name (name k)) nil]))
               attrs)))

(defn make-mapping [node tag-fix-fn]
  (let [tag (first (sort-by (fn [k]
                              (count (name k)))
                            (keys node)))
        attrs (select-keys node
                           (filter (fn [k]
                                     (re-matches #".*\[attr=.*\]"
                                                 (name k)))
                                   (keys node)))
        comments (into {}
                       (map (fn [[k v]]
                              (let [[_ comment-name] (re-matches #".*\[comment=(.*)\]" (name k))]
                                [comment-name v]))
                            (select-keys node
                                         (filter (fn [k]
                                                   (re-matches #".*\[comment=.*\]"
                                                               (name k)))
                                                 (keys node)))))
        multiple-values? (get comments "min-occurs")
        children (build-mapping (get node tag) tag-fix-fn)
        attributes (into {} (make-attributes attrs tag tag-fix-fn))]
    (concat
     [(tag-fix-fn tag)
      (when-not (empty? children)
        ((if multiple-values? vector identity) (into {} children)))]
     (when-not (empty? attributes)
       [(keyword (str (name (tag-fix-fn tag)) "|attrs")) attributes]))))

(defn build-mapping [soap-action-message tag-fix-fn]
  (into {}
        (map (fn [[x y]]
               [x y])
             (partition-all 2 (mapcat identity (map (fn [[_ node]]
                                                      (make-mapping (into {} node) tag-fix-fn))
                                            (group-by (fn [[k v]]
                                                        (first (string/split (str k) #"\[" 2)))
                                                      soap-action-message)))))))

(comment

  (make-mapping {:v1:airlineByIcaoResponse
                 {:return
                  {:iata nil
                   (keyword "iata[attr=:val]") nil}
                  (keyword "return[comment=min-occurs]") 0}
                 }
                identity)

  (concat [[1 2 3]] [4 5])

  {:soapenv:Envelope
   {:soapenv:Header nil,
    :soapenv:Body
    {:v1:airlineByIcaoResponse
     {:return|attrs
      {:offset nil}
      :return
      [{:category nil,
        :iata nil,
        :name nil,
        :icao nil,
        :dateFrom nil,
        :dateTo nil,
        :active nil,
        :phoneNumber nil,
        :fs nil}]}}}}

  (defprotocol PSOAPClient
    (soap-action [this])
    (send [this context])
    (parse [this])
    (-request-body [this context])
    (-response-body [this context]))

  (clojure.repl/doc doall)

  (->> image
       convert-gray
       find-edges
       (draw-mat panel))

  (doto (java.util.HashMap.)
    ((fn [x] (.put x "a" 1)))
    (.put "b" 2)
    (println))

  )
