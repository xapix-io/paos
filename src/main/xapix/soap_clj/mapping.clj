(ns xapix.soap-clj.mapping
  (:require [clojure.string :as string]
            [xapix.soap-clj.template :refer [sanitize-tag ->attr-name]]))

(declare make-mapping build-mapping)

(defn make-attributes [attrs tag]
  (filter identity
          (map (fn [[k v]]
                 (when (nil? v)
                   [(str (sanitize-tag tag) "_" (name (->attr-name (name k)))) nil]))
               attrs)))

(defn make-mapping [node]
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
        children (build-mapping (get node tag))
        attributes (make-attributes attrs tag)]
    (concat
     [(sanitize-tag tag)
      (when-not (empty? children)
        ((if multiple-values? vector identity) (into {} children)))]
     attributes)))

(defn build-mapping [soap-action-message]
  (into {}
        (map (fn [[x y]]
               [x y])
             (partition-all 2 (flatten (map (fn [[_ node]]
                                              (make-mapping (into {} node)))
                                            (group-by (fn [[k v]]
                                                        (first (string/split (str k) #"\[" 2)))
                                                      soap-action-message)))))))
