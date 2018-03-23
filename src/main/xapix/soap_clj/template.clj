(ns xapix.soap-clj.template
  (:require [clojure.data.xml :as xml]
            [clojure.string :as string]))

(defn make-attr [attr path]
  (let [[_ attr-name] (re-matches #".*\[attr=(.*)\]" attr)]
    [attr-name (str "{{" path "." attr-name "}}")]))

(defn make-attrs [attrs path]
  (into {} (map #(make-attr % path) attrs)))

(defn ->attr-name [attr]
  (let [[_ attr-namespace _ attr-name] (re-matches #".*\[attr=:([^/]+)(/(.*))?\]" attr)]
    (apply keyword (filter identity (vector attr-namespace attr-name)))))

(declare make-node make-nodes)

(defn sanitize-tag [tag]
  (last (string/split (name tag) #":")))

(defn make-node
  ([node]
   (make-node node nil))
  ([node ctx]
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
         optional? (or (get comments "optional")
                       (= (get comments "min-occurs") 0))
         multiple-values? (get comments "min-occurs")]
     (let [element (xml/element tag
                                (into {} (map (fn [[k v]]
                                                [(->attr-name (name k))
                                                 (if v
                                                   v
                                                   (str "{{ctx." (sanitize-tag ctx) "_" (sanitize-tag (->attr-name (name k))) "}}"))])
                                              attrs))
                                (let [nodes (make-nodes (get node tag) tag)]
                                  (if (empty? nodes)
                                    (str "{{ctx." (sanitize-tag ctx) "}}")
                                    nodes)))]
       (if ctx
         [(when optional?
            (xml/cdata "{% if ctx %}"))
          (when multiple-values?
            (xml/cdata "{% for x in ctx %}"))
          (xml/cdata (str "{% with ctx=" (if multiple-values? "x." "ctx.") (sanitize-tag tag) " %}"))
          element
          (xml/cdata "{% endwith %}")
          (when multiple-values?
            (xml/cdata "{% endfor %}"))
          (when optional?
            (xml/cdata "{% endif %}"))]
         element)))))

(defn make-nodes [nodes ctx]
  (map (fn [[_ node]]
         (make-node (into {} node) ctx))
       (group-by (fn [[k v]]
                   (first (string/split (str k) #"\[" 2)))
                 nodes)))

(defn build-template [soap-action-message]
  (xml/indent-str (make-node soap-action-message)))
