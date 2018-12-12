(ns paos.service
  (:require [clojure.data.xml :as data-xml]
            [clojure.data.zip :as data-zip]
            [clojure.data.zip.xml :as data-zip-xml]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [inflections.core :refer [plural]]
            [selmer.parser :as selmer])
  (:import clojure.data.xml.node.Comment))

(declare node->element)

(defn- tag-fix [tag-name]
  (cond-> tag-name
    (keyword? tag-name) name
    true                (string/split #":")
    true                last))

(defn- parse-comment [comment]
  (cond
    (string/starts-with? comment "Optional:")
    {:optional true}

    (re-matches #"type: ([a-zA-Z]+)$" comment)
    {:type (string/trim (second (re-matches #"type: ([a-zA-Z]+)$" comment)))}

    (string/starts-with? comment "Zero or more repetitions:")
    {:min-occurs 0}

    (re-matches #"([\d]+) or more repetitions:" comment)
    {:min-occurs (Integer/parseInt
                  (last (re-matches #"([\d]+) or more repetitions:" comment)))}

    (re-matches #"([\d]+) to ([\d]+) repetitions:" comment)
    (let [[_ min max] (re-matches #"([\d]+) to ([\d]+) repetitions:" comment)]
      {:min-occurs (Integer/parseInt min)
       :max-occurs (Integer/parseInt max)})

    (re-matches #"(type: ([a-zA-Z]+)|anonymous type) - enumeration: \[(.*)\]" comment)
    (let [matcher     (re-matches #"(type: ([a-zA-Z]+)|anonymous type) - enumeration: \[(.*)\]" comment)
          enumeration (mapv string/trim
                            (string/split (nth matcher 3)
                                          #","))
          type        (string/trim (or (nth matcher 2) "anonymous type"))]
      {:enumeration enumeration
       :type        type})

    :otherwise
    nil))

(defn tag-name= [tagname]
  (fn [loc]
    (or (= (name tagname) (name (:tag (zip/node loc))))
        (filter #(and (zip/branch? %) (= (name tagname) (name (:tag (zip/node %)))))
                (data-zip/children-auto loc)))))

(defn path->predicates [path]
  (map #(tag-name= (tag-fix %))
       path))

(defn- xml->map [root {:keys [path]}]
  (loop [fp (first path) rp (rest path) path-to-update []]
    (case fp
      0        (assoc-in {}
                         (map tag-fix (conj path-to-update (-> rp first plural keyword)))
                         (mapv #(xml->map % {:path rp})
                               (apply data-zip-xml/xml->
                                      root
                                      (path->predicates (conj path-to-update (first rp))))))
      :__attrs (assoc-in {}
                         (map tag-fix (conj path-to-update :__attrs (first rp) :__value))
                         (some->
                          (if (not-empty path-to-update)
                            (apply data-zip-xml/xml1->
                                   root
                                   (path->predicates path-to-update))
                            root)
                          (data-zip-xml/attr (first rp))))
      :__value (assoc-in {}
                         (map tag-fix (conj path-to-update :__value))
                         (some->
                          (apply (partial data-zip-xml/xml1-> root)
                                 (path->predicates path-to-update))
                          data-zip-xml/text))
      (recur (first rp) (rest rp) (conj path-to-update fp)))))

(defn- deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn- deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn- custom-merge [v1 v2]
  (cond
    (map? v1)    (deep-merge-with custom-merge v1 v2)
    (vector? v1) (vec (map-indexed (fn [idx v1]
                                     (deep-merge-with custom-merge
                                                      v1 (nth v2 idx)))
                                   v1))
    :otherwise   v2))

(defprotocol Element
  (get-original [this])
  (get-tag      [this])
  (get-fields   [this])
  (get-attrs    [this])
  (get-type     [this])
  (get-path     [this] [this fix-fn])
  (get-paths    [this] [this fix-fn] [this fix-fn path])
  (->mapping    [this] [this fix-fn])
  (->template   [this] [this root?])
  (->parse-fn   [this])
  (is-optional? [this])
  (is-array?    [this])
  (is-leaf?     [this])
  (is-enum?     [this]))

(defprotocol Service
  (content-type      [this])
  (soap-headers      [this])
  (soap-action       [this])
  (soap-version      [this])

  (request-xml       [this])
  (request-mapping   [this])
  (request-template  [this])
  (wrap-body         [this context])
  (wrap-response     [this context])
  (wrap-fault        [this context])

  (response-xml      [this])
  (response-mapping  [this])
  (response-template [this])
  (parse-response    [this response-xml])

  (fault-xml         [this])
  (fault-mapping     [this])
  (fault-template    [this])
  (parse-fault       [this fault-xml]))

(defn- content->fields [content type optional? enumeration]
  (let [content (filter #(not (string/starts-with? % "\n")) content)]
    (loop [el (first content) els (rest content) comments [] fields []]
      (if-not el
        fields
        (cond
          (string? el)
          {:__value
           (cond-> {:__type type}
             optional?   (merge {:__optional? true})
             enumeration (merge {:__enum enumeration}))}

          (instance? Comment el)
          (recur (first els) (rest els) (conj comments (:content el)) fields)

          :otherwise
          (recur (first els) (rest els) [] (conj fields (node->element el comments nil))))))))

(defn- string->stream
  ([^String s] (string->stream s "UTF-8"))
  ([^String s ^String encoding]
   (-> s
       (.getBytes encoding)
       (java.io.ByteArrayInputStream.))))

(defn- node->element [{:keys [tag attrs content]
                       :or   {content '()
                              attrs   {}}}
                      comments
                      original-xml]
  (let [{:keys [type min-occurs max-occurs
                optional enumeration]} (into {} (map parse-comment comments))
        fields                         (content->fields content
                                                        type
                                                        (or optional (= min-occurs 0))
                                                        enumeration)]
    (reify
      Element
      (get-original [_] original-xml)

      (get-tag [_] tag)

      (get-fields [_] fields)

      (get-attrs [_] attrs)

      (get-type [_] type)

      (get-path [this] (get-path this identity))
      (get-path [this fix-fn]
        (conj (if (is-array? this)
                [0]
                [])
              (fix-fn (get-tag this))))

      (get-paths [this] (get-paths this identity []))
      (get-paths [this fix-fn] (get-paths this fix-fn []))
      (get-paths [this fix-fn path]
        (let [fields         (get-fields this)
              this-path      (apply (partial conj path) (get-path this fix-fn))
              paths-to-attrs (map (fn [[attr-name attr-value]]
                                    (when (= attr-value "?")
                                      {:path (conj this-path :__attrs (fix-fn attr-name) :__value)}))
                                  (get-attrs this))]
          (filter identity
                  (concat paths-to-attrs
                          (if (is-leaf? this)
                            [{:path (conj this-path :__value)}]
                            (flatten
                             (if-not (map? fields)
                               (map (fn [c]
                                      (get-paths c fix-fn this-path))
                                    fields))))))))

      (->mapping [this] (->mapping this tag-fix))
      (->mapping [this fix-fn]
        (let [m ((if (is-array? this)
                   vector
                   identity)
                 {(fix-fn tag)
                  (merge (if-let [attrs (not-empty
                                         (->> attrs
                                             (filter (fn [[_ attrv]]
                                                       (= "?" attrv)))
                                             (map (fn [[attr-name _]]
                                                    [(fix-fn attr-name) {:__value {:__type "string"}}]))
                                             (into {})))]
                           {:__attrs attrs}
                           {})
                         (apply merge
                                (-> this
                                    (get-fields)
                                    (->> (map (fn [c]
                                               (if (satisfies? Element c)
                                                 (->mapping c fix-fn)
                                                 (into {} [c]))))))))})]
          (if (vector? m)
            {(-> tag fix-fn plural) m}
            m)))

      (->template [this]
        (data-xml/indent-str
         (->template this true)))
      (->template [this root?]
        (let [origin-tag (get-tag this)
              tag (tag-fix (get-tag this))]
          (if root?
            (data-xml/element origin-tag
                              attrs
                              (conj (into [(data-xml/cdata (str "{% with ctx=" tag " %}"))]
                                          (map (fn [c]
                                                 (if (satisfies? Element c)
                                                   (->template c false)))
                                               (get-fields this)))
                                    (data-xml/cdata "{% endwith %}")))
            [(when (is-optional? this)
               (data-xml/cdata (str "{% if ctx." (if (is-array? this)
                                                   (plural tag)
                                                   tag)
                                    " %}")))
             (when (is-array? this)
               (data-xml/cdata (str "{% for item in ctx." (plural tag) " %}")))
             (when-not root?
               (data-xml/cdata (str "{% with ctx=" (if (is-array? this)
                                                     (str "item." tag)
                                                     (str "ctx." tag))
                                    " %}")))
             (data-xml/element origin-tag
                               (->> attrs
                                   (map (fn [[attr-name attr-value]]
                                          [attr-name (if (= attr-value "?")
                                                       (str "{{ctx.__attrs." (tag-fix attr-name) ".__value}}"))]))
                                   (into {}))
                               (let [fields (get-fields this)]
                                 (if (map? fields)
                                   (data-xml/cdata (str "{{ctx.__value}}"))
                                   (map (fn [c]
                                          (if (satisfies? Element c)
                                            (->template c false)
                                            (data-xml/cdata (str "{{ctx.__value}}"))))
                                        (get-fields this)))))
             (when-not root?
               (data-xml/cdata "{% endwith %}"))
             (when (is-array? this)
               (data-xml/cdata "{% endfor %}"))
             (when (is-optional? this)
               (data-xml/cdata "{% endif %}"))])))

      (->parse-fn [this]
        (fn [xml]
          (let [xml (-> xml string->stream (data-xml/parse :namespace-aware true) zip/xml-zip)]
            (apply deep-merge-with custom-merge
                   (map (partial xml->map xml)
                        (get-paths this))))))

      (is-optional? [_] (or optional (= min-occurs 0)))

      (is-array? [_] (boolean (or min-occurs (and max-occurs (> max-occurs 1)))))

      (is-leaf? [_] (or (empty? fields)
                        (contains? fields :__value)))

      (is-enum? [_] (boolean (not-empty enumeration))))))

(defn xml->element [msg]
  (when (not-empty msg)
    (node->element
     (data-xml/parse (java.io.StringReader. msg)
                     :namespace-aware false
                     :include-node? #{:element :characters :comment})
     []
     msg)))

(defn render-template [template context]
  (selmer/render template context))

(defn ->service [action version request-msg response-msg fault-msg]
  (let [request-element  (xml->element request-msg)
        response-element (xml->element response-msg)
        fault-element    (xml->element fault-msg)]
    (reify
      Service
      (content-type      [this]
        (case (soap-version this)
          "soap"   "text/xml"
          "soap12" (str "application/soap+xml;"
                        (when-not (empty? (soap-action this))
                          (format "action=\"%s\"" (soap-action this))))))
      (soap-headers      [this]
        (case (soap-version this)
          "soap"   {"SOAPAction" (soap-action this)}
          "soap12" {}))
      (soap-action       [_] action)
      (soap-version      [this]
        (if (keyword? version)
          (name version)
          version))

      (request-xml       [_] (get-original request-element))
      (request-mapping   [_] (->mapping request-element))
      (request-template  [_] (->template request-element))

      (wrap-body         [this context]
        (let [template (request-template this)]
          (render-template template context)))

      (wrap-response     [this context]
        (let [template (response-template this)]
          (render-template template context)))

      (wrap-fault     [this context]
        (let [template (fault-template this)]
          (render-template template context)))

      (response-xml      [_] (get-original response-element))
      (response-mapping  [_] (->mapping response-element))
      (response-template [_] (->template response-element))

      (parse-response    [this response-xml]
        (let [parse-fn (->parse-fn response-element)]
          (parse-fn response-xml)))

      (fault-xml         [_] (get-original fault-element))
      (fault-mapping     [_] (->mapping fault-element))
      (fault-template    [_] (->template fault-element))

      (parse-fault       [this fault-xml]
        (let [parse-fn (->parse-fn fault-element)]
          (parse-fn fault-xml))))))
