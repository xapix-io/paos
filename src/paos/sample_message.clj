(ns paos.sample-message
  (:require [clojure.data.xml :as data-xml]
            [clojure.string :as string]
            [inflections.core :refer [plural]]
            [clojure.xml :as xml]))

(declare node->element)

(defn tag-fix [tag-name]
  (cond-> tag-name
    (keyword? tag-name) name
    true (string/split #":")
    true last))

(defn parse-comment [comment]
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
    (let [matcher (re-matches #"(type: ([a-zA-Z]+)|anonymous type) - enumeration: \[(.*)\]" comment)
          enumeration (mapv string/trim
                             (string/split (nth matcher 3)
                                          #","))
          type (string/trim (or (nth matcher 2) "anonymous type"))]
      {:enumeration enumeration
       :type type})

    :otherwise
    nil))

(defprotocol Element
  (get-original [this])
  (get-tag [this])
  (get-fields [this])
  (get-attrs [this])
  (get-type [this])
  (get-path [this] [this fix-fn])
  (get-paths [this] [this fix-fn] [this fix-fn path])
  (->mapping [this] [this fix-fn])
  (->template [this] [this root?])
  (->parse-fn [this])
  (is-optional? [this])
  (is-array? [this])
  (is-leaf? [this])
  (is-enum? [this]))

(defn content->fields [content type]
  (let [content (filter #(not (string/starts-with? % "\n")) content)]
    (loop [el (first content) els (rest content) comments [] fields []]
      (if-not el
        fields
        (cond
          (string? el)
          {:__value nil
           :__type type}

          (instance? clojure.data.xml.node.Comment el)
          (recur (first els) (rest els) (conj comments (:content el)) fields)

          :otherwise
          (recur (first els) (rest els) [] (conj fields (node->element el comments nil))))))))

(defn node->element [{:keys [tag attrs content]
                      :or {content '()
                           attrs {}}}
                     comments
                     original-xml]
  (let [{:keys [type min-occurs max-occurs
                optional enumeration]} (into {} (map parse-comment comments))
        fields (content->fields content type)]
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
        (let [fields (get-fields this)
              this-path (apply (partial conj path) (get-path this fix-fn))
              paths-to-attrs (map (fn [[attr-name attr-value]]
                                    (when (= attr-value "?")
                                      {:path (conj this-path :__attrs (fix-fn attr-name))}))
                                  (get-attrs this))]
          (filter identity
                  (concat paths-to-attrs
                          (if (is-leaf? this)
                            [{:path this-path}]
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
                 {(fix-fn tag) (merge (if-let [attrs (not-empty
                                                      (->> attrs
                                                          (filter (fn [[_ attrv]]
                                                                    (= "?" attrv)))
                                                          (map (fn [[attr-name _]]
                                                                 [(fix-fn attr-name) {:__value nil
                                                                                      :__type "string"}]))
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
        (let [tag (tag-fix (get-tag this))]
          (if root?
            (data-xml/element tag
                              attrs
                              (conj (into [(data-xml/cdata (str "{% with ctx=" tag " %}"))]
                                          (map (fn [c]
                                                 (if (satisfies? Element c)
                                                   (->template c false)))
                                               (get-fields this)))
                                    (data-xml/cdata "{% endwith %}")))
            [(when (is-optional? this)
               (data-xml/cdata "{% if ctx %}"))
             (when (is-array? this)
               (data-xml/cdata (str "{% for item in ctx." (plural tag) " %}")))
             (when-not root?
               (data-xml/cdata (str "{% with ctx=" (if (is-array? this)
                                                     (str "item." tag)
                                                     (str "ctx." tag))
                                    " %}")))
             (data-xml/element tag
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

      (->parse-fn [_])

      (is-optional? [_] (or optional (= min-occurs 0)))

      (is-array? [_] (boolean (or min-occurs (and max-occurs (> max-occurs 1)))))

      (is-leaf? [_] (or (empty? fields)
                        (contains? fields :__value)))

      (is-enum? [_] (boolean (not-empty enumeration))))))

(defn xml->element [msg]
  (node->element
   (data-xml/parse (java.io.StringReader. msg)
                   :namespace-aware false
                   :include-node? #{:element :characters :comment})
   []
   msg))


(comment

  (def x "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"
                               xmlns:book=\"http://www.cleverbuilder.com/BookService/\">
                 <soapenv:Header/>
                     <soapenv:Body>
                         <book:GetSomeBooks>
                             <!--1 or more repetitions:-->
                             <BookId BookType=\"?\">
                                 <!--type: string-->
                                 <ID>?</ID>
                                 <Type>
                                     <!--type: integer-->
                                     <SubType>?</SubType>
                                 </Type>
                             </BookId>
                             <!--type: string-->
                             <RequestId>?</RequestId>
                             <!--1 or more repetitions:-->
                             <!--type: string-->
                             <ArrayId x=\"?\" y=\"?\">?</ArrayId>
                         </book:GetSomeBooks>
                     </soapenv:Body>
                 </soapenv:Envelope>")

  (def y (xml->element x))

  (is-array? y)
  (is-optional? y)
  (is-leaf? y)



  (check y)

  (clojure.pprint/pprint
   (->mapping y))

  (println (data-xml/indent-str (->template y true)))

  (clojure.pprint/pprint
   (get-paths y tag-fix)
   )

  (get-original y)

  )

(defn check [c]
  (if (satisfies? Element c)
    [(is-array? c)
     (is-leaf? c)
     (is-optional? c)
     (get-tag c)
     (get-path c)
     (when-let [cs (get-fields c)]
       (map check cs))]))
