(ns org.cljsoup.protocol
  (:gen-class)
  (:import [org.jsoup.nodes Attribute Attributes Document Element TextNode]))

(defn- keyword-name [name]
  (-> name .toString .toLowerCase keyword))

(defprotocol Cljsoup
  (nodes [d] (map nodes d)))

(extend-protocol Cljsoup
  Attribute
  (nodes [a] [(keyword-name (.getKey a)) (.getValue a)])

  Attributes
  (nodes [as] (not-empty (into {} (map nodes as))))

  Document
  (nodes [d] (not-empty (map nodes (.childNodes d))))

  Element
  (nodes [e] {:tag (keyword-name (.tagName e))
              :attrs (nodes (.attributes e))
              :content (not-empty (map nodes (.childNodes e)))})
  TextNode
  (nodes [tn] (.getWholeText tn))

  nil
  (nodes [_] nil))

