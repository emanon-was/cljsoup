(ns org.cljsoup
  (:gen-class)
  (:require [clojure.java.io :as io]
            [org.cljsoup.protocol :as protocol]
            [org.cljsoup.inherit  :as inherit]
            [hiccup.core :as hiccup])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Attribute Attributes Document Element TextNode Node]
           [org.jsoup.parser Parser]
           [org.jsoup.select Selector]))

(inherit/inherit-ns 'hiccup.core)

(defn file-resource [path]
  (slurp (str (System/getProperty "user.dir") "/" path)))

;;(defprotocol Transform [selector alter])

(defn parse [html]
  (Jsoup/parse html "String" (Parser/xmlParser)))

(defn clone [document]
  (.clone document))

(defn select [selector document]
  (.select document selector))
    
(defn attr
  ([attribute] #(.attr % attribute))
  ([attribute value] #(.attr % attribute value)))

(defn transform* [document selector-and-method]
  (let [document (clone document)
        selector (first selector-and-method)
        method   (second selector-and-method)]
    (do
      (method (select selector document))
      document)))

(defn transform [document & selector-and-method]
  (println selector-and-method)
  (if selector-and-method
    (recur (transform* document (first selector-and-method))
           (rest selector-and-method))
    document))
  
(def html (parse (file-resource "resources/test.html")))


