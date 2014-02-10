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

(defrecord DocRelation [document where update delete])

(defn parse [html]
  (map->DocRelation {:document (Jsoup/parse html "String" (Parser/xmlParser))}))

(defn clone [soup]
  (map->DocRelation {:document (.clone (:document soup))}))

(defn select [selector soup]
  (let [s (clone soup)]
    (assoc s :selector selector)))

(defn select! [selector soup]
  (let [s (select selector soup)]
    (Selector/select (:selector s) (:document s))))
    
(defn select* [soup])

(defn select [soup]
  
  )

(defn select! [selector nodes]
  (map protocol/nodes (select selector nodes)))

(defn attr
  ([attribute] #(.attr % attribute))
  ([attribute value] #(.attr % attribute value)))

(def html (parse (file-resource "resources/test.html")))

