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

(defrecord Cljsoup [document selector])

(defn text-resource [path]
  (slurp (str (System/getProperty "user.dir") "/" path)))

(let [parser (Parser/xmlParser)]
  (defn parse [html]
    (Jsoup/parse html "String" parser)))

(defn parse! [html]
  (protocol/nodes (parse html)))

(defn select [selector nodes]
  (Selector/select selector nodes))

(defn select! [selector nodes]
  (map protocol/nodes (select selector nodes)))

(defn attr [attribute value]
  #(.attr % attribute value))

(def html (parse* (text-resource "resources/test.html")))

