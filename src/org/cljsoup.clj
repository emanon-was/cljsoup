(ns org.cljsoup
  (:gen-class)
  (:require [clojure.java.io :as io]
            [hiccup.core :as hiccup])
  (:import [org.jsoup Jsoup]
           [org.jsoup.parser Parser]))


(defn file-resource [#^String path]
  (slurp (str (System/getProperty "user.dir") "/" path)))

(defn parse [#^String html]
  (Jsoup/parse html "String" (Parser/xmlParser)))

(defn html-expand [#^Element document]
  (.toString document))

(defn clone [#^Element document]
  (.clone document))

(defn select [#^Element document #^String selector]
  (.select document selector))

(defn selection* [#^Element document selector-and-method]
  (let [document (clone document)
        selector (first selector-and-method)
        method   (second selector-and-method)
        elements (select document selector)]
    (if (nil? method)
      elements
      (map method elements))))

(defn selection [#^Element document selector-and-method & more]
  (if (empty? more)
    (selection* document selector-and-method)
    (map
     #(selection* document %)
     (cons selector-and-method more))))

(defn attr [#^String attribute]
  #(.attr % attribute))

(defn transform* [document selector-and-method]
  (let [document (clone document)
        selector (first selector-and-method)
        method   (second selector-and-method)]
    (method (select document selector))
    document))

(defn transform [document & selector-and-method]
  (if (empty? selector-and-method)
    document
    (recur (transform* document (first selector-and-method))
           (rest selector-and-method))))

(defn add-class [class-name]
  #(.addClass % class-name))

(defn after [html]
  #(.after % html))

(defn before [html]
  #(.before % html))

(defn attr [attribute value]
  #(.attr % attribute value))

  
(def html (parse (file-resource "resources/test.html")))




(defn- inherit-var [var]
  (eval
   (let [meta-data (meta var)
         name      (meta-data :name)
         macro     (meta-data :macro)
         dynamic   (meta-data :dynamic)]
     `(def ~(with-meta name
              (assoc (meta name)
                :macro   macro
                :dynamic dynamic)) ~var))))

(defn- inherit-ns [ns]
  (let [vars (vals (ns-publics ns))]
    (doseq [v vars] (inherit-var v))))

(inherit/inherit-ns 'hiccup.core)
