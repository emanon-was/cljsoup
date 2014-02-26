(ns org.cljsoup
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup.core :as hiccup])
  (:use [org.cljsoup.defstrict])
  (:import [clojure.lang PersistentVector]
           [java.net URL]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]
           [org.jsoup.parser Parser]))


;;
;; Utils
;;

(defstrict url [String url]
  (io/as-url url))

(defstrict file-resource [String path]
  (slurp (str (System/getProperty "user.dir") "/" path)))

(defn- ns-keyword []
  (keyword (ns-name *ns*)))

(defmacro ref-set! [name & body]
  `(dosync (ref-set ~name ~@body)))

;;
;; Document methods
;;

(defstrict parse
  ([String html] (Jsoup/parse html "String" (Parser/xmlParser)))
  ([URL url] (Jsoup/parse (io/as-url url) 60000)))

(defstrict html-expand
  ([Element e] (.toString e))
  ([Elements e] (.toString e)))

(defstrict clone [Element e] (.clone e))

(defstrict select [Element e String selector]
  (.select e selector))


;;
;; Element methods
;;

(defstrict add-class
  ([String class-name]
     #(add-class % class-name))
  ([Element element String class-name]
     (.addClass element class-name)))

(defstrict after [String html]
  #(.after % html))

(defstrict before [String html]
  #(.before % html))

(defstrict append [String html]
  #(.append % html))

(defstrict prepend [String html]
  #(.prepend % html))

(defstrict attr
  ([String attribute]
     #(.attr % attribute))
  ([String attribute String value]
     #(.attr % attribute value)))

(defstrict html
  ([]
     #(.html %))
  ([String html]
     #(.html % html))
  ([PersistentVector html]
     (hiccup/html html)))

(defstrict replace [String html]
  #(-> % (.html html) .unwrap))

(defstrict empty []
  #(.empty %))

(defstrict first-element []
  #(.first %))

;;
;; Interface
;;

(defn- selection* [document selector-and-method]
  (let [document (clone document)
        selector (first selector-and-method)
        method   (second selector-and-method)
        elements (select document selector)]
    (if (nil? method)
      elements
      (map method elements))))

(defn selection [document selector-and-method & more]
  (if (empty? more)
    (selection* document selector-and-method)
    (map
     #(selection* document %)
     (cons selector-and-method more))))

(defn- transform* [document selector-and-method]
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






;; --------------------------------------
;;
;;  以下テンプレートエンジン部分製作中
;;
;;


;;
;; Options
;;

(def template-mode   (ref {}))
(def template-prefix (ref {}))
(def template-cache  (ref {}))

(def template-ns (ns-keyword))
(ref-set! template-mode   (assoc @template-mode template-ns :development))
(ref-set! template-prefix (assoc @template-prefix template-ns nil))
(ref-set! template-cache  (assoc @template-cache template-ns nil))

;;
;; Privates
;;

(defn- use-mode []
  (let [nskey (ns-keyword)]
    (or (@template-mode nskey)
        (@template-mode template-ns))))

(defn- use-prefix []
  (let [nskey (ns-keyword)]
    (or (@template-prefix nskey)
        (@template-prefix template-ns))))

(defn- use-cache []
  (let [nskey (ns-keyword)]
    (or (@template-cache nskey)
        (@template-cache template-ns))))

;;
;; Etc
;;

(def abc (parse (file-resource "resources/test.html")))

(defmacro time100 [& body]
  (let [i (gensym)]
    `(time (dotimes [~i 100]
             ~@body))))

(defmacro time1000 [& body]
  (let [i (gensym)]
    `(time (dotimes [~i 1000]
             ~@body))))
