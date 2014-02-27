(ns org.cljsoup
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hiccup.core :as hiccup])
  (:use
   [org.cljsoup.macros])
  (:import
   ;; default
   [java.net URL]
   [clojure.lang PersistentVector]
   ;; jsoup
   [org.jsoup Jsoup]
   [org.jsoup.nodes Element]
   [org.jsoup.select Elements]
   [org.jsoup.parser Parser]))

;;
;; Utils
;;

(defstrict url [String s]
  (io/as-url s))

(defstrict file [String s]
  (io/as-file s))

(defstrict file-resource [String s]
  (slurp (str (System/getProperty "user.dir") "/" s)))

(defstrict parse
  ([URL u] (Jsoup/parse (io/as-url u) 60000))
  ([String s] (Jsoup/parse s "String" (Parser/xmlParser)))
  ([Element e] (.toString e))
  ([Elements e] (.toString e)))

;;
;; Element methods
;;

(defstrict clone
  ([] #(clone %))
  ([Element e] (.clone e))
  ([Elements e] (.clone e)))

(defstrict select
  ([String s] #(select % s))
  ([Element e String s] (.select e s))
  ([Elements e String s] (.select e s))
  ([Element e String s PersistentVector v]
     (let [e (clone e) t (select e s)]
       ((apply comp (reverse v)) t)))
  ([Elements e String s PersistentVector v]
     (let [e (clone e) t (select e s)]
       ((apply comp (reverse v)) t))))

(defstrict html
  ([PersistentVector v] (hiccup/html v))
  ([] #(html %))
  ([String s] #(html % s))
  ([Element e] (.html e))
  ([Elements e] (.html e))
  ([Element e String s] (.html e s))
  ([Elements e String s] (.html e s)))

(defstrict after
  ([String s] #(after % s))
  ([Element e String s] (.after e s))
  ([Elements e String s] (.after e s)))

(defstrict before
  ([String s] #(before % s))
  ([Element e String s] (.before e s))
  ([Elements e String s] (.before e s)))
  
(defstrict append
  ([String s] #(append % s))
  ([Element e String s] (.append e s))
  ([Elements e String s] (.append e s)))

(defstrict prepend
  ([String s] #(prepend % s))
  ([Element e String s] (.prepend e s))
  ([Elements e String s] (.prepend e s)))

(defstrict add-class
  ([String s] #(add-class % s))
  ([Element e String s] (.addClass e s))
  ([Elements e String s] (.addClass e s)))

(defstrict attr
  ([String s] #(attr % s))
  ([Element e String s] (.attr e s))
  ([Elements e String s] (.attr e s))
  ([String a String v] #(attr % a v))
  ([Element e String a String v] (.attr e a v))
  ([Elements e String a String v] (.attr e a v)))

(defstrict replace
  ([String s] #(replace % s))
  ([Element e String s] (.unwrap (.html e s)))
  ([Elements e String s] (.unwrap (.html e s))))

(defstrict empty
  ([] #(empty %))
  ([Element e] (.empty e))
  ([Elements e] (.empty e)))

(defstrict first-element
  ([] #(first-element %))
  ([Element e] (.first e))
  ([Elements e] (.first e)))

(defstrict last-element
  ([] #(last-element %))
  ([Element e] (.last e))
  ([Elements e] (.last e)))

;;
;; Interface
;;

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


(defn- ns-keyword []
  (keyword (ns-name *ns*)))

(defmacro ref-set! [name & body]
  `(dosync (ref-set ~name ~@body)))


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
