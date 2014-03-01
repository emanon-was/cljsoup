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
   [java.io File]
   ;; jsoup
   [org.jsoup Jsoup]
   [org.jsoup.nodes Node]
   [org.jsoup.select Elements]
   [org.jsoup.parser Parser]))

;;
;; Utils
;;

(defstrict url [String s]
  (io/as-url s))

(defstrict file [String s]
  (io/as-file s))

(defstrict local-file [String s]
  (file (str (System/getProperty "user.dir") "/" s)))

(defstrict parse
  ([URL u] (Jsoup/parse (slurp u) (.toString u) (Parser/xmlParser)))
  ([File f] (Jsoup/parse (slurp f) (.toString f) (Parser/xmlParser)))
  ([String s] (Jsoup/parse s "String" (Parser/xmlParser)))
  ([Node n] (.toString n))
  ([Elements e] (.toString e)))

;;
;; Node methods
;;

(defstrict clone
  ([] #(clone %))
  ([Node n] (.clone n))
  ([Elements e] (.clone e)))

(defstrict select
  ([String s] #(select % s))
  ([Node n String s & l]
     (if (not l)
       (.select n s)
       (let [n (clone n) t (select n s)]
         ((apply comp (reverse l)) t))))
  ([Elements e String s & l]
     (if (not l)
       (.select e s)
       (let [e (clone e) t (select e s)]
         ((apply comp (reverse l)) t)))))

(defstrict html
  ([Vector v] (hiccup/html v))
  ([] #(html %))
  ([String s] #(html % s))
  ([Node n] (.html n))
  ([Elements e] (.html e))
  ([Node n String s] (.html n s))
  ([Elements e String s] (.html e s)))

(defstrict after
  ([String s] #(after % s))
  ([Node n String s] (.after n s))
  ([Elements e String s] (.after e s)))

(defstrict before
  ([String s] #(before % s))
  ([Node n String s] (.before n s))
  ([Elements e String s] (.before e s)))
  
(defstrict append
  ([String s] #(append % s))
  ([Node n String s] (.append n s))
  ([Elements e String s] (.append e s)))

(defstrict prepend
  ([String s] #(prepend % s))
  ([Node n String s] (.prepend n s))
  ([Elements e String s] (.prepend e s)))

(defstrict add-class
  ([String s] #(add-class % s))
  ([Node n String s] (.addClass n s))
  ([Elements e String s] (.addClass e s)))

(defstrict attr
  ([String s] #(attr % s))
  ([Node n String s] (.attr n s))
  ([Elements e String s] (.attr e s))
  ([String a String v] #(attr % a v))
  ([Node n String a String v] (.attr n a v))
  ([Elements e String a String v] (.attr e a v)))

(defstrict replace
  ([String s] #(replace % s))
  ([Node e String s] (.unwrap (.html e s)))
  ([Elements e String s] (.unwrap (.html e s))))

(defstrict empty
  ([] #(empty %))
  ([Node n] (.empty n))
  ([Elements e] (.empty e)))

(defstrict first-element
  ([] #(first-element %))
  ([Node n] (.first n))
  ([Elements e] (.first e)))

(defstrict last-element
  ([] #(last-element %))
  ([Node n] (.last n))
  ([Elements e] (.last e)))

;;
;; Interface
;;

(defstrict transform
  ([Node n String s Function f] (select n s f)))


;; (defn- transform* [document selector-and-method]
;;   (let [document (clone document)
;;         selector (first selector-and-method)
;;         method   (second selector-and-method)]
;;     (method (select document selector))
;;     document))

;; (defn transform [document & selector-and-method]
;;   (if (empty? selector-and-method)
;;     document
;;     (recur (transform* document (first selector-and-method))
;;            (rest selector-and-method))))


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

