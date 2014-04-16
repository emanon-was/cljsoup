(ns cljsoup.core
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hiccup.core :as hiccup])
  (:use
   [cljsoup.macros])
  (:import
   ;; default
   [java.net URL]
   [java.io File]
   ;; jsoup
   [org.jsoup Jsoup]
   [org.jsoup.nodes
    Attribute Attributes Comment DataNode Document
    DocumentType Element Node TextNode]
   [org.jsoup.select Elements]
   [org.jsoup.parser Parser Tag]))


;;
;; for exports
;;

(let [ns *ns*]
  (defmacro inheritance []
    `(ns-inheritance ~(ns-name ns)))
  (defmacro uninheritance []
    `(ns-uninheritance ~(ns-name ns))))

;;
;; Utils
;;

(defstrict url [:String s]
  (io/as-url s))

(defstrict file [:String s]
  (io/file s))

(let [local-dir (System/getProperty "user.dir")]
  (defstrict local-file [:String s]
    (io/file local-dir s)))

(let [parser (Parser/xmlParser)]
  (defstrict parse
    ([:URL u] (Jsoup/parse (slurp u) (.toString u) parser))
    ([:File f] (Jsoup/parse (slurp f) (.toString f) parser))
    ([:String s] (Jsoup/parse s "String" parser))
    ([:Vector v] (Jsoup/parse (hiccup/html v) "Sexp" parser))))

(defstrict expand
  ([:Vector v] (hiccup/html v))
  ([:Element e] (.toString e))
  ([:Elements e] (.toString e)))

;;
;; Enlive nodes
;;

(defstrict ^:private keysym [:String s]
  (keyword (.. s toLowerCase)))

(defstrict nodes
  ([:Nil _] nil)
  ([:String s] (nodes (parse s)))
  ([:List l] (map nodes l))
  ([:Vector v] (map nodes v))
  ([:HashMap m] m)
  ([:Attribute a] [(keysym (.getKey a)) (.getValue a)])
  ([:Attributes as] (not-empty (into {} (map nodes as))))
  ([:Comment c] {:type :comment :data (.getData c)})
  ([:DataNode dn] (str dn))
  ([:Document d] (not-empty (map nodes (.childNodes d))))
  ([:DocumentType dtd]
     {:type :dtd :data
      ((juxt :name :publicid :systemid)
       (nodes (.attributes dtd)))})
  ([:Element e]
     {:tag (keysym (.tagName e))
      :attrs (nodes (.attributes e))
      :content (not-empty (map nodes (.childNodes e)))})
  ([:Elements e] (map nodes e))
  ([:TextNode tn] (.getWholeText tn)))


;;
;; Jsoup methods
;;

(defstrict clone
  ([] #(clone %))
  ([:Element e] (.clone e))
  ([:Elements e] (.clone e)))

(defstrict select
  ([:String s] #(select % s))
  ([:Element e :String s] (.select (clone e) s))
  ([:Elements e :String s] (.select (clone e) s)))

(defstrict html
  ([] #(html %))
  ([:String s] #(html % s))
  ([:Element e] (.html e))
  ([:Elements e] (.html e))
  ([:Element e :String s] (.html e s))
  ([:Elements e :String s] (.html e s)))

(defstrict outer-html
  ([] #(outer-html %))
  ([:Element e] (.outerHtml e))
  ([:Elements e] (.outerHtml e)))

(defstrict replace-with
  ([:String s] #(replace-with % s))
  ([:Element e :String s] (.unwrap (.html e s)))
  ([:Elements e :String s] (map (replace-with s) e)))

(defstrict after
  ([:String s] #(after % s))
  ([:Element e :String s] (.after e s))
  ([:Elements e :String s] (.after e s)))

(defstrict before
  ([:String s] #(before % s))
  ([:Element e :String s] (.before e s))
  ([:Elements e :String s] (.before e s)))
  
(defstrict append
  ([:String s] #(append % s))
  ([:Element e :String s] (.append e s))
  ([:Elements e :String s] (.append e s)))

(defstrict prepend
  ([:String s] #(prepend % s))
  ([:Element e :String s] (.prepend e s))
  ([:Elements e :String s] (.prepend e s)))

(defstrict add-class
  ([:String s] #(add-class % s))
  ([:Element e :String s] (.addClass e s))
  ([:Elements e :String s] (.addClass e s)))

(defstrict remove-class
  ([:String s] #(remove-class % s))
  ([:Element e :String s] (.removeClass e s))
  ([:Elements e :String s] (.removeClass e s)))

(defstrict toggle-class
  ([:String s] #(toggle-class % s))
  ([:Element e :String s] (.toggleClass e s))
  ([:Elements e :String s] (.toggleClass e s)))

(defstrict attrs
  ([] #(.attributes %))
  ([:Element e] (.attributes e))
  ([:Elements e] (map attrs e)))

(defstrict attr
  ([:String s] #(attr % s))
  ([:Element e :String s] (.attr e s))
  ([:Elements e :String s] (map (attr s) e))
  ([:String a :String v] #(attr % a v))
  ([:Element e :String a :String v] (.attr e a v))
  ([:Elements e :String a :String v] (.attr e a v)))

(defstrict remove-attr
  ([:String s] #(remove-attr % s))
  ([:Element e :String s] (.removeAttr e s))
  ([:Elements e :String s] (.removeAttr e s)))

(defstrict parent
  ([] #(parent %))
  ([:Element e] (first (.parents e)))
  ([:Elements e] (first (.parents e))))

(defstrict children
  ([] #(children %))
  ([:Element e] (vec (.childNodes e)))
  ([:Elements e] (map children e)))

(defstrict remove-all
  ([] #(remove-all %))
  ([:Element e] (.empty e))
  ([:Elements e] (.empty e)))

(defstrict first-element
  ([] #(first-element %))
  ([:Element e] (.first e))
  ([:Elements e] (.first e)))

(defstrict last-element
  ([] #(last-element %))
  ([:Element e] (.last e))
  ([:Elements e] (.last e)))

(defstrict text
  ([] #(text %))
  ([:Element e] (.text e))
  ([:Elements e] (.text e)))

(defstrict value
  ([] #(value %))
  ([:String s] #(value % s))
  ([:Element e] (.val e))
  ([:Elements e] (.val e))
  ([:Element e :String s] (.val e s))
  ([:Elements e :String s] (.val e s)))

(defstrict wrap
  ([:String s] #(wrap % s))
  ([:Element e :String s] (.wrap e s))
  ([:Elements e :String s] (.wrap e s)))

(defstrict unwrap
  ([] #(unwrap %))
  ([:Element e] (.unwrap e))
  ([:Elements e] (.unwrap e)))

(defstrict transform
  ([:String s :Function f] #(transform % s f))
  ([:Element e :String s :Function f]
     (let [e (clone e)]
       (-> e (.select s) f) e))
  ([:Elements e :String s :Function f]
     (let [e (clone e)]
       (-> e (.select s) f) e)))


