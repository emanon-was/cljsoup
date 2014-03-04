(ns cljsoup.template
  (:gen-class)
  (:use [cljsoup.macros]
        [cljsoup.core]))

(let [abcdefg (parse (local-file "resources/test.html"))]
  (defn test-html []
    (-> abcdefg
     (transform "div#test" (add-class "asdf"))
     (transform "clojure" (html (html [:div.htmltest "action"]))))))
  
;; (defmacro time100 [& body]
;;   (let [i (gensym)]
;;     `(time (dotimes [~i 100]
;;              ~@body))))

;; (defmacro time1000 [& body]
;;   (let [i (gensym)]
;;     `(time (dotimes [~i 1000]
;;              ~@body))))

