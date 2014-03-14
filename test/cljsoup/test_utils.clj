(ns cljsoup.test-utils
  (:require [clojure.test :refer :all]))

(defmacro error-is-true [& body]
  (try
    ~@body
    (catch Exception e true)))

