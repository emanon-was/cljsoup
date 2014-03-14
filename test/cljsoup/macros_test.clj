(ns cljsoup.macros-test
  (:use [cljsoup.macros])
  (:require [clojure.test :refer :all]
            [cljsoup.test-utils :refer :all]))

(defstrict test1 [] "test1")
(defstrict test2
  ([] "test2")
  ([String x] (str "test2:" x))
  ([Number x] (+ x x))
  ([String x String y] (str x ":" y))
  ([Number x Number y] (* x y))
  ([Number x Number y & plus] (+ x y (apply + plus)))
  ([String x String y & plus] (str x "/" y "/" (clojure.string/join "/" plus))))

(deftest classes?-test
  (testing "class?"
    (is (classes? '&))
    (is (classes? 'Nil))
    (is (classes? 'Function))
    (is (classes? 'List))
    (is (classes? 'Vector))
    (is (classes? 'HashMap))
    (is (classes? 'HashSet))
    (is (classes? 'Symbol))
    (is (classes? 'String))
    (is (classes? 'Number))
    (is (not (classes? 'x)))))

(deftest defstrict-test
  (testing "defstrict"
    (is (= (test1) "test1"))
    (is (= (test2) "test2"))
    (is (= (test2 "abc") "test2:abc"))
    (is (= (test2 1) 2))
    (is (error-is-true (test2 nil)))
    (is (error-is-true (test2 [])))
    (is (= (test2 "abc" "def") "abc:def"))
    (is (= (test2 "abc" "def" "ghi" "jkl") "abc/def/ghi/jkl"))
    (is (= (test2 1 2) 2))
    (is (= (test2 1 2 3) 6))))



