(ns cljsoup.macros-test
  (:use )
  (:require [clojure.test :refer :all]
            [org.cljsoup.macros :refer :all]))

(def Nil 'Nil)
(def Function 'Function)

(deftest classes?-test
  (testing "class?-test Missing"
    (is (not (classes? '&)))
    (is (classes? Nil))
    (is (classes? Function))
    (is (classes? String))
    (is (not (classes? 1)))))

(deftest classes-test
  (testing "class-test Missing"
    (is (= Long (classes 1)))
    (is (= String (classes "s")))
    (is (= Function (classes #(str 1))))
    (is (= Nil (classes nil)))))


