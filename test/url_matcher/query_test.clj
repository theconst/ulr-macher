(ns url-matcher.query-test
  (:require [clojure.test :refer :all]
            [url-matcher.query :as q :refer :all]))

(def truthy? (comp false? not))
(def falsy? (comp true? not))

(deftest clause-children-suite
  (testing "Clause children"
    (are [children expression] (= children (clause-children expression))
      '("v") '(::q/variable "v")
      '() nil
      '() '(:query)
      '((::q/literal "host") "()") '(::q/section (::q/literal "host") "()"))))

(deftest clause-type-suite
  (testing "Clause type suite"
    (are [t expression] (= t (clause-type expression))
      ::q/empty '()
      ::q/compound '((::q/section "s") (:host "h"))
      ::q/variable '(::q/variable "v")
      ::q/section '(::q/section (::q/literal "host"))
      ::q/literal '(::q/literal "l")
      ::q/name '(::q/name "n"))))

(deftest predicates-suite
  (testing "variable? predicate"
    (is (truthy? (variable? '(::q/variable (::q/name "var")))))
    (is (falsy? (variable? "string")))
    (is (falsy? (variable? '(::q/section (::q/literal "host") "(" ")")))))

  (testing "section? predicate"
    (is (truthy? (section? '(::q/section (::q/literal "host") "(" ")")))))
  (is (falsy? (section? '(::q/variable (::q/name "v"))))
      (is (falsy? (section? "string"))))

  (testing "name? predicate"
    (is (truthy? (name? '(::q/name "value"))))
    (is (falsy? (name? '(::q/section "section"))))
    (is (falsy? (name? "name")))))

(deftest remove-atoms-suite
  (testing "Tree only with compound node"
    (is (= '(::q/variable (::q/name "name"))
           (remove-atoms '(::q/variable (::q/name "name"))))))
  (testing "Tree with mixed node types"
    (is (= '(::q/variable (::q/name "name"))
           (remove-atoms '(::q/variable "[" (::q/name "name") "]")))))
  (testing "Tree with noise only"
    (is (= '(::q/variable)
           (remove-atoms '(::q/variable "[" "name" "]"))))))

(deftest pull-name-suitex
  (testing "Name is pulled if present"
    (is (= '(::q/variable "name")
           (pull-name '(::q/variable "[" (::q/name "name") "]")))))
  (testing "Excepition is thrown if name is ablent"
    (is (thrown? IllegalStateException (pull-name (::q/section "host"))))))

(deftest query-parser-suite
  (testing "Host queries"
    (is (= '(((::q/section "host") (::q/literal "google.com")))
           (parse "host(google.com)")))
    (is (= '(((::q/section "host") (::q/literal "www.")
                                   (::q/variable "dot-com")
                                   (::q/literal ".com"))))
        (parse "host(www.?[dot-com].com);"))
    (is (= '(((::q/section "host") (::q/variable "prefix")
                                   (::q/literal ".")
                                   (::q/variable "host")
                                   (::q/literal ".")
                                   (::q/variable "domain")))
           (parse "host(?[prefix].?[host].?[domain])"))))

  (testing "Path queries"
    (is (= '(((::q/section "path") (::q/variable "root")
                                   (::q/literal "/subroot")))
           (parse "path(?root/subroot)")))
    (is (= '(((::q/section "path") (::q/variable "duplicate")
                                   (::q/literal "/")
                                   (::q/variable "duplicate")))
           (parse "path(?duplicate/?[duplicate])")))
    (is (= '(((::q/section "path") (::q/literal "/home/")
                                   (::q/variable "user-name")
                                   (::q/literal "/")))
           (parse "path(/home/?user-name/);"))))

  (testing "Queryparam queries"
    (is (= '(((::q/section "queryparam") (::q/literal "query=wolf")))
           (parse "queryparam(query=wolf)")))
    (is (= '(((::q/section "queryparam") (::q/literal "q=")
                                         (::q/variable "query")
                                         (::q/literal "&offset=")
                                         (::q/variable "offset")))
           (parse "queryparam(q=?query&offset=?offset);"))))

  (testing "Compound queries"
    (is (= '(((::q/section "host") (::q/literal "sample.org"))
             ((::q/section "path") (::q/variable "where"))
             ((::q/section "queryparam") (::q/literal "q=bread&q=")
                                         (::q/variable "on-bread")))
           (parse "host(sample.org);
                   path(?[where]);
                   queryparam(q=bread&q=?[on-bread]);"))))

  (testing "Empty input"
    (is (empty? (parse "")) "Empty input should yield empty result"))

  (testing "Illegal input"
    (are [p d] (is (thrown? IllegalArgumentException p) d)
      (parse "path(q/?r") "Not matched parenthesis"
      (parse "host(alphabet.inc);;;") "Duplicate delimiter")))
