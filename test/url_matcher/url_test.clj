(ns url-matcher.url-test
  (:require [clojure.test :refer :all]
            [url-matcher.url :refer :all])
  (:import (java.net MalformedURLException)))

(deftest url-parse-suite
  (testing "Parsing of valid URLs"
    (is (= (parse "https://google.com?q=wolf&p=2")
           (->URL "https" "google.com" ""
                  (->URLQuery "q=wolf&p=2" ["q=wolf" "p=2"]))))
    (is (= (parse "https://gmail.com/inbox")
           (->URL "https" "gmail.com" "/inbox" nil)))
    (is (= (parse "http://neverland.com?some%20query")
           (->URL "http" "neverland.com" "" (->URLQuery "some%20query" ["some%20query"]))))
    (is (= (parse "file:///C/Users/Documents")
           (->URL "file" "" "/C/Users/Documents" nil))))
  (testing "Parsing of invalid URLs"
    (is (thrown? MalformedURLException (parse "mmm://mmm.org")))))
