(ns url-matcher.util-test
  "Common utilitis that cannot be otherwise categorised"
  (:require [clojure.test :refer :all]
            [url-matcher.util :refer :all]
            [clojure.zip :as zip]))


(deftest zip-transform-suite
  (testing "Transformations using zipper"
    (is (= [["bazz"]]
           (zip-transform {:zipper zip/vector-zip
                           :guard (partial = "fizz")
                           :editor (constantly "bazz")
                           :root [["fizz"]]})))
    (is (= ["bazz" ["bazz"]]
           (zip-transform {:zipper zip/vector-zip
                           :guard (partial = "fizz")
                           :editor (constantly "bazz")
                           :root ["fizz" ["fizz"]]})))

    (is (= ["bazz" ["buzz"]]
           (zip-transform-> {:zipper zip/vector-zip, :root ["fizz" ["fizz"]]}
             {:guard #{"fizz"}, :editor (constantly "bazz")}
             {:guard #{["bazz"]}, :editor (constantly ["buzz"])})))))
