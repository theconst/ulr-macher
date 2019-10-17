(ns url-matcher.matcher-test
  (:require [clojure.test :exclude [successful?] :refer :all]
            [url-matcher.matcher :as matcher :refer :all]
            [url-matcher.query :as query]
            [clojure.string :as str]))

(defn pattern [& args]
  "Compiles pattern from arguments (pattern is variable if starts with '?')"
  (mapv #(let [n (name %1)]
            (cond
              (str/starts-with? n "?")
              (query/make-clause ::query/variable [(subs n 1)])

              (str/starts-with? n "->")
              (query/make-clause ::query/section [(subs n 2)])

              :else
              (query/make-clause ::query/literal [(str n)])))
        args))

(def m+ ^{:doc "Alias for `match-success`"} match-success)
(def m- ^{:doc "Alias for `match-failure`"} match-failure)

(deftest variable-mathching-suite
  (testing "Simple variable matching"
    (are [expected expression]
      (= expected (match-pattern {} (pattern :?v) expression))

      (match-success {"v" ""} "")
      ""

      (concat (m- {"v" ""} "ab" "Expected end, but was 'ab'")
              (m- {"v" "a"} "b" "Expected end, but was 'b'")
              (m+ {"v" "ab"} ""))
      "ab"))
  (testing "Ambigous variable matching"
    (is (= (concat (m- {"v" "a"} "b" "Ambiguity for 'v' (old='a', new='')")
                   (m- {"v" "a"} "b" "Ambiguity for 'v' (old='a', new='b')"))
           (match-pattern {"v" "a"} (pattern :?v) "b")))
    (is (= (concat (m- {"v" "a"} "a" "Ambiguity for 'v' (old='a', new='')")
                   (m+ {"v" "a"} ""))
           (match-pattern {"v" "a"} (pattern :?v) "a")))))

(deftest literal-matching-suite
  (testing "Successful literal matching"
    (are [literal expression]
       (= (m+ {} (str/replace-first literal expression ""))
          (match-pattern {} (pattern literal) expression))
        "" ""
        "literal" "literal"))
  (testing "Faulty litral matching"
    (is (= (m- {} "failure" "Expected 'success', but was 'failure'")
           (match-pattern {} (pattern "success") "failure")))))

(deftest matcher-facade-suite
  (testing "Successful matching"
    (are [expected p e]
      (= {::matcher/state ::matcher/success, ::matcher/results expected} (match p e))

      [{"v" "", "z" "mmm"} {"v" "m", "z" ""}]
      (pattern :?v :?z :?v :?v)
      "mmm"

      [{"v" "", "z" "madamiamadam"},
      {"v" "m", "z" "adamiamada"},
      {"v" "madam", "z" "ia"}]
      (pattern :?v :?z :?v)
      "madamiamadam"

      [{"v" "fizz"}]
      (pattern :?v :bazz)
      "fizzbazz"

      [{"v" "fizz" "z" "fizz"}]
      (pattern :?v :bazz :?z)
      "fizzbazzfizz"

      [{"domain" "com", "host" "google"}]
      (pattern "www." :?host "." :?domain)
      "www.google.com"

      [{"domain" "com" "host" "yandex"}]
      (pattern :->host "www." :?host "." :?domain)
      {"host" "www.yandex.com"}))

  (testing "Faulty matching"
    (are [expected p e]
      (= {::matcher/state ::matcher/failure, ::matcher/results expected}
         (match p e))

      [{::matcher/input "www.yandex.com"
        ::matcher/context {}
        ::matcher/message "Expected 'www.google.com', but was 'www.yandex.com'"}]
      (pattern "www.google.com")
      "www.yandex.com"

      [{::matcher/input "m"
        ::matcher/context {"v" ""}
        ::matcher/message "Expected end, but was 'm'"}
       {::matcher/input "m"
        ::matcher/context {"v" ""}
        ::matcher/message "Ambiguity for 'v' (old='', new='m')"}
        {::matcher/input ""
         ::matcher/context {"v" "m"}
         ::matcher/message "Ambiguity for 'v' (old='m', new='')"}]
      (pattern :?v :?v)
      "m")))
