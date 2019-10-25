(ns url-matcher.matcher-test
  (:require [clojure.test :exclude [successful?] :refer :all]
            [url-matcher.matcher :as matcher :refer :all]
            [url-matcher.query :as query]
            [clojure.string :as str]))

(defn pattern
  "Compiles pattern from arguments (pattern is variable if starts with '?')"
  [& args]
  (mapv #(let [n (name %1)]
           (cond
             (str/starts-with? n "?")
             (query/make-clause ::query/variable (list (subs n 1)))

             (str/starts-with? n "->")
             (query/make-clause ::query/section (list (subs n 2)))

             :else
             (query/make-clause ::query/literal (list (str n)))))
        args))

(defn matcher>
  "Makes matcher for a pattern specified in `args`"
  [& args]
  (make-matcher (apply pattern args)))

(defn combine [op & args]
  (case op
    :and (conjunction args)
    :or (disjunction args)
    op))

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

      [{"v" "", "z" "mmm"}, {"v" "m", "z" ""}, {"v" "", "z" "zzz"}, {"v" "z", "z" ""}]
      (pattern :?v :?z :?v :?v)
      ["mmm" "zzz"]

      [{"v" "", "z" "madamiamadam"},
       {"v" "m", "z" "adamiamada"},
       {"v" "madam", "z" "ia"}]
      (pattern :?v :?z :?v)
      "madamiamadam"

      [{"v" "fizz"}]
      (pattern :?v :bazz)
      "fizzbazz"

      [{"v" "fizz", "z" "fizz"}]
      (pattern :?v :bazz :?z)
      "fizzbazzfizz"

      [{"domain" "com", "host" "google"}]
      (pattern "www." :?host "." :?domain)
      "www.google.com"

      [{"domain" "com", "host" "yandex"}]
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

(deftest compound-matcher-suite
  (testing "Successful compound matches"
    (are [expected matcher expression]
         (= {::matcher/state ::matcher/success, ::matcher/results expected}
            (apply-matcher matcher expression))

      [{"y" "3"}]
      (combine :and (matcher> "y=" :?y))
      "y=3"

      [{"x" "2"}]
      (combine :or (matcher> "x=" :?x))
      "x=2"

      [{"domain" "dribble.com"}, {"domain" "integers"}]
      (combine :or
               (matcher> :->host :?domain)
               (matcher> :->query "domain=" :?domain))
      {"host" "dribble.com", "query" "domain=integers"}

      [{"domain" "dribble.com"
        "id" "1905065-Travel-Icons-pack"
        "offset" "1"}]
      (combine :and
               (matcher> :->host "www." :?domain)
               (matcher> :->path "/shots/" :?id)
               (combine :or (matcher> :->queryparam "offset=" :?offset)
                        (matcher> :->queryparam "list=" "losers")))
      {"host" "www.dribble.com"
       "path" "/shots/1905065-Travel-Icons-pack"
       "queryparam" ["list=users" "offset=1"]}))

  (testing "Failing compound matches"
    (are [matcher expression]
         (= ::matcher/failure (::matcher/state (apply-matcher matcher expression)))

      (combine :and (matcher> "y=" :?y) (matcher> "z=" :?y))
      "z=y"

      (combine :or (matcher> "z") (matcher> "x"))
      "y")))

(deftest matcher-info-suite
  (testing "Correct metadata is attached to matcher"
    (are [expected matcher]
         (= expected (matcher-info matcher))

      '(:matcher ((::query/section "host") (::query/variable "domain")))
      (matcher> :->host :?domain)

      '(:or (:matcher ((::query/literal "x")))
            (:matcher ((::query/literal "y"))))
      (combine :or (matcher> "x") (matcher> "y"))

      '(:and (:matcher ((::query/literal "x")))
             (:matcher ((::query/literal "y"))))
      (combine :and (matcher> "x") (matcher> "y")))))

(deftest queries-to-matcher-converter-test
  (testing "Query is compiled to conjunction of clauses"
    (are [expected-matcher queries]
         (= (matcher-info expected-matcher)
            (matcher-info (queries->matcher queries)))

      (combine :and
               (matcher> :->host "host") (matcher> :->host :?host)
               (matcher> :->path "path"))
      [(pattern :->host "host")
       (pattern :->host :?host)
       (pattern :->path "path")])))
