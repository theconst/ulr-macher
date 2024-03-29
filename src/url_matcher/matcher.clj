(ns url-matcher.matcher
  "Defines pattern matching of string to pattern"
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [url-matcher.util :refer [prefixes-and-suffixes find-first]]
            [url-matcher.query :as query]))

(defn match-success
  "Single successful match result"
  [context remaining-input]
  [{::state ::success, ::context context, ::input remaining-input}])

(defn match-failure
  "Match failure with additional diagnostic message"
  ([context remaining-input message]
   [{::state ::failure, ::context context, ::message message,
     ::input remaining-input}])
  ([context remaining-input]
   (match-failure context remaining-input "Match failure")))

(def ^:private has-state #(comp (partial = %) ::state))
(def ^:private successful? (has-state ::success))
(def ^:private failed? (has-state ::failure))
(def ^{:doc "Checks if match is full, i. e. the whole input is consumed"
       :private true}
  full? (comp str/blank? ::input))

(defmulti match-pattern
  "Performs non-deterministic matching against 'pattern' section of query"
  (fn [_ p _] (query/clause-type p)))

(defmethod match-pattern ::query/empty [context _ expression]
  (log/tracef "Matching '%s' to <BLANK> pattern [%s]" expression context)
  (if (str/blank? expression)
    (match-success context "")
    (match-failure context expression
                   (format "Expected end, but was '%s'" expression))))

(defmethod match-pattern ::query/literal [context literal expression]
  (let [[_ value] literal]
    (log/tracef "Matching '%s' to '%s' [%s]" value expression context)
    (if (str/starts-with? expression value)
      (match-success context (subs expression (count value)))
      (match-failure context expression
                     (format "Expected '%s', but was '%s'"
                             value, expression)))))

(defmethod match-pattern ::query/variable [context variable expression]
  (let [[_ name] variable
        old-value (context name)
        has-value (some? old-value)
        into-context #(assoc context name %)
        ambiguity (partial format "Ambiguity for '%s' (old='%s', new='%s')"
                           name, old-value)]
    (log/tracef "Matching ?%s to '%s' [%s]" name expression context)
    (mapcat
     (fn [[prefix suffix]]
       (if (and has-value (not= old-value prefix))
         (match-failure context expression (ambiguity prefix))
         (match-success (into-context prefix) suffix)))
     (prefixes-and-suffixes expression))))

(defn- match-from
  "Match starting from `pattern`"
  [pattern {s ::state, ctx ::context, i ::input :as r}]
  (case s
    ::success (match-pattern ctx pattern i)
    ::failure [r]))

(defn- match-step
  "Single step of matching"
  [[[head & tail] matches]]
  [tail (mapcat (partial match-from head) matches)])

(defmethod match-pattern ::query/compound
  [context pattern expression]
  (log/tracef "Matching %s to '%s' [%s]" pattern expression context)

  (->> [pattern (match-success context expression)]
       (iterate match-step)
       (drop-while (comp some? first))
       (first)
       (match-step) ;; final match step to match empty sequence
       (second)))

(defn match-section
  "Match single section of pattern"
  [contexts pattern expression]
  (let [results (mapcat #(match-pattern %1 pattern expression) contexts)
        partial-matches (filter successful? results)
        exact-matches (filter full? partial-matches)]
    (if (not-empty exact-matches)
      {::state ::success, ::results (map ::context exact-matches)}

      {::state ::failure
       ::results (->> results
                      (filter failed?)
                      (map #(select-keys % [::context ::input ::message])))})))

(def ^{:doc "Contxext with no bound variables"} empty-context [{}])

(defn- sum-matches
  "Merges two match results"
  [{s1 ::state, rs1 ::results :as r1}
   {s2 ::state, rs2 ::results :as r2}]
  (log/tracef "Summing %s and %s" r1 r2)

  (if (= s1 s2)
    {::state s1 ::results (concat rs1 rs2)}
    (find-first successful? [r1 r2])))

(defprotocol IMatchable
  "Defines expression that can be mathed to some `pattern` using `contexts`"
  (match-to [self contexts pattern] "match `self` to `pattern` in `contexts`"))

(extend-type clojure.lang.IPersistentMap
  IMatchable
  (match-to [self contexts pattern]
    (match-to (get self (query/section-name pattern) "")
              contexts
              (query/clause-children pattern))))

(extend-type clojure.lang.Seqable
  IMatchable
  (match-to [self contexts pattern]
    (reduce sum-matches (map #(match-section contexts pattern %1) self))))

(extend-type String
  IMatchable
  (match-to [self contexts pattern]
    (match-section contexts pattern self)))

(extend-type nil
  IMatchable
  (match-to [self context pattern]
    (match-section context pattern "")))

(defn match
  "Match `pattern` (section + body) to `expression`
  (map of vectors or strings) in `contexts`
  Pattern is represented as a sequence of clauses.
  Expression is a either a map of strings or string"
  ([contexts pattern expression]
   (match-to expression contexts pattern))
  ([pattern expression]
   (match empty-context pattern expression)))

(defn apply-matcher
  "Applies matcher in a given context"
  ([matcher contexts expression]
   (log/tracef "Applying %s to %s"
               (:matcher-info (meta  matcher))
               expression)
   (matcher contexts expression))
  ([matcher expression]
   (matcher empty-context expression)))

(defn matcher-info
  "Gets human-readable informtion about matcher"
  [matcher]
  (:matcher-info (meta matcher)))

(defn make-matcher
  "Makes matcher for a given pattern"
  [pattern]
  (with-meta
    (fn pattern-matcher [contexts expression]
      (match contexts pattern expression))
    {:matcher-info (list :matcher pattern)}))

(defn disjunction
  "Disjunction of `matchers`"
  [matchers]
  (with-meta
    (fn disjunction-matcher [contexts expression]
      (->> matchers
           (map #(apply-matcher %1 contexts expression))
           (reduce sum-matches)))
    {:matcher-info (cons :or (mapv matcher-info matchers))}))

(defn conjunction
  "Conjunction of `matchers`"
  [matchers]
  (with-meta
    (fn conjunction-matcher [contexts expression]
      (reduce (fn conjunction-step [{s ::state, contexts ::results :as r} matcher]
                (case s
                  ::success (matcher contexts expression)
                  ::failure r))
              {::state ::success, ::results contexts}
              matchers))
    {:matcher-info (cons :and (mapv matcher-info matchers))}))

(defn queries->matcher
  "Converts list of queries to a single matcher in the following fashion:
  All quries are joined by conjunction"
  [queries]
  (conjunction (mapv make-matcher queries)))

(defn results->maps
  "Converts match results to maps (or empty if a failure occured)"
  [{s ::state, rs ::results}]
  (case s
    ::success rs
    ::failure empty-context))
