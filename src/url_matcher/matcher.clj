(ns url-matcher.matcher
  "Defines pattern matching of string to pattern"
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [url-matcher.util :refer [prefixes-and-suffixes find-first]]
            [url-matcher.query :as query]))

(defn match-success [context remaining-input]
  "Single successful match result"
  [{::state ::success, ::context context, ::input remaining-input}])

(defn match-failure
  "Match failure with additional diagnostic message"
  ([context remaining-input message]
    [{::state ::failure, ::context context, ::message message,
      ::input remaining-input}])
  ([context remaining-input]
    "Generic match failure"
    (match-failure context remaining-input "Match failure")))

(def has-state #(comp (partial = %) ::state))
(def successful? (has-state ::success))
(def failed? (has-state ::failure))
(def full? (comp str/blank? ::input))

(defmulti match-pattern
  "Performs non-deterministic matching against 'pattern' section of query"
  (fn [_ p _] (query/clause-type p)))

(defmethod match-pattern ::query/empty [context _ expression]
  "Matches empty pattern to `expression` in context"
  (log/tracef "Matching '%s' to <BLANK> pattern [%s]" expression context)
  (if (str/blank? expression)
    (match-success context "")
    (match-failure context expression
                   (format "Expected end, but was '%s'" expression))))

(defmethod match-pattern ::query/literal [context literal expression]
  "Matches `literal` to `expression` in `context`"
  (let [[_ value] literal]
   (log/tracef "Matching '%s' to '%s' [%s]" value expression context)
   (if (str/starts-with? expression value)
      (match-success context (subs expression (count value)))
      (match-failure context expression
                     (format "Expected '%s', but was '%s'"
                             value, expression)))))

(defmethod match-pattern ::query/variable [context variable expression]
  "Matches `varaible` to `expression` in `context`"
  (let [[_ name] variable
        old-value (context name)
        has-value (not (nil? old-value))
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

(defn match-from [pattern {s ::state, ctx ::context, i ::input :as r}]
  (case s
   ::success (match-pattern ctx pattern i)
   ::failure [r]))

(defn match-step [[[head & tail] matches]]
  [tail (mapcat (partial match-from head) matches)])

(defmethod match-pattern ::query/compound [context pattern expression]
  "Matches compound pattern to expression"
  (log/tracef "Matching %s to '%s' [%s]" pattern expression context)

  (->> [pattern (match-success context expression)]
    (iterate match-step)
    (drop-while (comp not nil? first))
    (first)
    (match-step) ;; final match step to match empty sequence
    (second)))


(defn match-section [contexts pattern expression]
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

(defn sum-matches [{s1 ::state, rs1 ::results :as r1}
                   {s2 ::state, rs2 ::results :as r2}]
  (if (= s1 s2)
    {::state s1 ::results (concat rs1 rs2)}
    (find-first successful? [r1 r2])))

(defn match
  ([contexts pattern expression]
   "Match `pattern` (section + body) to `expression`
   (map of vectors or strings) in `contexts`
   Pattern is represented as a sequence of clauses.
   Expression is a either a map of strings or string"
   (cond
     ;;TODO enough cases for multimethod
     (map? expression)
     (match contexts
            (query/clause-children pattern)
            (get expression (query/section-name pattern) ""))

     (string? expression)
     (match-section contexts pattern expression)

     (coll? expression)
     (reduce (partial merge-with sum-matches)
             (map #(match contexts pattern %1) expression))

     :else
     (throw (IllegalArgumentException. "Illegal expression type (map, str, coll)"))))
  ([pattern expression]
   "Match `pattern` to `expression` in empty `context`"
   (match empty-context pattern expression)))

(defn apply-matcher
  "Applies matcher in a given context"
  ([matcher contexts expression]
    (matcher contexts expression))
  ([matcher expression]
    (matcher empty-context expression)))

(defn make-matcher [pattern]
  (fn pattern-matcher
    ^{:matcher-info (str "Matcher for " pattern)}
    [contexts expression]
    (match contexts pattern expression)))

(defn disjunction [matchers]
 (fn dijunction-matcher
   ^{:matcher-info (str "Disjunction of " (map :matcher-info matchers))}
   [contexts expression]
   (->> matchers
    (map #(%1 contexts expression))
    (reduce sum-matches {::state ::success, ::results []}))))

(defn conjunction [matchers]
 "Conjunction of `matchers`"
 (fn conjunction-matcher
   ^{:matcher-info (str "Conjunction of " (map :matcher-info matchers))}
   [contexts expression]
   (reduce (fn conjunction-step [{s ::state, contexts ::results :as r} matcher]
              (case s
                ::success (matcher contexts expression)
                ::failure r))
           {::state ::success, ::results contexts}
           matchers)))

(defn queries->matcher [queries]
  "Converts list of queries to a single matcher in the following fashion:
  Distinct clauses are joined using conjunction, queries to a single section
  are joined by disjunction"
  (->> queries
    (group-by query/section-name)
    (vals)
    (map (partial map make-matcher))
    (map disjunction)
    (conjunction)))

(defn results->maps [{s ::state, rs ::results}]
  "Converts match results to map (or empty if a failure occured)"
  (case s
    ::success rs
    ::failure {}))
