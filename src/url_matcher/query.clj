(ns url-matcher.query
  "Processes query language and returns abstract representation of it"
  (:require [clj-antlr.core :as antlr]
            [clojure.java.io :refer [resource]]
            [url-matcher.util :refer [find-first
                                      remove-last-char
                                      zip-transform->]]
            [clojure.zip :as zip]
            [clojure.string :as str])
  (:import [clj_antlr ParseError]))

(def clauses-ns (find-ns 'url-matcher.query))
(def grammar (slurp (resource "query.g4")))
(def grammar-root :query)

(def ^{:doc (str "Parser for query. Use the following syntax for variables: "
                 "?variable or ?[variable]. See also `query.g4`")}
  query-dsl-parser (antlr/parser grammar {:root grammar-root}))

(defn clause-type
  "Returns type of clause"
  [clause]
  (cond
    (string? clause) ::atom
    (keyword? clause) ::tag
    (empty? clause) ::empty
    (nil? clause) ::empty

    :else (let [[t & _] clause] (if (coll? t) ::compound t))))

(def ^{:doc "Returns children of compound clause, otherwise nil is returned"}
  clause-children rest)

(def ^{:doc "Returns new clause"}
  make-clause cons)

(defn- replace-children [prototype new-children]
  (make-clause (first prototype) new-children))

(defn- qualify
  "Qualifies `tag` with `clauses-ns`"
  [tag]
  (assert clauses-ns "Clauses namespace should exist")

  (keyword (str clauses-ns) (name tag)))

(defmacro ^:private defpredicate
  "Defines predicate on parse node. Use this only for compound nodes,
   primitive ones can be tested using string? or similar"
  ([sym]
   (let [predicate-name (name sym)
         unqualified-tag (remove-last-char predicate-name)
         clause-tag (qualify unqualified-tag)]
     `(defpredicate ~sym
        (str "Tests if clause is " ~unqualified-tag)
        #{~clause-tag})))
  ([sym condition]
   `(defpredicate ~sym (:doc (meta ~condition)) ~condition))
  ([sym docstring condition]
   (assert (str/ends-with? (name sym) "?") "Predicate should end with ?")
   `(defn ^{:doc ~docstring} ~sym [c#] (~condition (clause-type c#)))))

(defpredicate tag?)
(defpredicate atom?)
(defpredicate variable?)
(defpredicate section?)
(defpredicate name?)
(defpredicate query?)
(defpredicate subquery?)
(defpredicate pattern?)
(defpredicate literal?)
(defpredicate has-punctuation?
  "Checks if predicate has punctuation (string that can be removed)"
  #{::variable ::subquery ::query})

(defn make-zipper
  "Create zipper for clause traversal starting from `root`"
  [root]
  (zip/zipper coll? clause-children replace-children root))

(defn remove-atoms
  "Keeps compound node, as nodes without type maintain no semantic information"
  [clause]
  (replace-children clause (remove atom? (clause-children clause))))

(defn pull-name
  "Pulls name of `clause` to the upper level so that it is the only child"
  [clause]
  (if-let [name-clause (find-first name? (clause-children clause))]
    (replace-children clause (clause-children name-clause))
    (throw (IllegalStateException. (str "No name for " clause)))))

(defn join-children
  "Join children of `clause`"
  [clause]
  (replace-children clause (list (str/join (clause-children clause)))))

(defn flatten-subquery
  "Extract pattern subquery from clause"
  [clause]
  (make-clause (find-first section? clause)
               (mapcat clause-children (filter pattern? clause))))

(defn section-name
  "Get section name of a section clause"
  [[section & _ :as query]]
  (when-not (section? section)
    (throw (IllegalArgumentException. (str "Missing section in " query))))
  (let [[_ name] section] name))

(defn parse
  "Wrapper for `query-dsl-parser` that strips away unnessary characters
  and produces sequence of queries grouped by section"
  [query]
  (try
    (clause-children
     (zip-transform-> {:zipper make-zipper, :root (query-dsl-parser query)}
                      {:zipper zip/seq-zip, :guard tag?, :editor qualify}
                      {:guard has-punctuation? :editor remove-atoms}
                      {:guard literal?, :editor join-children}
                      {:guard variable?, :editor pull-name}
                      {:guard subquery? :editor flatten-subquery}))
    (catch ParseError pe
      (throw (IllegalArgumentException. pe)))))
