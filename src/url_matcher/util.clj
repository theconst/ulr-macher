(ns url-matcher.util
  "Common utilities that cannot be otherwise categorised"
  (:require [clojure.zip :as zip]))

(defn find-first
  "Returns first item in `coll` that matches `predicate`"
  [predicate coll]
  (first (filter predicate coll)))

(defn remove-last-char
  "Removes last characeter from `s`"
  [s]
  (subs s 0 (dec (count s))))

(defn split-string-at
  "Returns tuple split of `s` at position"
  [s position]
  [(subs s 0 position) (subs s position)])

(defn prefixes-and-suffixes
  "Returns tuple of prefixes and suffixes"
  [s]
  (map (partial split-string-at s) (range 0 (inc (count s)))))

(defn make-editor [guard editor]
  (if-not guard #(zip/edit %1 editor)
          (fn guarded-editor [orig]
            (if (guard (zip/node orig))
              (zip/edit orig editor)
              orig))))

(defmacro zip-transform
  "Transforms tree starting from `root` using `zipper`
   Transform is applied using `editor` only if `guard` is true"
  [{zipper :zipper, guard :guard, editor :editor, root :root}]
  (assert zipper "Zipper should be specified")
  (assert editor "Editor should be specified")

  `(let [editor# (make-editor ~guard ~editor)]
     (->> (~zipper ~root)
          (iterate (comp zip/next editor#))
          (drop-while (complement zip/end?))
          (first)
          (zip/root))))

(defmacro zip-transform->
  "Applies multiple transform definitions starting from `root` using."
  [{zipper :zipper, root :root} & transform-definitions]
  (assert root "Root should be speified")
  (when-not zipper
    (if-let [faulty (find-first (complement :zipper) transform-definitions)]
      (assert false (format "No zipper for %s (default is missing)" faulty))))

  (reduce (fn [r t] `(zip-transform ~(merge {:zipper zipper, :root r} t)))
          root
          transform-definitions))

(defn try-parse
  "Tries to parse `v` if it is a boolean or an integer"
  [^String v]
  (cond
    (re-matches #"[0-9]+" v) (java.math.BigInteger. v)

    :else
    (condp = v
      "true" true
      "false" false
      v)))
