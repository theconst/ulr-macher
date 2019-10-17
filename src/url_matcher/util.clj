(ns url-matcher.util
  "Common utilitis that cannot be otherwise categorised"
  (:require [clojure.zip :as zip]))

(defn find-first [predicate coll]
   "Returns first item in `coll` that matches `predicate`"
   (first (filter predicate coll)))

(defn split-string-at [s position]
 "Returns tuple split of `s` at position"
 [(subs s 0 position) (subs s position)])

(defn prefixes-and-suffixes [s]
 "Returns tuple of prefixes and suffixes"
 (map (partial split-string-at s) (range 0 (inc (count s)))))

(defn make-editor [guard editor]
  (if-not guard #(zip/edit %1 editor)
    (fn guarded-editor [orig]
      (if (guard (zip/node orig))
        (zip/edit orig editor)
        orig))))

(defn take-second [_ snd]
  "Function that always returns its second argument"
  snd)

(defmacro zip-transform [{zipper :zipper, guard :guard, editor :editor, root :root}]
  "Transforms tree starting from `root` using `zipper`
   Trasform is applied using `editor` only if `guard` is true"
  (assert zipper "Zipper should be specified")
  (assert editor "Editor should be specified")

  `(->> (~zipper ~root)
    (iterate (comp zip/next (make-editor ~guard ~editor)))
    (take-while (complement zip/end?))
    (reduce take-second)
    (zip/root)))

(defmacro zip-transform-> [{zipper :zipper, root :root} & transform-definitions]
  "Applies multiple transform definitions starting from `root` using."
  (assert root "Root should be speified")
  (when-not zipper
    (if-let [faulty (find-first (complement :zipper) transform-definitions)]
      (assert false (format "No zipper for %s (default is missing)" faulty))))

  (reduce (fn [r t] `(zip-transform ~(merge {:zipper zipper, :root r} t)))
          root
          transform-definitions))

;;TODO: test besides REPL
(comment


(transform-> [zip/vector-zip [:a :ba :bac]]
   {:guard (complement coll?) :editor (constantly :b)})


(macroexpand '(make-editor nil identity))



(clojure.pprint/pprint
  (clojure.walk/macroexpand-all
    '(transform-> [zip/vector-zip [:a :ba :bac]]
       {:guard (constantly false) :editor (constantly [:b])})))

)
