(ns url-matcher.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [url-matcher.query :as query]
            [url-matcher.url :as url]
            [url-matcher.matcher :as matcher]))

(defn url->expression [{host :host
                        {params :query-parameters} :query
                        path :path}]
  {"host" host
   "queryparam" params
   "path" path})

(defn try-parse [v]
  "Tries to parse `v` if it is a boolean or an integer"
  (cond
    (re-matches #"[0-9]+" v) (java.math.BigInteger. v)

    :else
    (condp = v
      "true" true
      "false" false
      v)))

(defn result-map->result-vec [result-map]
  "Adapts result map from matcher by keywordizing variables and gussing some types"
  (mapv (fn [[k v]]
          [(keyword (str k)) (try-parse v)])
        result-map))

(defn recognize [query url-string]
  (-> query
    (query/parse)
    (matcher/queries->matcher)
    (matcher/apply-matcher (url->expression (url/parse url-string)))
    (matcher/results->maps)
    (first)
    (result-map->result-vec)))

(defn -main
  [& args]
  (println (apply recognize args)))
