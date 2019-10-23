(ns url-matcher.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [url-matcher.query :as query]
            [url-matcher.url :as url]
            [url-matcher.matcher :as matcher]
            [url-matcher.util :as util]))

(defn url->expression [{host :host
                        {params :query-parameters} :query
                        path :path}]
  "Transform url to expression that can be matched"
  {"host" host
   "queryparam" params
   "path" path})

(defn result-map->result-vec [result-map]
  "Adapts result map from matcher by keywordizing variables and gussing some types"
  (mapv (fn [[k v]]
          [(keyword (str k)) (util/try-parse v)])
        result-map))

(defn recognize [queries url-string]
  (let [query-matcher (matcher/queries->matcher (query/parse queries))
        expression (url->expression (url/parse url-string))]
    (-> (matcher/apply-matcher query-matcher expression)
      (matcher/results->maps)
      (first)
      (result-map->result-vec))))

(defn -main [& args]
  (println (apply recognize args)))
