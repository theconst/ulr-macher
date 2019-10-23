(ns url-matcher.core
  (:gen-class)
  (:require [clojure.pprint :refer [print-table]]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.tools.cli :as cli]
            [url-matcher.query :as query]
            [url-matcher.matcher :as matcher]
            [url-matcher.url :as url]
            [url-matcher.util :as util]))

(defn url->expression [{host :host
                        {params :query-parameters} :query
                        path :path}]
  "Transform url to expression that can be matched"
  {"host" host
   "queryparam" params
   "path" path})

(defn result-map->result-vec [result-map]
  "Adapts result map from matcher by making variables keywords
  and inferring some types"
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

;;
;; Quick and dirty command line app
;;

(def cli-options
  [["-u" "--url URL" "URL to match"]
   ["-p" "--pattern PATTERN" "Pattern to match agains"]
   ["-h" "--help" "Displays this message"]])

(defn print-result [result]
  (print-table [(into {} result)]))

(defn exit
  ([]
    (exit :success))
  ([status]
    (System/exit ({:success 0, :wrong-usage 1, :unknown-error 2} status))))

(defn prompt [msg]
  (println msg)
  (let [result (read-line)]
    (case result
      "/exit" (exit)
      result)))

(defn repl-session []
  (try
    (let [target (prompt "Enter URL: ")
          pattern (prompt "Enter pattern: ")]
      (print-result (recognize pattern target)))
   (catch IllegalArgumentException ex
      (println "Check correctness of input. Cause: " ex))
   (catch Exception ex
      (println "Unknow error ocurred. Cause " ex)
      (exit :unknown-error))))

(defn -main [& args]
  (let [{{target :url, pattern :pattern :as options} :options
         errors :errors
         summary :summary}
        (cli/parse-opts args cli-options)]
    (when errors
      (println "Error: " errors)
      (println "Use --help option to get help")
      (exit :wrong-usage))
    (when (:help options)
      (println "Utility for matching URLs to queries")
      (println summary)
      (exit))
    (if (not (and target pattern))
      (while true
        (repl-session))
      (print-result (recognize pattern target)))))
