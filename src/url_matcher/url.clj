(ns url-matcher.url
  "Adapter for java.net.URL"
  (:require [clojure.string :as s]))

(defrecord URLQuery [raw query-parameters])
(defrecord URL [scheme host path query])

(defn parse-query [query-string]
  "Parse `query-string` and store both raw query as well as
  conventionally parsed query string (using & as parameter
  separator and = as key value separator)"
  (if query-string
    (->URLQuery query-string (s/split (or query-string "") #"&"))))


(defn parse [url-string]
  "Parse `url-string` or throw `java.net.MalformedURLException`"
  (let [url (java.net.URL. url-string)]
    (map->URL {:scheme (.getProtocol url)
               :host (.getHost url)
               :path (.getPath url)
               :query (parse-query (.getQuery url))})))
