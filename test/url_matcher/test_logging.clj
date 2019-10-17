(ns url-matcher.test_logging
  "Configuration of test logging"
  (:require [clojure.tools.logging :as log])
  (:import [org.slf4j LoggerFactory]
           [ch.qos.logback.classic Logger Level]))


(doto (LoggerFactory/getLogger Logger/ROOT_LOGGER_NAME)
  (.setLevel Level/DEBUG))
