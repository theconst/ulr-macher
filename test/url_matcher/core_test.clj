(ns url-matcher.core-test
  (:require [clojure.test :refer :all]
            [url-matcher.core :refer :all]))

(deftest url-matcher-suite
  (testing "Sample queries from
           https://gist.github.com/kachayev/4811a5b7a6222b9d10f7e8e460d39f69"
    (are [x]
      ;; Constraints on order are relaxed (user essentially obtains a map)
      (= (set (::expected x)) (set (recognize (::pattern x) (::expression x))))

      {::expected [[:id 562360748727611392],
                   ;; This value is modified, since absolute and relative path
                   ;; should be ditunguished for URL
                   ;; (not= bradfitz/icon.img  /bradfitz/icon.img)
                   [:user "/bradfitz"]]
       ::pattern "host(twitter.com); path(?user/status/?id);"
       ::expression "http://twitter.com/bradfitz/status/562360748727611392"}

      {::expected [[:id "1905065-Travel-Icons-pack"]
                   ;; Integers are parsed consistently
                   [:offset 1]]
       ;;same here (absolute and relative paths should be distinguished)
       ::pattern "host(dribbble.com); path(/shots/?id); queryparam(offset=?offset);"
       ::expression "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"}

      {::expected nil
       ::pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"
       ::expression "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"}

      {::expected nil
       ::pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"
       ::expression "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users"}

      {::expected [[:host "dribble"]]
       ::pattern "host(?[host].com); path(/?[host]);"
       ::expression "https://dribble.com/dribble"})))
