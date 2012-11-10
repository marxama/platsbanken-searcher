(ns platsbanken-searcher.core
  (:require [net.cgrand.enlive-html :as enlive]))

(defn page-contains
  "Searches url for the given search term. Will look for
   the search term surrounded by non-word characters. Is
   case-insensitive."
  [url search-term]
  (let [re (read-string (str "#\"(?i)\\W" search-term "\\W\""))] 
    ; use enlive instead to extract contents!
    (-> url slurp ((partial re-find re)) string?)))

(defn trim
  [s]
  (->> (seq s)
    (drop-while #(or (= \space %) (= \newline %) (= \tab %)))
    reverse
    (drop-while #(or (= \space %) (= \newline %) (= \tab %)))
    reverse
    (apply str)))

(defn page-summary
  "Returns a map containing :url, :title, :employer and :location."
  [url]
  (let [html (enlive/html-resource (java.net.URL. url))
        title (->> (enlive/select html [:h1])
                second
                :content
                first
                trim)
        employer (->> (enlive/select html [:.employer])
                   first
                   :content
                   first
                   trim)
        location (->> (enlive/select html [:.location])
                   first
                   :content
                   first
                   trim)]
    {:url url
     :title title
     :employer employer
     :location location}))

(defn relative-to-absolute-url
  [url]
  (str "http://www.arbetsformedlingen.se" url))



(defn search-platsbanken
  "Searches all ads on Platsbanken, given the url to a list of results, for
   all given search terms. Returns a seq of maps of summaries."
  [base-url & search-terms]
  (let [html (enlive/html-resource (java.net.URL. base-url))
        next-page (->> (enlive/select html [:.next :a])
                    first
                    :attrs
                    :href)
        following-ads (if-not (nil? next-page)
                        (future 
                          (apply search-platsbanken 
                                 (relative-to-absolute-url next-page) search-terms))
                        (delay nil))
        ad-links (->> (enlive/select html [:.tablelist :a])
                   (map :attrs)
                   (map :href)
                   (map relative-to-absolute-url))
        matching-ads (->> ad-links
                       (filter (fn [url]
                                 (some #(page-contains url %) search-terms)))
                       (map page-summary))]
    (into matching-ads @following-ads)))
