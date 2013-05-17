(ns vinepeek-clusterer.clustering
  (:use [clj-lazy-ml.forgetful-clusterer :as ml]
        [vinepeek-clusterer.wav :as w]
        [vinepeek-clusterer.image :as i]
       ))






(defn setup-just-fingerprint-clusterer []
  "just looks at fingerprint similarity for each vine"
  (ml/forgetful-clusterer (fn [v1 v2] (- 1 (w/similarity   (:wavPrint v1) (:wavPrint v2))))
                          (fn [& vines] 
                              {:wavPrint (apply w/avg-wav-fingerprint-2 (map :wavPrint vines))})
                          0.1
                          :maxClusters 2 :centroidCapacity 1000 :centroidHalfLife 1000)
  )


(defn setup-just-img-clusterer []
  "just looks at img similarity for each vine"
  (ml/forgetful-clusterer (fn [v1 v2] (i/img-sim-score (:image1 v1) (:image1 v2)))
                          (fn [& vines] 
                            {:image1 (apply i/avg-colors (map :image1 vines)) }   )
                          0.1
                          :maxClusters 2 :centroidCapacity 1000 :centroidHalfLife 1000)
  )





(def languageCodes (vector "en" "es" "pt" "it" "tr" "ko" "fr" "ru" "de" "ja"))

   (defn rearrange-vine-for-clusterer [vine]
     (vector 
       (:weeks_old       (:profile vine)) ;age of the profile
       (:followers_count (:profile vine)) ;followers
       (:statuses_count  (:profile vine)) ;total tweets
       (:lang            (:profile vine)) ;language 
       (:mentionsCount   vine)            ;how many people @ed in tweet 
       (:local_hour      vine)            ;local time of tweet  ex. 9.583 for 9:35ish
       (:wavPrint        vine)            ;wav fingerprint
       (first (:colors   vine)            ; first average color of video
       )
     ))
