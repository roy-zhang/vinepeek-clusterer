(ns vinepeek-clusterer.clustering
  (:use [clj-lazy-ml.forgetful-clusterer :as ml]
        [vinepeek-clusterer
         [wav :as w]
         [image :as i]
        ]
       ))


(defn setup-just-fingerprint-clusterer []
  "just looks at fingerprint similarity for each vine"
  (ml/forgetful-clusterer (fn [v1 v2]  (w/similarity   (:wavPrint v1) (:wavPrint v2)))
                          (fn [& vines] 
                              {:wavPrint (apply w/avg-wav-fingerprint-2 (map :wavPrint vines))})
                          0.1
                          :maxClusters 2 :centroidCapacity 1000 :centroidHalfLife 1000))

(defn setup-just-img-clusterer []
  "just looks at img similarity for each vine"
  (ml/forgetful-clusterer (fn [v1 v2] 
                            (i/img-sim-score (:image1 v1) (:image1 v2)))
                          (fn [& vines] 
                            {:image1 (apply i/avg-colors (map :image1 vines)) }   )
                          0.1
                          :maxClusters 2 :centroidCapacity 1000 :centroidHalfLife 1000))


			(defn- normalizerFunc []
			  (let [min (atom 0)
			        max (atom 0)]
			    (fn [num]
			      (when (< num @min) (reset! min (double num)))
			      (when (> num @max) (reset! max (double num)))
                  (if (= @min @max) 0.5
                    (/ (- num @min) (- @max @min))))
			  ))
			
			(defn- avg-of-keyword [vines & kws]
			  (let [mapped (map #(get-in % kws) vines)]
			    (/ (apply + mapped)
			        (double (count mapped)    )
			    )))
			
			(defn- most-frequent [items]
			  (->> items
			    frequencies
			    (sort-by val)
			    reverse
			    ffirst
			    ))
   
	   (defn gender-diff [v1 v2]
	    (let [v1g (:female?  (:profile v1))
	          v2g (:female?  (:profile v2))]
	      (cond
	        (or (= "?" v1g) (= "?" v2g)) 0.5
	        (= v1g v2g) 0.0
	        :else 1.0)))
   
   
(defn setup-info-clusterer []
  "just looks at img similarity for each vine"
  (let [ weeks_old_NF        (normalizerFunc)
         followers_count_NF  (normalizerFunc)
         statuses_count_NF   (normalizerFunc)        ]
  (ml/forgetful-clusterer (fn [v1 v2]                             
                           (+  (Math/abs (- (weeks_old_NF       (:weeks_old       (:profile v1))) (weeks_old_NF       (:weeks_old       (:profile v2)))))
                               (Math/abs (- (followers_count_NF (:followers_count (:profile v1))) (followers_count_NF (:followers_count (:profile v2)))))
                               (Math/abs (- (statuses_count_NF  (:statuses_count  (:profile v1))) (statuses_count_NF  (:statuses_count  (:profile v2)))))
                               (gender-diff v1 v2)
                               (if (= (:lang v1) (:lang v2)) 0.0 1.0)
                               (/ (Math/abs (- (:local_hour v1) (:local_hour v2))) 24.0)
                               ))
                          (fn [& vines] 
                            { :local_hour      (avg-of-keyword vines  :local_hour)  
                              :profile   {
                                         :weeks_old       (avg-of-keyword vines :profile :weeks_old)
                                         :followers_count (avg-of-keyword vines :profile :followers_count)
                                         :statuses_count  (avg-of-keyword vines :profile :statuses_count)
                                         :lang            (most-frequent (map (comp :lang :profile) vines))
                                         :female?         (most-frequent (map (comp :female? :profile) vines))
                                         }})
                          0.1
                          :maxClusters 2 :centroidCapacity 1000 :centroidHalfLife 1000)
  ))

   
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
     
(defn setup-all-clusterer []
  "just looks at img similarity for each vine"
  (let [ weeks_old_NF        (normalizerFunc)
         followers_count_NF  (normalizerFunc)
         statuses_count_NF   (normalizerFunc)        ]
  (ml/forgetful-clusterer (fn [v1 v2]                             
                           (+  (w/similarity   (:wavPrint v1) (:wavPrint v2))
                               (i/img-sim-score (:image1 v1) (:image1 v2))
                               (Math/abs (- (weeks_old_NF       (:weeks_old       (:profile v1))) (weeks_old_NF       (:weeks_old       (:profile v2)))))
                               (Math/abs (- (followers_count_NF (:followers_count (:profile v1))) (followers_count_NF (:followers_count (:profile v2)))))
                               (Math/abs (- (statuses_count_NF  (:statuses_count  (:profile v1))) (statuses_count_NF  (:statuses_count  (:profile v2)))))
                               (gender-diff v1 v2)
                               (if (= (:lang v1) (:lang v2)) 0.0 1.0)
                               (/ (Math/abs (- (:local_hour v1) (:local_hour v2))) 24.0)
                               ))
                          (fn [& vines] 
                            {:image1          (apply i/avg-colors (map :image1 vines))
                             :wavPrint        (apply w/avg-wav-fingerprint-2 (map :wavPrint vines))
                             :profile   {
                                         :weeks_old       (avg-of-keyword vines :profile :weeks_old)
                                         :followers_count (avg-of-keyword vines :profile :followers_count)
                                         :statuses_count  (avg-of-keyword vines :profile :statuses_count)
                                         :lang            (most-frequent (map (comp :lang :profile) vines))
                                         :female?         (most-frequent (map (comp :female? :profile) vines))
                                         }
                              :local_hour      (avg-of-keyword vines  :local_hour)
                             }
                             )
                          0.1
                          :maxClusters 2 :centroidCapacity 1000 :centroidHalfLife 1000)
  ))

(def languageCodes (vector "en" "es" "pt" "it" "tr" "ko" "fr" "ru" "de" "ja"))
