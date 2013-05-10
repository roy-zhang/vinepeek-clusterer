(ns clj-lazy-ml.discretizer
  "Uses kmeans clustering to nominalize numeric values
   usage:
     (def numDis (numeric-discretizer 5 [java.lang.Double java.lang.Double [:a :b :c]]))
     (def numDis (update-numeric-discretizer numDis [3.5 2 1]))
     (nominalize numDis [3.5 2 1])    => [1 0 1] "
  (:require [clj-lazy-ml.k-means-clustering :as kmeans ]))


(defrecord NumericDiscretizer [bins typeVec clusterers])

			 (defn create-clusterers [typeVec bins]
			  "java.lang.Double types gets transformed into a dkc, otherwise nil "
			  (mapv  (fn [type] 
			           (if (= type java.lang.Double) 
			             (kmeans/decaying-kmeans-clusterer bins typeVec :centroidHalfLife 15 :centroidCapacity 30 :countDown bins)
			             nil ))
			         typeVec))
    
(defn numeric-discretizer [bins typeVec]
  (NumericDiscretizer. bins typeVec (create-clusterers typeVec bins)))


			(defn update-clusterers [clusterers newVec]
		       "new clusterers with newVec added"
			  (mapv (fn [aclusterer? val] (when aclusterer? (conj aclusterer? val)))
			        clusterers newVec))
   
(defn update-numeric-discretizer [{bins :bins typeVec :typeVec clusterers :clusterers} newVec]
  (NumericDiscretizer. bins typeVec (update-clusterers clusterers  (map vector newVec)) ))
 

			 	(defn clusterers-to-sorted-centroids [clusterers]
				  "transforms dkc into sorted centroids"
				  (mapv   #(when % (sort-by first (kmeans/centroids %)))   clusterers))
			
			(defn  nominalize-vector [clusterers newVec]
			  "map newVec to sorted position of centroid its closest to"
			  (let [sortedCentroids (clusterers-to-sorted-centroids clusterers)]
			    (mapv (fn [clusterer centroids val]
			          (if centroids   (.indexOf centroids (kmeans/nearest-centroid clusterer val))    
			                          (first val)))  ;get index of centroid nearest to vec
			        clusterers sortedCentroids newVec)))
	
(defn nominalize [{bins :bins typeVec :typeVec clusterers :clusterers} newVec]
  (nominalize-vector clusterers (map vector newVec)) )




