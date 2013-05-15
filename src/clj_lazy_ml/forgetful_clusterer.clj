(ns clj-lazy-ml.forgetful-clusterer
  (:require [utils.decay :refer [ decaying-list replace-list ]]
            [clojure.core.reducers :as r]     )
  (:use [plumbing.core]
        [utils.core]
        [vinepeek-clusterer.wav]))


(defrecord ForgetfulClusterer [ distanceFunc avgFunc threshold centroidMaps options])

	(defn closest-point [ distanceFunc collToTest point ]
	  ""
	  (let [minPair (fn ([] [Double/POSITIVE_INFINITY]) ([ca cb] (if (< (distanceFunc point ca) (distanceFunc point cb)) ca cb )))] 
	    (r/fold minPair  (vec collToTest))))
    
    (defn adjust-cm [{avg :avgFunc cm :centroidMaps :as fgcl} nearest newPoint]
      "adds point, adjusts centroid"
      (->> newPoint
        (conj (cm nearest))
        (avg)
        (#(clojure.set/rename-keys cm {nearest %}))
        (assoc fgcl :centroidMaps)))
    
    (defn threshold-pair [{cm :centroidMaps thresh :threshhold dist :distanceFunc}]
      "first pair of centroids that are less than threshold apart"
        (some (fn [[c1 c2]] (when (> thresh (dist c1 c2)) [c1 c2]))  (set (map set (combinations (keys cm) 2))))
      )
    
    (defn recluster [{dist :distanceFunc avg :avgFunc cm :centroidMaps opts :options :as fgcl}]
      ""
      (let [group-by-centroid (fn [dist pts ctds] (pgroup-by (partial closest-point dist ctds) pts))
            centroidsToSeqs   (loop [cenToGr     (map-vals seq cm)  ; map {centroid : listOfPoints}
                                     loopsLeft   (:maxLoops opts)]
                                (if (< loopsLeft 0)  cenToGr ;maxloops hit 
                                  
                                  (let [oldCenToNew   (map-vals   #(apply avg %) cenToGr)]
                                    (if (every? (fn [[oldC newC]] (> (:epsilon opts) (dist oldC newC))) oldCenToNew)  cenToGr   ;centroids not moving
                                      
                                      (recur (group-by-centroid dist (apply concat (vals cenToGr)) (vals oldCenToNew) ) ;loop again with new centroids
                                             (dec loopsLeft))
                                      )))) ]
    (assoc fgcl :centroidMaps
        (for-map [[centroid seqs] centroidsToSeqs] ;this part takes a seq and puts it into a decaying list, smallest distance to centroid first.
              centroid (replace-list  (:decay-list opts)  seqs)) )))
    
    (defn merge-cm [{avg :avgFunc cm :centroidMaps :as fgcl} c1 c2]
      (let [points1 (cm c1)
            points2 (cm c2)
            small   (min (count points1) (count points2)) ]
        (assoc fgcl :centroidMaps
               (-> cm
                 (dissoc c1)
                 (dissoc c2)
                 (assoc (avg c1 c2) (concat (interleave points1 points2) (drop small points1) (drop small points2))))
      )))
      
    (defn add-point [{dist :distanceFunc thresh :threshhold cm :centroidMaps opts :options :as fgcl} newPoint]
      " get new point       get closest centroid        add to that group        adjust centroid
        check whether any centorids are within threshold, recluster
        merge if still within threshold, cluster again"
      (let [nearest (closest-point dist (keys cm) newPoint)]
         (if (or (empty? cm) ;add cluster if none exist or (outside threshold of nearest and still under maxClusters)
                 (and (< (count cm) (:maxClusters opts)) (> (dist nearest newPoint) thresh)))
                (assoc-in fgcl [:centroidMaps newPoint] (conj (:decay-list (:options fgcl)) newPoint)) ;add new centroid
             
         (let [ addedfgcl   (adjust-cm fgcl nearest newPoint)
                newCentroid (closest-point dist (keys (:centroidMaps addedfgcl)) newPoint) ]
           (if-not (threshold-pair addedfgcl) addedfgcl
	           (loop [ cl (recluster addedfgcl) ]
	                (if-let [[c1 c2] (threshold-pair fgcl)]
	                  (recur (recluster (merge-cm fgcl c1 c2)))
	                  cl)))
             ))))
    
      (defn forgetful-clusterer 
      "needs: function to calc dist between two points, 
              function to avg a set of points, 
               threshold to start new centroid, "
      [distanceFunc avgFunc threshold  & {:keys [resetInterval countDown collapse-fn centroidHalfLife centroidCapacity maxLoops epsilon] 
                    :or    {getDistance    (partial distance-squared typeVec)
                            maxClusters     20
                            collapse-fn      (fn [a b] a)
                            centroidHalfLife 50
                            centroidCapacity 200
                            maxLoops         10
                            epsilon          (* 0.05 0.05)
                                         }}]
     (ForgetfulClusterer. distanceFunc avgFunc threshold nil
                          {:decay-list (decaying-list centroidHalfLife :capacity centroidCapacity :collapse collapse-fn)
                           :maxClusters maxClusters
                           :maxLoops maxLoops
                           :epsilon epsilon
                           }))
      
      ;----------------------------------------------------------------
      
      ;example usage, where points are assumed to be a sequence of doubles
	        (defn- update-minMax  [ minMax ^doubles newVec ]
			  "stores min and max values encountered so far"
			  (if minMax
			    {:max (mapv max (minMax :max) newVec) :min (mapv min (minMax :min) newVec)}
			    {:max newVec :min newVec} ))
			
            (defn- normalize  [ {mins :min maxs :max} ^doubles vec]
			  "scales numerics inbetween min-max on scale of 0 to 1, nothing to nominals"
			  (mapv (fn [min max ^double val] 
                     (/ (- val min) (- max min)))
                    mins maxs vec))
      
      (defn distanceFunc-doubles []
        "values normalized to be between 0-1 to give equal weight to each dimension, 
           also dist is squared, so threshold needs to be as well"
        (let [minMax* (atom nil)]
          
          (fn [ ^doubles point1 ^doubles point2] 
              (swap! minMax* update-minMax point1)
              (swap! minMax* update-minMax point2)
            (sum (map #(* (- %1 %2) (- %1 %2)) 
                      (normalize @minMax* point1)
                      (normalize @minMax* point2)))
            )))
      
      (defn averageFunc-doubles [& points]
        (map #(/ (sum %) (double (count %))) (invert-rows points)))

      
   
      
      
  
 