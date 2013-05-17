(ns clj-lazy-ml.forgetful-clusterer
  (:require [utils.decay :refer [ decaying-list replace-list ]]
            [clojure.core.reducers :as r]     )
  (:use [plumbing.core]
        [utils.core]
        ))


(defrecord ForgetfulClusterer [ distanceFunc avgFunc threshold centroidMaps options])

	(defn closest-point [ distanceFunc collToTest point ]
	  "returns elem from coll with smallest dist to point  "
	  (let [minPair (fn ([] nil) 
                   ([ca cb] 
                     (cond (nil? ca) cb (nil? cb) ca :else
                     (if (< (distanceFunc point ca) (distanceFunc point cb)) ca cb ))))] 
	    (r/fold minPair  (vec collToTest))))
 
     (defn closest-centroid [{dist :distanceFunc cm :centroidMaps} point ]
       (closest-point dist (keys cm) point))
    
    (defn adjust-cm [{avg :avgFunc cm :centroidMaps :as fgcl} nearest newPoint]
      "adds point, adjusts centroid"
      (let [newList (conj (cm nearest) newPoint)]
        (-> fgcl
          (assoc-in [:centroidMaps nearest] newList)
          (update-in [:centroidMaps] clojure.set/rename-keys  {nearest (apply avg (seq newList))}))
      ))
    
    (defn threshold-pair [{cm :centroidMaps thresh :threshold dist :distanceFunc}]
      "first pair of centroids that are less than threshold apart"
        (some (fn [[c1 c2]] (when (> thresh (dist c1 c2)) [c1 c2]))   (combinations (keys cm) 2)))
    
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
      (let [small   (min (count (cm c1)) (count (cm c2))) 
            merged  (concat (interleave (cm c1) (cm c2)) (drop small (cm c1)) (drop small (cm c2)))]
        (assoc fgcl :centroidMaps
               (->  cm
                 (assoc  (avg c1 c2) (concat (interleave (cm c1) (cm c2)) (drop small (cm c1)) (drop small (cm c2))))
                 (dissoc c1)
                 (dissoc c2)
                 )
      )))
    
    (defn add-point [{dist :distanceFunc thresh :threshold cm :centroidMaps opts :options :as fgcl} newPoint]
      " get new point       get closest centroid        add to that group        adjust centroid
        check whether any centorids are within threshold, recluster
        merge if still within threshold, cluster again"
      (let [nearest (closest-centroid fgcl newPoint)]
         (if (or (empty? cm) ;add cluster if none exist or (outside threshold of nearest and still under maxClusters)
                 (and (< (count cm) (:maxClusters opts)) (> (dist nearest newPoint) thresh)))
             (assoc-in fgcl [:centroidMaps newPoint] (conj (:decay-list (:options fgcl)) newPoint)) ;add new centroid
             
         (let [ addedfgcl   (adjust-cm fgcl nearest newPoint) ]
           (if-let [[c1 c2] (threshold-pair addedfgcl)]
             (recluster (merge-cm fgcl c1 c2))
              addedfgcl
             )
             ))))
    
    (defn add-points [fgcl points]    (recluster (reduce add-point fgcl points)))
    
    (defn centroids [fgcl] (keys (:centroidMaps fgcl)))
    
    (defn training-points [fgcl] (apply concat (vals (:centroidMaps fgcl))))
    
    
      (defn forgetful-clusterer 
      "needs: function to calc dist between two points, 
              function to avg a set of points, 
               threshold to start new centroid, "
      [distanceFunc avgFunc threshold  & {:keys [  collapse-fn centroidHalfLife centroidCapacity maxClusters maxLoops epsilon] 
                    :or    {collapse-fn      (fn [a b] a)
                            centroidHalfLife 50
                            centroidCapacity 200
                            maxClusters      20
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
      
      (defn distanceFunc-doubles  [ ^doubles point1 ^doubles point2] 
            (sum (map #(* (- %1 %2) (- %1 %2)) 
                      point1
                      point2))
            )
      
      (defn averageFunc-doubles [& points]
        (map #(/ (sum %) (double (count %))) (invert-rows points)))

      
   ;(def exampleCl (forgetful-clusterer 
    ;                                distanceFunc-doubles
     ;                               averageFunc-doubles
     ;                               200
     ;                               :maxClusters 10 
     ;                              :centroidHalflife 50 
     ;                               :centroidCapacity 200 )
   
   ;(centroids (add-points exampleCl '([13 13] [51 15] [25 63]))
      
      
  
 