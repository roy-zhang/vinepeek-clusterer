(ns clj-lazy-ml.k-means-clustering
  "kmeans clustering with additional heuristic of removing via halflife decay centroids exceeding capacity "
  (:require [utils.decay :refer [ decaying-list replace-list ]]
            [clojure.core.reducers :as r]     )
  (:use [plumbing.core]
        [utils.core]
        [vinepeek-clusterer.wav]))


;"typeVec types
;  :double   
;  vectors of nominal values
;  :wav-fingerprint
;  :RGB
;  :ignore

;---- normalizing stuff
	(defn- update-minMax  [typeVec minMax  newVec ]
	  "stores min and max values encountered so far"
	  (if minMax
	    {:max (vec (map (fn [type currentMax possibleMax] 
                       (cond 
                         (= type :double) (max currentMax possibleMax) 
                         :else 0)) 
                     typeVec (minMax :max) newVec))
         
         :min (vec (map (fn [type currentMin possibleMin] 
                   (cond 
                     (= type :double) (min currentMin possibleMin) 
                     :else 0)) typeVec (minMax :min) newVec))
	    }
       {:max (vec (repeat (count newVec) 0))
        :min (vec (repeat (count newVec) 0))}
     ))
	
	(defn- normalize  [ typeVec minMax  vec]
	  "scales numerics inbetween min-max on scale of 0 to 1, nothing to nominals" ;any transform from raw
	  (mapv (fn [type minVal maxVal  val] 
	              (cond
	                (coll? type)               val
                    (= type :wav-fingerprint)  val
                    (= type :RGB)              val
	                (= type :double)  (if (= minVal maxVal) 0.5 (/ (- val minVal) (- maxVal minVal))) 
                    
                    :else 0.25))
	            typeVec (:min minMax) (:max minMax) vec)  )
	
	(defn unnormalize [typeVec minMax  point] 
	  "takes normalized value and expands to between min max, earlier points might be off due to range issues" 
	  (mapv (fn [type minVal maxVal  val] 
	          (cond 
	            (coll? type)               (nth type (int val))
                (= type :wav-fingerprint)  val
                (= type :RGB)              val
	            (= type :double)  (+ minVal (* val (- maxVal minVal)))
	            ))
	        typeVec (:min minMax) (:max minMax) point))
 
     (defn update-typeVec [ typeVec rawX]
       "looks for new nominal values and adds them to typeVec"
       (mapv (fn [type val] 
	              (cond 
	                (coll? type)    (if (contains? type val) type (conj type val))
	                :else           type	               ))
         typeVec
         rawX))
     
     (defn rawX-to-doubles [typeVec rawX]
       "nominal rawX to doubles for its index"
       (mapv (fn [type val] 
	              (cond 
	                (coll? type)    (.indexOf type val)
	                :else           val               ))
         typeVec rawX))
 
     
(defn- distance-squared  [ typeVec  point1  point2 
                          & {:keys [nominalWeight] :or {nominalWeight 0.25 } }] ;dist between nominals as default same as 0.5*0.5 dist apart numeric
     "Euclidean distance between two collections considered as coordinates. 0.25 if a different nominal value"
     (sum (map (fn [ type  val1 val2 ] 
                      (cond 
                       (= type :double)  (* (- val2 val1) (- val2 val1)) 
                       (coll? type)               (if (= val2 val1) 0 nominalWeight)
                       (= type :wav-fingerprint)  (- 1 (similarity val2 val1))
                       (= type :RGB)            (sum (map #(/ % 255.0) (map -  val1 val2)))
                       :else 1))
                    typeVec point1 point2 )))

            (defn- random-point  [typeVec]
               (map (fn [type]
                                   (cond
                                     (= type :double)   (rand)
                                     (coll? type)                (rand-int (count type))    ))
                                     :else 0
                                 typeVec ))  

(defn- avg-points 
  "avg numeric or most frequent nominal"
  ([ typeVec & points ]
   (mapv (fn [ type ^doubles col ]
                  (cond
                    (coll? type)                 (first (unchunk (most-frequent-n 1 col)) ) 
                    (= type :double)    (/ (sum col) (count col))
                    :else                        (rand-nth col)   ))
                typeVec (invert-rows points)))
  ([typeVec] "in case of no points" (random-point typeVec)  )  )


;---- centroid looping

	(defn- closest-point [ distanceFunc centroids  point ]
	  "which among centroids is closest to point"
	  (let [minPair (fn ([] [Double/POSITIVE_INFINITY]) ([ca cb] (if (< (distanceFunc point ca) (distanceFunc point cb)) ca cb )))] 
	    (r/fold minPair  (vec centroids))))
 
 
 (defn- group-by-centroid [distanceFunc points centroids  ]
   " finds closest centroid for all points and maps centroids to those points"
   (pgroup-by (partial closest-point distanceFunc centroids) points))
    
 
 (defn- loop-new-centroidMaps [ typeVec distanceFunc centroidMaps
                              {:keys [maxLoops epsilon decay-list] :or {maxLoops 10 epsilon (* 0.05 0.05) decay-list (decaying-list 50 :capacity 200)} } ]
  "recalculates centroids until centroids stop moving or maxLoops reached"
   (let [ centroidsToSeqs    (loop [cenToGr  (map-vals seq centroidMaps)  ; map {centroid : listOfPoints}
                                   loopsLeft   maxLoops]
                              (if (< loopsLeft 0)  cenToGr ;maxloops hit 
             
                                (let [oldCenToNew   (map-vals   #(apply (partial avg-points typeVec) %) cenToGr)]
                                  (if (every? (fn [[oldC newC]] (> epsilon (distanceFunc oldC newC))) oldCenToNew)  cenToGr   ;centroids not moving
                 
                                    (recur (group-by-centroid distanceFunc (apply concat (vals cenToGr)) (vals oldCenToNew) ) ;loop again with new centroids
                                           (dec loopsLeft))
                                    )))) ]
    (for-map [[centroid seqs] centroidsToSeqs] ;this part takes a seq and puts it into a decaying list, smallest distance to centroid first.
              centroid (replace-list  decay-list  seqs))
    ))


(defprotocol Clusterer
  (nearest-centroid [this vec])
  (centroids [this])
  (centroid-maps [this])
  (training-points [this])
  (debugging [this])
  )


;----DecayingKMeansClusterer internal functions
             
        
(defn- create-inital-CentroidMaps [k toProcess 
                                   {:keys [decay-list] :or {decay-list (decaying-list 50 :capacity 200) }}]
    "takes k points and maps them to empty decaying lists  ~ different collapse functions added here as well"
		    (for-map [centroid (take k (distinct toProcess))]
		             centroid  decay-list ))
		
(defn- add-points-to-centroidMaps [distanceFunc centroidMaps points]
  "add point to the decaying list of centroid its closest to" 
  (reduce #(update-in %1 [(closest-point distanceFunc (keys centroidMaps) %2)]  conj %2) centroidMaps points ))

(defn interpolated-diagonals[{min :min max :max} num]
  "generates num vectors from minMax"
  (loop [ accum     (transient [])
          numLeft    num
          step      (map #(/ %  (double num)) (map - max min))    
          place     (map + [1 1] (map #(/ % 2) step) )          ]
    (if (> numLeft 0) 
      (recur (conj! accum place) (dec numLeft) step (map + place step))
      (persistent! accum)
      )))


(deftype DecayingKMeansClusterer [^:long k typeVec getDistance centroidMaps 
                                  ^:long resetInterval ^:long countDown minMax 
                                  ^clojure.lang.ISeq toProcess options]
  Clusterer
    (centroids [this]     (if centroidMaps 
                            (into [] (r/map   #(unnormalize typeVec minMax %)  (r/map vec (keys centroidMaps) )))
                            toProcess
                            ))
    (training-points [this] (into [] (r/map #(unnormalize typeVec minMax %) (aconcat (vals centroidMaps))))  ) 
    (centroid-maps [this] (->> centroidMaps
                            (map-vals (partial map (partial unnormalize typeVec minMax)))
                            (map-keys #(unnormalize typeVec minMax %))
                            )) 
    (nearest-centroid [this vec]  (closest-point getDistance (centroids this) vec))
    (debugging [this] { :cm centroidMaps :cd countDown :mm minMax :tp toProcess}) 
  clojure.lang.IPersistentCollection
  (count [this] (count (seq this)))
  (cons  [this rawX]
    (let [newTypeVec     (update-typeVec typeVec rawX)
          newVec         (rawX-to-doubles typeVec rawX)
          minMax         (update-minMax typeVec minMax  newVec)
          toProcess      (conj toProcess  (normalize typeVec minMax newVec)) ]
      (if (>= countDown 0) ;just add a point
        (DecayingKMeansClusterer. k newTypeVec getDistance 
                                    centroidMaps                                                                               
                                    resetInterval (dec countDown) minMax toProcess options )
        (if centroidMaps ; loop if centroids exist
          (DecayingKMeansClusterer. k typeVec getDistance 
                                    (loop-new-centroidMaps newTypeVec getDistance (add-points-to-centroidMaps getDistance centroidMaps toProcess) options)                                                                                   
                                    resetInterval (dec resetInterval) minMax '() options )
          
          (DecayingKMeansClusterer. k newTypeVec getDistance  (create-inital-CentroidMaps k toProcess options)
                                    resetInterval (dec resetInterval) minMax '() options) )
          )))
  (empty [this]      (DecayingKMeansClusterer. k typeVec getDistance nil resetInterval (+ resetInterval k) nil '() options))
  (equiv [this that] (.equiv seq that))
  clojure.lang.Sequential)


  (defn decaying-kmeans-clusterer 
      "k is how many clusters
	   typeVec is a vector of types positioned by col index ex. [ :double :double [:a :b :c] ]
	   getDistance is a function that returns a number from two points
	   resetInterval is how many inputs are stored before centroids are adjusted
	   minMax stores min and max numeric values incountered: predefine if known " 
	  
      [k typeVec & {:keys [getDistance resetInterval countDown minMax collapse-fn centroidHalfLife centroidCapacity maxLoops epsilon] 
                    :or    {getDistance    (partial distance-squared typeVec) 
                            resetInterval    32
                            countDown        32
                            minMax           nil
                            collapse-fn      (fn [a b] a)
                            centroidHalfLife 50
                            centroidCapacity 200
                            maxLoops         10
                            epsilon          (* 0.05 0.05)
                                         }}]
     (DecayingKMeansClusterer. k typeVec getDistance nil
                               resetInterval (+ countDown k) minMax  
                               '() {:decay-list (decaying-list centroidHalfLife :capacity centroidCapacity :collapse collapse-fn) 
                                    :maxLoops maxLoops
                                    :epsilon epsilon
                                            }))
  
 