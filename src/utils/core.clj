(ns utils.core
  (:require [clojure.core.reducers :as r])
  (:use [plumbing.core]))

(defn invert-rows [grid]
  "flip rows into lists of column values also magically O(1)?"
   (apply map list  grid))

(defn chop [string]
  "removes last char" 
  (subs string 0 (- (count string) 1)))

(defn most-frequent-n [n items]
  (->> items
    frequencies-fast
    (sort-by val)
    reverse
    (take n)
    (map first)))

(defn pgroup-by [f coll] 
  "parallel group-by" 
  (r/fold   (fn ([] {})  
                ([L R]  (merge-with concat L R)))
    (r/map #(hash-map (f %) (list %))  
            (vec coll))  ))
	
(defn st []
        (clojure.stacktrace/print-stack-trace *e))
  
(defn shuff2 [nums]
     (map vector (shuffle (range nums)) (shuffle (range nums))))

;https://github.com/clojure/math.combinatorics/blob/5cf956fbc2d7225aad8e9ed8e2670e7af64417c4/src/main/clojure/clojure/math/combinatorics.clj#L69
(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
	 iter-comb
	 (fn iter-comb [c j]
	   (if (> j n) nil
	       (let [c (assoc c j (dec (c j)))]
		 (if (< (c j) j) [c (inc j)]
		     (loop [c c, j j]
		       (if (= j 1) [c j]
			   (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
	 step
	 (fn step [c j]
	   (cons (rseq (subvec c 1 (inc n)))
		 (lazy-seq (let [next-step (iter-comb c j)]
			     (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))
;https://github.com/clojure/math.combinatorics/blob/5cf956fbc2d7225aad8e9ed8e2670e7af64417c4/src/main/clojure/clojure/math/combinatorics.clj#L69
(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]      
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
	(let [cnt (count items)]
	  (cond (> n cnt) nil
		(= n cnt) (list (seq items))
		:else
		(map #(map v-items %) (index-combinations n cnt)))))))

(defn pipe []
  "http://clj-me.cgrand.net/2010/04/02/pipe-dreams-are-not-necessarily-made-of-promises/" 
  (let [q (java.util.concurrent.LinkedBlockingQueue.)
        EOQ (Object.)
        NIL (Object.)
        s (fn s [] (lazy-seq (let [x (.take q)]
                               (when-not (= EOQ x)
                                 (cons (when-not (= NIL x) x) (s))))))]
    [(s) (fn ([] (.put q EOQ)) ([x] (.put q (or x NIL))))]))


(defn pipe-seq
  "http://www.pitheringabout.com/?p=874&utm_source=dlvr.it&utm_medium=twitter&utm_campaign=pmap-styled-pipes
   Consumes the col with function f returning a new lazy seq. 
   The consumption is done in parallel using n-threads backed 
   by a queue of the specified size. The output sequence is also
   backed by a queue of the same given size."
  [f n-threads pipe-size col]
  (let [q (java.util.concurrent.LinkedBlockingQueue. pipe-size)
        finished-feeding (promise)
        latch (java.util.concurrent.CountDownLatch. n-threads)
        [out-seq out-queue] (pipe pipe-size)]
 
    ;; Feeder thread
    (future
      (doseq [v (remove nil? col)]
        (.put q v))
      (deliver finished-feeding true))
 
    (dotimes [i n-threads]
      (future (try (loop []
                     (let [v (.poll q 50 java.util.concurrent.TimeUnit/MILLISECONDS)]
                       (when v (out-queue (f v)))
                       (when-not (and (zero? (.size q))
                                      (realized? finished-feeding))
                         (recur))))
                   (finally
                     (.countDown latch)))))
 
    ;; Supervisor thread
    (future
      (.await latch)
      (out-queue))
 
    out-seq))


(defn levenshtein-distance
  "levenshtein distance" 
  [a b]
  (let [x (vec a)
        y (vec b)]
    (letfn
        [(L [l m]
           (let [l- (- l 1)
                 m- (- m 1)]
             (cond (= 0 l) m
                   (= 0 m) l
                   (= (x l-) (y m-)) (L l- m-)
                   :else (+ 1 (min
                                   (L l m-)
                                   (L l- m)
                                   (L l- m-))))))]
      (L (count x) (count y)))))









;;------------junk

;(defn- avg-points-p 
;              "avg numeric or most frequent nominal only good 10000+"
;              ([ typeVec & points ] 
;               (r/fold (fn ([] (random-point typeVec))
;                          ([va vb] 
;                             (mapv   
;                              (fn [t a b]
;                                (cond
;                                   (coll? type)   (if (= 0 (rand-int 2)) a b )
;                                   (= t java.lang.Double)    (/ (+ a b) 2)
;                                 ))
;                               typeVec va vb  )))
;                       points
;                         ))
;              ([typeVec] "in case of no points" (random-point typeVec)  )  )
;            (time (apply (partial avg-points t ) (shuff2 1000000) ))