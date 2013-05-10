(ns utils.decay
  "net.cgrand.decay  pretty wacky collection by Christopher Grand
  Exponentially decaying lists. See http://awelonblue.wordpress.com/2013/01/24/exponential-decay-of-history-improved/
  for background and http://clj-me.cgrand.net/2013/02/12/decaying-lists-log-scale-for-lists/ for documentation

  removed state
" (:use [plumbing.core])
  )

;; PRNG, formulas straight from java.util.Random javadoc
(defn- seed ^long [^long n]
  (bit-and (unchecked-multiply n 0x5DEECE66D)
    (unchecked-dec (bit-shift-left 1 48))))

(defn- next-seed ^long [^long seed]
  (bit-and (unchecked-add (unchecked-multiply seed 0x5DEECE66D) 0xB)
    (unchecked-dec (bit-shift-left 1 48))))

(defn- take-bits ^long [^long n ^long cnt]
  (bit-shift-right n (- 48 cnt)))

(defn- next-double ^double [^long s1 ^long s2]
  (/ (+ (bit-shift-left (take-bits s1 26) 27) (take-bits s2 27))
    (unchecked-double (bit-shift-left 1 53))))

(defn- geometric ^long [^double r ^double lambda]
  (unchecked-long (/ (Math/log (- 1.0 r)) lambda)))

(defn- decay-split [l ^long n]
  (let [left (doto (object-array (inc n)) (aset n (first l)))]
    (loop [right (next l) n n]
      (if (pos? n)
        (let [n (dec n)]
          (aset left n (first right))
          (recur (next right) n))
        [(seq left) right]))))

(defn keep-latest 
  "Returns the most recent of the two candidates to decay."
  [left right] left)


(defprotocol DecayingCollection
  (decay-loc [coll]
    "returns nil or [left right], a split view of the collection where the next
     collapse is going to happen. (first left) and (first right) are the items
     being collapsed." )
  (replace-list [coll seq])
  )

(deftype DecayingList [lambda ^clojure.lang.ISeq l ^long seed collapse
                       ^:volatile-mutable ^long nnseed ^:volatile-mutable split capacity]
  DecayingCollection
    (decay-loc [coll]
      (or split
        (let [nseed (next-seed seed)
              d (next-double seed nseed)
              n (min (geometric d lambda) (- capacity 2))]
          (set! nnseed (next-seed nseed))
          (when (> (count l) (inc n))
            (decay-split l n)))))
    (replace-list [coll seq]
      (DecayingList. lambda seq nnseed collapse 0 nil capacity))
    clojure.lang.IPersistentCollection
    (count [this] (.count l))
    (cons [this x]
       (let [ l    (if-let [[[left :as lefts] [right :as rights]] (set! split (decay-loc this))]
                     (->    (next rights) 
                       (conj    (let [ [ l r] (decay-loc this)] (collapse (first (unchunk l)) (first (unchunk r)))))
                       (into    (next lefts))
                       )       
                     l)
            dl (DecayingList. lambda (conj l x) nnseed collapse 0 nil capacity)]
        (set! split nil) ; clearing the cache
        dl))
    (empty [this] (DecayingList. lambda () seed collapse 0 nil capacity))
    (equiv [this that] (.equiv l that))
  clojure.lang.Sequential
  clojure.lang.Seqable
    (seq [this] (.seq l))
  clojure.lang.IHashEq
    (hasheq [this] (.hasheq ^clojure.lang.IHashEq l))
  Object
    (hashCode [this] (.hashCode l))
    (equals [this that] (.equals this that)))


(defn decaying-list
  "Returns a new decaying list with specified half-life (10 by default."
  ([] (decaying-list 10))
  ([half-life & {:keys [collapse capacity] 
                 :or {collapse keep-latest
                      capacity Long/MAX_VALUE}}]
    (DecayingList. (- (/ (Math/log 2) half-life)) () 
      (seed (System/currentTimeMillis)) collapse
      0 nil
      capacity)))





