(ns vinepeek-clusterer.firstnames
  (:use [clojure.string :only [split split-lines capitalize]]) )


(def first-name-to-gender-map 
  (let [csvPath (str (. (java.io.File. ".") getCanonicalPath) "\\" "firstnames.csv") ]
    (into {} (map 
            (fn [namegen] (let [[first gender] (split namegen #",")] 
                            {(capitalize first) (= "F" gender)} ))
            (-> csvPath
              (slurp)
              (split-lines)))
          )))
      
 (defn female? [name]
   (-> name
     (split #" ")
     (first)
     (capitalize)
     (first-name-to-gender-map "?")))
 
 (defn gender-diff [v1 v2]
    (let [v1g (female? (:name (:profile v1)))
          v2g (female? (:name (:profile v2)))]
      (cond
        (or (= "?" v1g) (= "?" v2g)) 0.5
        (= v1g v2g) 0.0
        :else 1.0)))