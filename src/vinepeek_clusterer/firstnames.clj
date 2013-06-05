(ns vinepeek-clusterer.firstnames
  (:use [clojure.string :only [split split-lines capitalize]]) )


(def first-name-to-gender-map 
  (let [csvPath (str (. (java.io.File. ".") getCanonicalPath) "/" "firstnames.csv") ]
    (into {} (map 
            (fn [namegen] (let [[first gender] (split namegen #",")] 
                            {(capitalize first) (= "F" gender)} ))
            (-> csvPath
              (slurp)
              (split-lines)))
          )))
      
 (defn female? [name]
   (if (or (true? name) (false? name)) name
   (-> name
     (split #" ")
     (first)
     (capitalize)
     (first-name-to-gender-map "?"))))
 
 