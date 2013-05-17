(ns vinepeek-clusterer.core
  (:use 
    [clojure.java.io :as io]
    [clojure.set]
    [clj-lazy-ml.forgetful-clusterer]
    [clojure.data.json :only [write-str read-str]]
        [vinepeek-clusterer 
            [crawler    :as c ]
            [vine-parse :as v ]
            [image      :as i ]
            [wav        :as w ]
            [clustering :as ml]]
       )
    (:import (java.io File)
           (java.net URL URI)))

(def cfg { :opDir "op"})

(defn path []  (. (java.io.File. ".") getCanonicalPath))
  
(defn copy-file [source-path dest-path]
  (io/copy (io/file source-path) (io/file dest-path)))

  (defn list-files
    ([path] 
      (seq (.list (java.io.File. path))))
    ([path ending]
      (filter (fn [string] (= (map identity ending) (take-last (count ending) string)))
        (list-files path) ))
    )

  
;vine extraction
	(defn- extract-images-and-wav [vine]
	  (let [opPath       (str (path) "\\" (:opDir cfg) "\\")
	        idPath       (str opPath (:id vine))
	        videoPath    (str idPath ".mp4" )
	        wavPath      (str idPath ".wav")
	        spectrumPath (str idPath "_spectrum.jpg")
            image1Path   (str idPath "_snap1.png")
            image2Path   (str idPath "_snap2.png")
            ]
     
	    (c/download-video vine videoPath)
	    (mp4Decoder.Decoder/decodeMp4 opPath videoPath)
	    (w/image-wav-normalized wavPath spectrumPath)
	    
     (-> vine
       (assoc :wavPrint (w/fingerprint wavPath) )
       (assoc :localPath { :video    videoPath
                           :wav      wavPath
                           :spectrum spectrumPath
                           :image1   image1Path
                           :image2   image2Path })
       (assoc :image1 (i/scale-down image1Path 10))
	    )))
 
 (defn get-vine []
  (->> (c/get-current-vine)
    (v/modify-vine)
    (extract-images-and-wav )
    ))
 
 



;de/serializing
  (defn spit-vine [vine]
    (let [newPath   (str (path) "\\" (:opDir cfg) "\\" (:id vine) ".txt")]
      (spit newPath (write-str vine)) vine ))
  
  (defn slurp-vine
    "deserializes txt file in op folder"
    ([txtPath]
      (let [path       (str (path) "\\" (:opDir cfg) "\\" txtPath)
            jsoned     (read-str (slurp path))]
        (into {} (for [[k v] jsoned] { (keyword k) v}))))
    ([subFolder txtPath]
      (slurp-vine (str subFolder "\\" txtPath)))
    )
    


(defn loop-vine-retention  [times]
  "just downloads a new vine every 3 seconds and extracts fingerprint"
    (dotimes [n times]
      (try 
      (do (. Thread (sleep 3000))
        (c/get-current-vine)
             
          )
      (catch Exception e "")
      )))
   


(defn just-wav-experiment []
"get 2 groups of vines
 shuffle then cluster into 2 groups 
 check how many were grouped incorrectly
 get new vines, and attempt to catch similar into clusters"

(let 
  [outdoorsVines   (map (partial slurp-vine "outdoors") 
                        (list-files (str (path) "\\" (:opDir cfg) "\\" "outdoors") "txt"))
   indoorpetsVines (map (partial slurp-vine "indoorpets") 
                        (list-files (str (path) "\\" (:opDir cfg) "\\" "indoorpets") "txt"))
   clustered       (add-points
                     (ml/setup-just-fingerprint-clusterer)
                     (shuffle (concat outdoorsVines indoorpetsVines)))
   cluster1        (seq (first  (vals (:centroidMaps clustered))))
   cluster2        (seq (second (vals (:centroidMaps clustered))))
   
   outdoorsCount   (max (count (intersection (set cluster1) (set outdoorsVines))) 
                        (count (intersection (set cluster2) (set outdoorsVines))))
   indoorsCount    (max (count (intersection (set cluster1) (set indoorpetsVines))) 
                        (count (intersection (set cluster2) (set indoorpetsVines))))
 
   ]
 (println outdoorsCount "/" (count outdoorsVines) " outdoors together")
 (println indoorsCount "/" (count indoorpetsVines) " indoors together")
 
  

  ))




(defn revise-image1 [subfolder newDim vine]
  (let [ newPath   (str (path) "\\" (:opDir cfg) "\\" subfolder "\\" (:id vine) ".txt")            ]
	    (spit newPath (write-str     
                     (-> vine
                       (assoc :image1 (i/scale-down ((:localPath vine) "image1") newDim)))))
  ))



(defn just-img-experiment []
"get 2 groups of vines
 shuffle then cluster into 2 groups 
 check how many were grouped incorrectly
 get new vines, and attempt to catch similar into clusters"

(let 
  [outdoorsVines   (map (partial slurp-vine "outdoors") 
                        (list-files (str (path) "\\" (:opDir cfg) "\\" "outdoors") "txt"))
   indoorpetsVines (map (partial slurp-vine "indoorpets") 
                        (list-files (str (path) "\\" (:opDir cfg) "\\" "indoorpets") "txt"))
   clustered       (add-points
                     (ml/setup-just-img-clusterer)
                     (shuffle (concat outdoorsVines indoorpetsVines)))
   cluster1        (seq (first  (vals (:centroidMaps clustered))))
   cluster2        (seq (second (vals (:centroidMaps clustered))))
   
   outdoorsCount   (max (count (intersection (set cluster1) (set outdoorsVines))) 
                        (count (intersection (set cluster2) (set outdoorsVines))))
   indoorsCount    (max (count (intersection (set cluster1) (set indoorpetsVines))) 
                        (count (intersection (set cluster2) (set indoorpetsVines))))
 
   ]
 (println outdoorsCount "/" (count outdoorsVines) " outdoors together" "   ||   "
            indoorsCount "/" (count indoorpetsVines) " indoors together")
 [outdoorsCount indoorsCount]
  

  ))


(defn repeat-just-img-experiment [trials]
  (let [averages (map (fn [list] (/ (apply + list) (double (count list))))
                      (invert-rows  (take trials (repeatedly just-img-experiment))))]
    
     (println (first averages)  " outdoors together")
     (println (second averages) " indoors together")
  ))





(defn -main  [& args]

  )