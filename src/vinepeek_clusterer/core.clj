(ns vinepeek-clusterer.core
  (:use [clj-lazy-ml.k-means-clustering]
        [clojure.java.io :only [delete-file]]
        [clojure.data.json :as json]
        [vinepeek-clusterer 
            [crawler    :as c ]
            [vine-parse :as v ]
            [image      :as i ]
            [wav        :as w ]]
       )
    (:import (java.io File)
           (java.net URL URI)))

(def cfg { :opDir "op"})

(defn path []  (. (java.io.File. ".") getCanonicalPath))
  
	(defn- add-video-info [vineMap wavPrint & colors]
	  (-> vineMap
	    (assoc :wavPrint wavPrint)
	    (assoc :colors (take-while identity colors))
	  ))

(defn extract-images-and-wav [vine]
  (let [opPath       (str (path) "\\" (:opDir cfg) "\\")
        idPath       (str opPath (:id vine))
        videoPath    (str idPath ".mp4" )
        wavPath      (str idPath "\\audio.wav")
        spectrumPath (str idPath "\\spectrum.jpg")]
    
    (c/download-video vine videoPath)
    (mp4Decoder.Decoder/decodeMp4 opPath videoPath)
    (w/image-wav-normalized wavPath spectrumPath)
    
    (add-video-info vine (w/fingerprint wavPath) 
                    (i/process-image (str idPath "\\snap0.png"))
                    (i/process-image (str idPath "\\snap1.png"))
                    )
    ))

(defn delete-op-folder [vine]
  (let [opPath       (str (path) "\\" (:opDir cfg) "\\")
        idPath       (str opPath (:id vine))
        videoPath    (str idPath ".mp4" )
        wavPath      (str idPath "\\audio.wav")
        spectrumPath (str idPath "\\spectrum.jpg")]
    ;(delete-file videoPath true)
    (delete-file wavPath true)
    ))

(def languageCodes (vector "en" "es" "pt" "it" "tr" "ko" "fr" "ru" "de" "ja"))

(defn setup-clusterer []
  (decaying-kmeans-clusterer 5 [:double :double :double languageCodes :double :double :wav-fingerprint :RGB]
                             :centroidHalfLife 600 :centroidCapacity 2000 :maxLoops 5))

   (defn rearrange-vine-for-clusterer [vine]
     (vector 
       (:weeks_old       (:profile vine)) ;age of the profile
       (:followers_count (:profile vine)) ;followers
       (:statuses_count  (:profile vine)) ;total tweets
       (:lang            (:profile vine)) ;language 
       (:mentionsCount   vine)            ;how many people @ed in tweet 
       (:local_hour      vine)            ;local time of tweet  ex. 9.583 for 9:35ish
       (:wavPrint        vine)            ;wav fingerprint
       (first (:colors   vine)            ; first average color of video
       )
     ))
   


  (defn spit-vine [vine]
    (let [newPath   (str (path) "\\" (:opDir cfg) "\\" (:id vine) ".txt")]
      (spit newPath (json/write-str vine)) vine ))
  
  (defn slurp-vine
    "deserializes txt file in op folder"
    ([txtPath]
      (let [path       (str (path) "\\" (:opDir cfg) "\\" txtPath)
            jsoned     (json/read-str (slurp path))]
        (into {} (for [[k v] jsoned] { (keyword k) v}))))
    ([subFolder txtPath]
      (slurp-vine (str subFolder "\\" txtPath)))
    )
    
  (defn list-files
    ([path] 
      (seq (.list (java.io.File. path))))
    ([path ending]
      (filter (fn [string] (= (map identity ending) (take-last (count ending) string)))
        (list-files path) ))
    )
  
(defn loop-process  [times]
  "creates a clusterer1 and sticks new vines into it every 6 seconds"
  (let [clusterer* (atom (setup-clusterer))]
    (dotimes [n times]
      (try
	      (do (. Thread (sleep 6000))
	        (->> (c/get-current-vine)
	             (v/modify-vine)
	             (extract-images-and-wav )
	             (rearrange-vine-for-clusterer )
	             (swap! clusterer* conj)
	             ((fn [clusterer] (println (centroids clusterer))))
             ))
         (catch Exception e "")
       ))))

(defn loop-vine-retention  [times]
  "just downloads a new vine every 3 seconds and extracts fingerprint"
    (dotimes [n times]
      (try 
      (do (. Thread (sleep 3000))
        (->> (c/get-current-vine)
             (v/modify-vine)
             (extract-images-and-wav )
             (spit-vine)
             (delete-op-folder)
          ))
      (catch Exception e "")
      )))
   

;testing just the fingerprint with the clusterer
  (defn testing-audio-fingerprint []
    "stick shuffled mix into clusterer, see how many actually get clustered together"
    (let [ rearrange-vine   (fn [vine] (vector (:wavPrint vine) ))
           clusterer         (decaying-kmeans-clusterer 2 [ :wav-fingerprint ]
                             :centroidHalfLife 600 :centroidCapacity 2000 :maxLoops 5)
           
           indoorDir        (str (path) "\\" (:opDir cfg) "\\indoorpets")
           outdoorDir       (str (path) "\\" (:opDir cfg) "\\outdoors")
           indoorRears      (map (comp rearrange-vine (partial slurp-vine  "indoorpets"))
                                 (list-files indoorDir  "txt"))
           outdoorRears     (map (comp rearrange-vine (partial slurp-vine "outdoors")) 
                                 (list-files outdoorDir "txt")) ]
       (println (count indoorRears) " indoors  " (count outdoorRears) " outdoors")
       (println (centroid-maps (into clusterer (shuffle (concat indoorRears outdoorRears)))))
    
    ))






(defn -main  [& args]
  (loop-process 100)

  )



(def rearrange-vine   (fn [vine] (vector (:wavPrint vine) )))
(def          clusterer         (decaying-kmeans-clusterer 2 [ :wav-fingerprint ]
                                                           :centroidHalfLife 600 :centroidCapacity 2000 :maxLoops 5))

(def       indoorDir        (str (path) "\\" (:opDir cfg) "\\indoorpets"))
(def       outdoorDir       (str (path) "\\" (:opDir cfg) "\\outdoors"))
(def      indoorRears      (map (comp rearrange-vine (partial slurp-vine  "indoorpets"))
                                (list-files indoorDir  "txt")))
(def      outdoorRears     (map (comp rearrange-vine (partial slurp-vine "outdoors")) 
                                (list-files outdoorDir "txt")) )



