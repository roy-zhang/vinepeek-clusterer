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

(def cfg { :opDir "op"})a

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

   (defn rearrage-vine-for-clusterer [vine]
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
  
  (defn slurp-vine [txtPath]
    (let [path       (str (path) "\\" (:opDir cfg) "\\" txtPath)
          jsoned     (json/read-str (slurp path))]
      (into {} 
            (for [[k v] jsoned] { (keyword k) v}))
      ))
    
  (defn list-files
    ([] 
      (seq (.list (java.io.File. (str (path) "\\" (:opDir cfg))))))
        
    ([ending]
      (filter (fn [string] (= (map identity ending) 
                              (take-last (count ending) string)))
        (list-files) ))
    )
  
(defn loop-process  [times]
  (let [clusterer* (atom (setup-clusterer))]
    (dotimes [n times]
      (try
	      (do (. Thread (sleep 6000))
	        (->> (c/get-current-vine)
	             (v/modify-vine)
	             (extract-images-and-wav )
	             (rearrage-vine-for-clusterer )
	             (swap! clusterer* conj)
	             ((fn [clusterer] (println (centroids clusterer))))
             ))
         (catch Exception e "")
       ))))

(defn loop-vine-retention  [times]
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
   

(defn -main  [& args]
  (loop-process 100)

  )
