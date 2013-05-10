(ns vinepeek-clusterer.core
  (:use [clj-lazy-ml.k-means-clustering]
       [clojure.java.io :only [delete-file]]
       [vinepeek-clusterer 
            [crawler    :as c ]
            [vine-parse :as v ]
            [image      :as i ]
            [wav        :as w ]]
       ))

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
    (delete-file videoPath true)
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
       (:lang            (:profile vine)) ;language  eg  "en" 
       (:mentionsCount   vine)            ;how many people @ed in tweet 
       (:local_hour      vine)            ;local time of tweet  ex. 9.583 for 9:35ish
       (:wavPrint        vine)            ;wav fingerprint
       (first (:colors   vine)            ; first average color of video
       )
     ))

(defn add-to-clusterer [clusterer vine] (cons clusterer (rearrage-vine-for-clusterer vine)))
  
(defn loop-process  [times]
  (let [clusterer* (atom (setup-clusterer))]
    (dotimes [n times]
      (do (. Thread (sleep 6000))
        (->> (c/get-current-vine)
             (v/modify-vine)
             (extract-images-and-wav )
             (swap! clusterer* add-to-clusterer)
             ((fn [clusterer] (println (centroids clusterer))))
             
          )))))
   

(defn -main  [& args]
  (loop-process 5)

  )
