(ns vinepeek-clusterer.core
  (:use [clj-lazy-ml.k-means-clustering :as cluster]
       [clojure.java.io :only [delete-file]]
       [vinepeek-clusterer 
            [crawler    :as c ]
            [vine-parse :as v ]
            [image      :as i ]
            [wav        :as w ]]
       ))


  ;create clusterer
  ;loop every 6 seconds
	  ;get vine
	   ;extract twitter
	   ;extract avg color
	   ;extract fingerprint
	  ;add to clusterer
	  ;display centroids listings img
  
  ;every hour push to db
  ;display historical centroids 
  
(defn setup-clusterer []
  (decaying-kmeans-clusterer 
    5
    []
  ))

(defn get-vine []  (v/modify-vine (c/get-current-vine)))
  
(defn path []  (. (java.io.File. ".") getCanonicalPath))

(defn add-video-info [vineMap wavPrint & colors]
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
                    (i/process-image (str idPath "\\snap2.png")))
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
  
  
(def cfg { :opDir "op"})

(defn -main  [& args]
  

  )
