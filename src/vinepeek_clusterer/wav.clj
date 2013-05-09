(ns vinepeek-clusterer.wav
    (:import com.musicg.wave.Wave
             com.musicg.wave.extension.Spectrogram
             com.musicg.graphic.GraphicRender
             com.musicg.fingerprint.FingerprintSimilarity
             com.musicg.fingerprint.FingerprintSimilarityComputer
             com.musicg.fingerprint.FingerprintManager
             java.lang.Math
             ))

    (defn- avg [coll] (/ (apply + coll) (count coll)))
	
	(defn- spec-frame-to-avgs [bins specFrame ]
	  "specFrame is the 256 long dbl array that holds pitch between -1 1"
	  (map avg
	    (partition (quot (count specFrame) bins) specFrame))
	  )

    (defn fingerprint [wav]
      (.getFingerprint (Wave. wav))
      )
    
    (defn similarity [fp1 fp2]
      (.getFingerprintsSimilarity (FingerprintSimilarityComputer. fp1 fp2))
      )


(defn image-wav-normalized [wav jpg]
  ""
  (-> wav
      (Wave. )
      (Spectrogram.)
      (.getNormalizedSpectrogramData)
      ( #(.renderSpectrogramData (new GraphicRender) % jpg) )
 ))



	
	(defn- double-array-2d [ccoll]
	  (into-array (map double-array ccoll)))
 
 
(defn image-wav-absolute [wav jpg]
  (let [absSpecDat (-> wav
                     (Wave. )
                     (Spectrogram.)
                     (.getAbsoluteSpectrogramData)
                     (drop 3) )
        maxDblLogged (Math/log (apply max (map (partial apply max) absSpecDat)))
        normed       (double-array-2d (map (fn [spec] 
                                             (map   #(/ (Math/log %) maxDblLogged) spec))
                                        absSpecDat))  ]
      (.renderSpectrogramData (new GraphicRender) normed jpg)))
