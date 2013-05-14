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
    
        (defn vec-to-byte-array [vec]
          (byte-array (map byte vec))
          )
    
    (defn similarity [fp1 fp2]
      (.getSimilarity 
        (.getFingerprintsSimilarity 
          (FingerprintSimilarityComputer. (vec-to-byte-array fp1) (vec-to-byte-array fp2)))
      ))


(defn image-wav-normalized [wav jpg]
  ""
  (-> wav
      (Wave. )
      (Spectrogram.)
      (.getNormalizedSpectrogramData)
      ( #(.renderSpectrogramData (new GraphicRender) % jpg) )
 ))