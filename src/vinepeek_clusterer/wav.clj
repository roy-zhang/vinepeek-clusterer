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
      (->
      (.getSimilarity 
        (.getFingerprintsSimilarity 
          (FingerprintSimilarityComputer. (vec-to-byte-array fp1) (vec-to-byte-array fp2))))
       (Math/log)
       (- )
      ))
    
    (defn avg-wav-fingerprint [simFunc & fingerprints]
      "since blending sound seems non-trivial, 
   this will just return the fingerprint with the most similarity to all the others"
      
      (apply (partial max-key 
                      (fn [fprint] (apply + (map #(simFunc fprint %) fingerprints))))
             fingerprints)
      )
    
    (defn avg-wav-fingerprint-2 [& fingerprints]
      ""
      (let [somefp (rand-nth fingerprints)]
        (reduce (fn [fp1 fp2] (if (> (similarity somefp fp1) (similarity somefp fp2)) fp1 fp2))  fingerprints)
        )
      )

(defn image-wav-normalized [wav jpg]
  ""
  (-> wav
      (Wave. )
      (Spectrogram.)
      (.getNormalizedSpectrogramData)
      ( #(.renderSpectrogramData (new GraphicRender) % jpg) )
 ))