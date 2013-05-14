(ns vinepeek-clusterer.crawler
    (:require [clj-http.client :as client]
              [clojure.data.json :as json]
              [clojure.java.io :as io])
    (:use vinepeek-clusterer.vine-parse)
    )

(defn get-current-vine [] 
  "a get this this url seems to return everything needed"
  (:body (client/get "http://vpeeker.com/videos" {:as :json})))

(defn download-video [vineMap path]
  (with-open [writer (clojure.java.io/output-stream path)]
     (.write writer 
       (:body (client/get (:video_url vineMap) {:as :byte-array}))
       )))

(defn decode-mp4 [ mainOutputDirPath videoPath]
  "refering to the mp4Decoder java project"
  (mp4Decoder.Decoder/decodeMp4 mainOutputDirPath videoPath))



