(ns vinepeek-clusterer.crawler
    (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))


	(defn dissoc-in ;;apparently not a built-in function
			  "Dissociates an entry from a nested associative structure returning a new
			  nested structure. keys is a sequence of keys. Any empty maps that result
			  will not be present in the new structure."
			  [m [k & ks :as keys]]
			  (if ks
			    (if-let [nextmap (get m k)]
			      (let [newmap (dissoc-in nextmap ks)]
			        (if (seq newmap)
			          (assoc m k newmap)
			          (dissoc m k)))
			      m)
			    (dissoc m k)))

(defn get-current-vine [] 
  "a get this this url seems to return everything needed"
  (:body (client/get "http://vinepeek.com/videos" {:as :json})))



(defn filter-vine [vineMap]
  "remove probably unneccesary values"
  (reduce dissoc-in vineMap 
        '([:tweet :in_reply_to_status_id_str]
          [:tweet :favorited]
          [:tweet :in_reply_to_screen_name]
          [:tweet :user :profile_sidebar_fill_color]
          [:tweet :user :profile_background_color]
          [:tweet :user :notifications]
          [:tweet :user :verified]
          [:tweet :user :profile_background_tile]
          [:tweet :user :profile_sidebar_border_color]
          [:tweet :user :default_profile]
          [:tweet :user :protected]
          [:tweet :user :is_translator]
          [:tweet :user :following]
          [:tweet :user :profile_link_color]
          [:tweet :user :follow_request_sent]
          [:tweet :user :profile_use_background_image]
          [:tweet :user :contributors_enabled]
          [:tweet :in_reply_to_user_id_str]
          [:tweet :retweet_count]
          [:tweet :metadata ]
          [:tweet :in_reply_to_status_id]
          [:tweet :contributors]
          [:tweet :in_reply_to_user_id]
          [:tweet :possibly_sensitive]
          [:tweet :retweeted]
          [:tweet :truncated]
          [:channel_id ]
          [:approved]
          [:moderated])))
    
(defn write-json-to-file [vineMap path]
  (spit path (json/write-str vineMap)))

(defn download-video [vineMap path]
  (with-open [writer (clojure.java.io/output-stream path)]
     (.write writer 
       (:body (client/get (:video_url vineMap) {:as :byte-array}))
       )))

(defn decode-mp4 [ mainOutputDirPath videoPath]
  "refering to the mp4Decoder java project"
  (mp4Decoder.Decoder/decodeMp4 mainOutputDirPath videoPath))

(defn do-all-the-above [videoDir outputsDir]
  (let [vineMap   (filter-vine (get-current-vine))
        dateStr   (clojure.string/replace (:created_at vineMap) #":" ".")
        videoPath (str videoDir "/" dateStr ".mp4" )]
       (download-video vineMap videoPath)
       (decode-mp4 outputsDir videoPath)
       (write-json-to-file vineMap (str outputsDir "/" dateStr "/" "data.json"))
       (:text (:tweet vineMap))
    )
  )

(defn repeat-above 
  ([videoDir outputsDir times]  
    (dotimes [n times] (do  (. Thread (sleep 6000)) 
                         (do-all-the-above videoDir outputsDir))))
    )

; (def videos  "C:/Users/rzhng/workspace/mp4Decoder")
; (def outputs "C:/Users/rzhng/workspace/mp4Decoder/outputs")
; (repeat-above videos outputs 5)


