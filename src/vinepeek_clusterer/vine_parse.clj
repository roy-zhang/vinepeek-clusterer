(ns vinepeek-clusterer.vine-parse
  (:use vinepeek-clusterer.util
        [clj-time core format]
        [vinepeek-clusterer.firstnames]))

(defn- filter-vine [vineMap]
  (reduce dissoc-in vineMap 
        '([:channel_id ]
          [:approved]
          [:moderated]
          [:updated-at]
          
          [:tweet :in_reply_to_status_id_str]
          [:tweet :favorited]
          [:tweet :in_reply_to_screen_name]
          [:tweet :in_reply_to_user_id_str]
          [:tweet :retweet_count]
          [:tweet :metadata ]
          [:tweet :in_reply_to_status_id]
          [:tweet :contributors]
          [:tweet :in_reply_to_user_id]
          [:tweet :possibly_sensitive]
          [:tweet :retweeted]
          [:tweet :source]
          [:tweet :truncated]
          
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
          [:tweet :user :profile_background_image_url]
          [:tweet :user :default_profile_image]
          [:tweet :user :listed_count]
          [:tweet :user :profile_banner_url]
          [:tweet :user :profile_image_url_https]
          [:tweet :user :profile_image_url]
          [:tweet :user :profile_background_image_url_https]
          [:tweet :user :profile_text_color]
          [:tweet :user :profile_use_background_image]
          ))
  )

(defn- rearrange-vine [vineMap]
    (-> vineMap
	    (assoc :hashtags      (get-in vineMap [:tweet :entities :hashtags]))
	    (assoc :url           (get-in vineMap [:tweet :entities :urls 0 :url]))
	    (assoc :mentionsCount (count (get-in vineMap [:tweet :entities :user_mentions])))
	    (dissoc-in [:tweet :entities ])
	    (assoc :profile       (get-in vineMap [:tweet :user] ))
	    (dissoc-in [:tweet :user])
	    (dissoc-in [:profile :entities])
    )
  )


;assuming  vines are modfied from this point

		(defn- vine-local-time [vineMap]
		  (let [created-at (get-in vineMap [:created_at])
		        utc_offset (get-in vineMap [:profile :utc_offset])]
		  (to-time-zone   
		    (parse (formatters :date-time-no-ms) created-at)
		    (time-zone-for-offset (/ utc_offset 3600))
		    )))
		
		(defn- hours-local-time [vineMap]
		  (let [local (vine-local-time vineMap)]
		    (+ (hour local) (/ (minute local) 60.0))
		  ))
		
		(defn- profile-age-in-weeks [vineMap]
		  (in-weeks (interval 
		              (parse (formatter "EEE MMM dd HH:mm:ss Z yyyy") 
		                     (get-in vineMap [:profile :created_at]))
		              (now))))
  
         (defn- remove-url-from-text [vineMap]
           (update-in vineMap [:tweet :text] 
                      #(clojure.string/replace % (re-pattern (:url vineMap)) "" )  )
           )

	(defn- add-extras-to-vine [vineMap]
	  (-> vineMap
	    (assoc :local_hour (hours-local-time vineMap))
	    (assoc-in [:profile :weeks_old] (profile-age-in-weeks vineMap))
        (assoc-in [:profile :female?]   (female? (:name (:profile vineMap))))
        (remove-url-from-text)
	  ))

(defn modify-vine [vineMap]
  "remove probably unneccesary values, move up profiles"
   (-> vineMap
     (filter-vine)
     (rearrange-vine)
     (add-extras-to-vine))
  )



