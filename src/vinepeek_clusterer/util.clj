(ns vinepeek-clusterer.util)

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