(ns vinepeek-clusterer.image
  (:import java.awt.Color
           java.awt.image.BufferedImage)
  (:use [clojure.java.io :only [as-file input-stream output-stream] :as io])
  )


(defn- avg-color [png]
  (when (.exists (as-file png))
    (let [ img  (javax.imageio.ImageIO/read (as-file png))
           simg (new BufferedImage 1 1 (.getType img))
           g    (.createGraphics simg) ]
	    (.drawImage g img 0 0 1 1 nil)
	    (.dispose g)
	    (new Color (.getRGB simg 0 0))
	        )))


(defn process-image [png]
  (let [ avgColor (avg-color png)]
    [(.getRed avgColor)
     (.getGreen avgColor)
     (.getBlue avgColor)
     ]
  ))