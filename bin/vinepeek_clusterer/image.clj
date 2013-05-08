(ns vinepeek-clusterer.image
  (:import java.awt.image.BufferedImage
           java.awt.Color
           javax.imageio.IO
           java.awt.image.BufferedImage)
  (:use [clojure.java.io :only [as-file input-stream output-stream] :as io])
  )


(defn- avg-color [png]
    (let [ img  (javax.imageio.ImageIO/read (as-file png))
           simg (new BufferedImage 1 1 (.getType img))
           g    (.createGraphics simg) ]
	    (.drawImage g img 0 0 1 1 nil)
	    (.dispose g)
	    (new Color (.getRGB simg 0 0))
	        ))

(defn process [png]
  (let [ avgColor (avg-color png)]
    { :avgRed (.getRed avgColor)
      :avgGreen (.getGreen avgColor)
      :avgBlue  (.getBlue avgColor)
     }
  ))