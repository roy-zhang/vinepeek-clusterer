(ns vinepeek-clusterer.image
  (:import java.awt.Color
           java.awt.image.BufferedImage)
  (:use [clojure.java.io :only [as-file input-stream output-stream] :as io])
  )


(defn scale-down [png newDim]
  "png path and squard dim to list of [r g b]"
  (when (.exists (as-file png))
    (let [ img  (javax.imageio.ImageIO/read (as-file png))
           simg (new BufferedImage newDim newDim (.getType img))
           g    (.createGraphics simg) ]
	    (.drawImage g img 0 0 newDim newDim nil)
	    (.dispose g)
        (for [x (range newDim)
              y (range newDim)]
          (let [color (new Color (.getRGB simg x y))]
            (vector (.getRed color) (.getGreen color) (.getBlue color)))
          )
	        )))

     (defn color-diff-score [c1 c2]
      (Math/abs (/
         (apply + (map - c1 c2))
         (double (* 3 255))) )
        )

(defn img-sim-score [colors1 colors2]
  "0 for same, 1 for opposite"
  (/ 
    (apply + (map color-diff-score colors1 colors2))
    (count colors1)
  ))


(defn invert-rows [grid]
  "flip rows into lists of column values also magically O(1)?"
   (apply mapv vector  grid))

(defn avg-colors [& colors]
  (mapv (fn [rgbs] 
         (mapv (fn [cs] 
                (/ (apply + cs) 
                   (double (count cs))))
              (invert-rows rgbs) ))
        (invert-rows colors))
  )
