(ns clj-txt2img.core
  (:require [clojure.string :as cstr])
  (:import (java.awt Font Color Graphics2D
                     FontMetrics RenderingHints
                     Toolkit)
           (java.io File)
           (java.nio.file Files)
           (javax.imageio ImageIO)
           (java.awt.image BufferedImage)
           (clojure.lang Delay)))

(set! *warn-on-reflection* true)

(def default-options
  ; make sure to use monospace fonts
  {:font         (delay (Font. "Courier New" Font/PLAIN 22))
   :padding-top  16
   :padding-left 16
   :background   Color/WHITE
   :foreground   Color/BLACK
   :margin-top   -2
   ; supported formats jpeg, png, bmp, wbmp, gif
   :format       "png"})

(defn- with-graphics [^Graphics2D graphics f]
  (try
    (f graphics)
    (catch Exception e
      (.dispose graphics)
      (throw e))))

(defn- get-image-size [lines opts]
  (let [max-line   (apply max-key count lines)
        line-count (count lines)

        ^BufferedImage
        img        (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D
        graphics   (doto (.createGraphics img) (.setFont @(:font opts)))]
    (with-graphics graphics
                   (fn [^Graphics2D graphics]
                     (let [^FontMetrics
                           font-metrics (.getFontMetrics graphics)

                           width        (+ (.stringWidth font-metrics max-line)
                                           (* 2 (:padding-left opts)))
                           height       (+ (* line-count (.getAscent font-metrics))
                                           (* 2 (:padding-top opts)))]
                       [width height])))))

(defn- set-hints-for-best-quality [^Graphics2D g]
  (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
  (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR)
  (.setRenderingHint g RenderingHints/KEY_COLOR_RENDERING RenderingHints/VALUE_COLOR_RENDER_QUALITY)
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC)
  (.setRenderingHint g RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY)
  (.setRenderingHint g RenderingHints/KEY_STROKE_CONTROL RenderingHints/VALUE_STROKE_PURE)
  (let [desktop-hints
        (.getDesktopProperty
          (Toolkit/getDefaultToolkit)
          "awt.font.desktophints")]
    (when desktop-hints
      (doseq [[k v] desktop-hints]
        (.setRenderingHint g k v)))))

(defn ^File text-to-image [msg & [opts]]
  (let [opts
        (merge default-options opts)

        font
        (let [f (:font opts)]
          (if (instance? Delay f) @f f))

        lines
        (cstr/split msg #"\n")

        [w h]
        (get-image-size lines opts)

        ^BufferedImage img
        (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D graphics
        (doto (.createGraphics img)
          (set-hints-for-best-quality)
          (.setFont font)
          (.setBackground (:background opts))
          (.setColor (:foreground opts)))
        ^FontMetrics font-metrics
        (.getFontMetrics graphics)

        out-file
        (File/createTempFile "clj-tab-img" (str "." (:format opts)))]
    (try
      (doseq [[i ^String v] (map-indexed vector lines)
              :let [^Long left (:padding-left opts)
                    ^Long top  (+ (+ (:padding-top opts) (:margin-top opts))
                                  (* (inc i) (.getAscent font-metrics)))]]
        (.drawString graphics v left top))
      (ImageIO/write img ^String (:format opts) out-file)
      out-file
      (catch Exception e
        (.dispose graphics)
        (throw e))
      (finally
        (.deleteOnExit out-file)
        (.dispose graphics)))))

(defn text-to-image-bytes [msg & [opts]]
  (let [image-file (text-to-image msg opts)]
    (Files/readAllBytes (.toPath image-file))))

(def t2i text-to-image)
(def t2ib text-to-image-bytes)
