(ns the-divine-cheese-code.visualization.svg
  (:require [clojure.string :as s])
  (:refer-clojure :exclude [min max]))

(defn comparator-over-maps ;; 1
  [comparison-fn ks]
  (fn [maps]
    (zipmap ks               ;; 2
            (map (fn [k] (apply comparison-fn (map k maps))) ;; 3
                 ks))))

(def min (comparator-over-maps clojure.core/min [:lat :lng])) ;; 4
(def max (comparator-over-maps clojure.core/max [:lat :lng]))

(defn translate-to-00
  [locations]
  (let [mincoords (min locations)] (map #(merge-with - % mincoords) locations)))

(defn scale
  [width height locations]
  (let [maxcoords (max locations)
        ratio {:lat (/ height (:lat maxcoords))
               :lng (/ width (:lng maxcoords))}]
    (map #(merge-with * % ratio) locations)))

(defn latlng->point
  "Convert lat/lng map to  comma-separated string"
  [latlng]
  (str (:lng latlng) "," (:lat latlng)))

(defn points
  "Given a seq of lat/lng maps, return string of points joined by space"
  [locations]
  (s/join " " (map latlng->point locations)))

(defn line
  [points]
  (str "<polyline points=\"" points "\" />"))

(defn transform
  "Chain other functions"
  [width height locations]
  (->> locations
       translate-to-00
       (scale width height)))

(defn xml
  "svg 'template', which also flips coordinate system"
  [width height locations]
  (str "<svg height =\"" height "\" width=\"" width "\">"
       ;; these two g flip the coordinate system
       "<g transform=\"translate(0," height ")\">"
       "<g transform=\"scale(1, -1)\">"
       (-> (transform width height locations)
           points
           line)
       "</g></g>"
       "</svg>"))
