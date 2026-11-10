(ns directed.route
  (:require [directed.arc :refer [make-arc]]
            [directed.graph :as g]
            [utils :refer [all-distinct? consecutive-pairs ???]]))

(defn make-route [vertices]
  {:vertices (vec vertices)
   :arcs     (vec (map make-arc (consecutive-pairs vertices)))})

(defn route [& vertices]
  (make-route vertices))

(defn vertices [route]
  (:vertices route))

(defn arcs [route]
  (:arcs route))

(defn length [route]
  (???))

(defn start [route]
  (???))

(defn end [route]
  (???))

(defn route->graph [route]
  (???))

(defn graph-contains-route? [graph route]
  (???))

(defn chain? [route]
  (???))

(defn- simple? [route]
  (or (all-distinct? (vertices route))
      (and (= (start route) (end route))
           (all-distinct? (rest (vertices route))))))

(defn path? [route]
  (???))

(defn cyclic? [route]
  (???))

(defn cycle? [route]
  (???))

(defn circuit? [route]
  (???))
