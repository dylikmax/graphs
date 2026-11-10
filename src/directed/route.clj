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
  (count (arcs route)))

(defn start [route]
  (first (vertices route)))

(defn end [route]
  (last (vertices route)))

(defn route->graph [route]
  (g/make-graph (set (vertices route)) (set (arcs route))))

(defn graph-contains-route? [graph route]
  (g/subgraph? (route->graph route) graph))

(defn chain? [route]
  (all-distinct? (arcs route)))

(defn- simple? [route]
  (or (all-distinct? (vertices route))
      (and (= (start route) (end route))
           (all-distinct? (rest (vertices route))))))

(defn path? [route]
  (and (chain? route) (simple? route)))

(defn cyclic? [route]
  (= (start route) (end route)))

(defn cycle? [route]
  (and (chain? route) (cyclic? route)))

(defn circuit? [route]
  (and (path? route) (cyclic? route)))
