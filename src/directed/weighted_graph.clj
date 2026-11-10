(ns directed.weighted-graph
  (:require [directed.arc :refer [arc]]
            [directed.graph :refer [arcs make-graph]]
            [utils :refer [???]]))

(defn make-weighted-graph [graph arc-weights]
  (assoc graph :weights (into {} (map (fn [e] [e (arc-weights e)])
                                      (arcs graph)))))

(defn weighted-graph [vertices & weighted-arcs]
  (let [weights (into {} (map (fn [[a b w]] [(arc a b) w])
                              weighted-arcs))]
    (make-weighted-graph (make-graph vertices (keys weights))
                         weights)))

(defn weights [graph]
  (:weights graph))

(defn arc-length [graph from to]
  ((weights graph) (arc from to)))

(defn route-length [graph route]
  (???))
