(ns undirected.weighted-graph
  (:require [undirected.edge :refer [edge]]
            [undirected.graph :refer [edges make-graph]]
            [utils :refer [???]]))

(defn make-weighted-graph [graph edge-weights]
  (assoc graph :weights (into {} (map (fn [e] [e (edge-weights e)])
                                      (edges graph)))))

(defn weighted-graph [vertices & weighted-edges]
  (let [weights (into {} (map (fn [[a b w]] [(edge a b) w])
                              weighted-edges))]
    (make-weighted-graph (make-graph vertices (keys weights))
                         weights)))

(defn weights [weighted-graph]
  (:weights weighted-graph))

(defn total-weight [weighted-graph]
  (???))
