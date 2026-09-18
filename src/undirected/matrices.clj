(ns undirected.matrices
  (:require [undirected.edge :as e]
            [undirected.graph :as g]
            [utils :refer []]))

(defn adjacency-matrix [graph]
  (let [vertices (sort (g/vertices graph))]
    (for [v1 vertices]
      (for [v2 vertices]
        (if (g/adjacent? graph v1 v2)
          1
          0)))))

(defn incidence-matrix [graph]
  (let [edges    (sort-by (fn [e] (apply str (sort (e/ends e)))) (g/edges graph))
        vertices (sort (g/vertices graph))]
    (for [e edges]
      (for [v vertices]
        (if (e/incident? e v)
          1
          0)))))