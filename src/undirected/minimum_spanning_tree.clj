(ns undirected.minimum-spanning-tree
  (:require [undirected.connected :refer [connected-vertices? connected?]]
            [undirected.edge :as e]
            [undirected.graph :as g :refer [add-edge empty-graph]]
            [undirected.tree :refer :all]
            [undirected.weighted-graph :refer [make-weighted-graph weights]]
            [utils :refer [???]]))

(defn spanning-tree? [graph tree]
  {:pre [(connected? graph)]}
  (???))

(defn edge-incident-some-edge? [graph edge]
  {:pre [(not (g/contains-edge? graph edge))]}
  (???))

(defn edge-creates-cycle? [graph edge]
  {:pre [(not (g/contains-edge? graph edge))]}
  (???))

(defn minimum-spanning-tree-prim [graph]
  {:pre  [(connected? graph)]
   :post [(spanning-tree? graph %)]}
  (let [edges (sort-by (weights graph) (g/edges graph))]
    (loop [g (g/add-edge (empty-graph (g/vertices graph)) (first edges))
           remaining-edges (rest edges)]
      (if (edge-count-tree? g)
        (make-weighted-graph g (weights graph))
        (???)))))

(defn minimum-spanning-tree-kruskal [graph]
  {:pre  [(connected? graph)]
   :post [(spanning-tree? graph %)]}
  (loop [g (empty-graph (g/vertices graph))
         remaining-edges (sort-by (weights graph) (g/edges graph))]
    (if (edge-count-tree? g)
      (make-weighted-graph g (weights graph))
      (???))))
