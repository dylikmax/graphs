(ns undirected.minimum-spanning-tree
  (:require [undirected.connected :refer [connected-vertices? connected?]]
            [undirected.edge :as e]
            [undirected.graph :as g :refer [add-edge empty-graph]]
            [undirected.tree :refer :all]
            [undirected.weighted-graph :refer [make-weighted-graph weights]]
            [utils :refer [???]]))

(defn spanning-tree? [graph tree]
  {:pre [(connected? graph)]}
  (and (tree? tree)
       (= (g/vertices graph)
          (g/vertices tree))
       (g/subgraph? tree graph)))


(defn edge-incident-some-edge? [graph edge]
  {:pre [(not (g/contains-edge? graph edge))]}
  (some #(e/edges-incident? edge %) (g/edges graph)))

(defn edge-creates-cycle? [graph edge]
  {:pre [(not (g/contains-edge? graph edge))]}
  (connected-vertices? graph (first (e/ends edge)) (second (e/ends edge))))

(defn minimum-spanning-tree-prim [graph]
  {:pre  [(connected? graph)]
   :post [(spanning-tree? graph %)]}
  (let [all-edges (g/edges graph)
        w (weights graph)
        first-edge (apply min-key w all-edges)
        initial-g (g/add-edge (empty-graph (g/vertices graph)) first-edge)]
    (loop [g initial-g]
      (if (edge-count-tree? g)
        (make-weighted-graph g w)
        (let [valid-edges (filter (fn [e]
                                    (and (not (g/contains-edge? g e))
                                         (edge-incident-some-edge? g e)
                                         (not (edge-creates-cycle? g e))))
                                  all-edges)
              next-edge (apply min-key w valid-edges)]
          (recur (g/add-edge g next-edge)))))))

(defn minimum-spanning-tree-kruskal [graph]
  {:pre  [(connected? graph)]
   :post [(spanning-tree? graph %)]}
  (loop [g (empty-graph (g/vertices graph))
         remaining-edges (sort-by (weights graph) (g/edges graph))]
    (if (edge-count-tree? g)
      (make-weighted-graph g (weights graph))
      (let [next-edge (first remaining-edges)]
        (if (not (edge-creates-cycle? g next-edge))
          (recur (g/add-edge g next-edge) (rest remaining-edges))
          (recur g (rest remaining-edges)))))))