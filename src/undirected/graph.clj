(ns undirected.graph
  (:require [clojure.set :as set]
            [undirected.edge :as e :refer [edge make-edge]]
            [utils :refer [all-distinct? pairs]]))

(defn make-graph [vertices edges]
  (assert (all-distinct? vertices) "Все вершины графа должны быть различны")
  (assert (all-distinct? edges) "Все рёбра графа должны быть различны")
  (let [vertex-set (set vertices)]
    (assert (every? (fn [e] (set/subset? (e/ends e) vertex-set)) edges)
            "Рёбра должны соединять вершины графа")
    {:vertices vertex-set
     :edges    (set edges)}))

(defn graph [vertices & vertex-pairs]
  (make-graph vertices
              (map make-edge vertex-pairs)))

(defn vertices [graph]
  (:vertices graph))

(defn edges [graph]
  (:edges graph))

(defn contains-vertex? [graph vertex]
  (contains? (vertices graph) vertex))

(defn contains-edge? [graph edge]
  (contains? (edges graph) edge))

(defn order [graph]
  (count (vertices graph)))

(defn edge-count [graph]
  (count (edges graph)))

(defn incident-edges [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (filter #(e/incident? % vertex) (edges graph)))

(defn adjacent-vertices [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (map #(e/other-end % vertex) (incident-edges graph vertex)))

(defn adjacent? [graph v1 v2]
  {:pre [(contains-vertex? graph v1) (contains-vertex? graph v2)]}
  (contains? (set (adjacent-vertices graph v1)) v2))

(defn degree [graph vertex]
  (count (incident-edges graph vertex)))

(defn degrees [graph]
  (map #(degree graph %) (vertices graph)))

(defn pendant? [graph vertex]
  (= 1 (degree graph vertex)))

(defn isolated? [graph vertex]
  (= 0 (degree graph vertex)))

(defn empty-graph? [graph]
  (empty? (edges graph)))

(defn empty-graph [vertices]
  (make-graph vertices []))

(defn complete-graph? [graph]
  (= (edge-count graph) (/ (* (order graph) (dec (order graph))) 2)))

(defn complete-graph [vertices]
  (make-graph vertices (map make-edge (pairs vertices))))

(defn one-edge-graph [edge]
  (make-graph (e/ends edge) [edge]))

(defn subgraph? [g1 g2]
  (and (set/subset? (vertices g1) (vertices g2))
       (set/subset? (edges g1) (edges g2))))

(defn union [g1 g2]
  (make-graph (set/union (vertices g1) (vertices g2))
              (set/union (edges g1) (edges g2))))

(defn intersection [g1 g2]
  (make-graph (set/intersection (vertices g1) (vertices g2))
              (set/intersection (edges g1) (edges g2))))

(defn difference [g1 g2]
  (make-graph (vertices g1)
              (set/difference (edges g1) (edges g2))))

(defn add-edge [graph edge]
  (make-graph (vertices graph)
              (conj (edges graph) edge)))

(defn remove-edge [graph edge]
  (make-graph (vertices graph)
              (disj (edges graph) edge)))

(defn disjoint? [g1 g2]
  (empty? (set/intersection (vertices g1) (vertices g2))))

(defn mutually-disjoint? [graphs]
  (every? (fn [[g1 g2]] (disjoint? g1 g2)) (pairs graphs)))