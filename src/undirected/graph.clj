(ns undirected.graph
  (:require [clojure.set :as set]
            [undirected.edge :as e :refer [edge make-edge]]
            [utils :refer [all-distinct? pairs ???]]))

(defn make-graph [vertices edges]
  (assert (all-distinct? vertices), "Все вершины графа должны быть различны")
  (assert (all-distinct? edges), "Все рёбра графа должны быть различны")
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
  (???))

(defn contains-edge? [graph edge]
  (???))

(defn order [graph]
  (???))

(defn edge-count [graph]
  (???))

(defn incident-edges [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (???))

(defn adjacent-vertices [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (???))

(defn adjacent? [graph v1 v2]
  {:pre [(contains-vertex? graph v1) (contains-vertex? graph v2)]}
  (???))

(defn degree [graph vertex]
  (???))

(defn degrees [graph]
  (???))

(defn pendant? [graph vertex]
  (???))

(defn isolated? [graph vertex]
  (???))

(defn empty-graph? [graph]
  (???))

(defn empty-graph [vertices]
  (???))

(defn complete-graph? [graph]
  (???))

(defn complete-graph [vertices]
  (???))  ; Hint: use utils/pairs

(defn one-edge-graph [edge]
  (???))

(defn subgraph? [g1 g2]
  (???))

(defn union [g1 g2]
  (???))

(defn intersection [g1 g2]
  (???))

(defn difference [g1 g2]
  (???))

(defn add-edge [graph edge]
  (???))

(defn remove-edge [graph edge]
  (???))

(defn disjoint? [g1 g2]
  (???))

(defn mutually-disjoint? [graphs]
  (???))  ; Hint: use utils/pairs
