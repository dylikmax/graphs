(ns undirected.edge
  (:require [utils :refer [all-distinct?]]
            [clojure.set :as set]))

(defn make-edge [vertex-pair]
  (let [ends (set vertex-pair)]
    (assert (= 2 (count ends)) "Ребро должно содержать две разные вершины")
    {:ends ends}))

(defn edge [a b]
  (make-edge [a b]))

(defn ends [edge]
  (:ends edge))

(defn incident? [edge vertex]
  (contains? (ends edge) vertex))

(defn edges-incident? [e1 e2]
  (boolean (seq (set/intersection (ends e1) (ends e2)))))

(defn other-end [edge vertex]
  {:pre [(incident? edge vertex)]}
  (first (disj (ends edge) vertex)))