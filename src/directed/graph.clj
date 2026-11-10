(ns directed.graph
  (:require [clojure.set :as set]
            [directed.arc :refer [end make-arc start]]
            [utils :refer [all-distinct? ???]]))

(defn make-graph [vertices arcs]
  (assert (all-distinct? vertices) "Все вершины графа должны быть различны")
  (assert (all-distinct? arcs) "Все дуги графа должны быть различны")
  (let [vertex-set (set vertices)]
    (assert (every? (fn [a] (and (contains? vertex-set (start a))
                                 (contains? vertex-set (end a))))
                    arcs)
            "Дуги должны соединять вершины графа")
    {:vertices vertex-set
     :arcs     (set arcs)}))

(defn graph [vertices & vertex-pairs]
  (make-graph vertices
              (map make-arc vertex-pairs)))

(defn vertices [graph]
  (:vertices graph))

(defn arcs [graph]
  (:arcs graph))

(defn contains-vertex? [graph vertex]
  (???))

(defn contains-arc? [graph arc]
  (???))

(defn neighbours [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (???))

(defn subgraph? [g1 g2]
  (???))
