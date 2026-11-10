(ns undirected.tree
  (:require [undirected.connected :refer [connected-components connected?]]
            [undirected.graph :as g]))

(defn edge-count-tree? [graph]
  (= (g/edge-count graph) (- (g/order graph) 1)))

(defn tree? [graph]
  (and (connected? graph)
       (= (count (g/edges graph))
          (dec (count (g/vertices graph))))))

(defn forest? [graph]
  (= (count (g/edges graph))
     (- (count (g/vertices graph))
        (count (connected-components graph)))))