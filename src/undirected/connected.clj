(ns undirected.connected
  (:require [clojure.set :as set]
            [undirected.edge :as e]
            [undirected.graph :refer :all]
            [utils :refer [???]]))

(defn connected-component [graph start]
  (loop [visited #{}
         remaining #{start}]
    (if (empty? remaining)
      (make-graph visited
                  (???))
      (let [adjacent (set (mapcat (fn [v] (adjacent-vertices graph v))
                                  remaining))
            unvisited (set/difference adjacent visited)]
        (recur (???)
               (???))))))

(defn connected-components [graph]
  {:post [(mutually-disjoint? %)]}
  (loop [components []
         remaining (vertices graph)]
    (if (empty? remaining)
      (???)
      (let [v (first remaining)
            comp (connected-component graph v)]
        (recur (???)
               (???))))))

(defn connected-vertices? [graph, v1, v2]
  (???))

(defn connected? [graph]
  (???))
