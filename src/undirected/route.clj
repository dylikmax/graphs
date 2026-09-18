(ns undirected.route
  (:require [utils :refer [consecutive-pairs subsequence? all-distinct?]]
            [undirected.edge :refer [make-edge]]
            [undirected.graph :as g]))

(defn make-route [vertices]
  {:vertices (vec vertices)
   :edges    (vec (map make-edge (consecutive-pairs vertices)))})

(defn route [& vertices]
  (make-route vertices))

(defn vertices [route]
  (:vertices route))

(defn edges [route]
  (:edges route))

(defn length [route]
  (count (edges route)))

(defn start [route]
  (first (vertices route)))

(defn end [route]
  (last (vertices route)))

(defn contains-vertex? [route vertex]
  (contains? (set (vertices route)) vertex))

(defn add-vertex [route vertex]
  (make-route (conj (vertices route) vertex)))

(defn subroute? [subroute route]
  (subsequence? (vertices subroute) (vertices route)))

(defn route->graph [route]
  (g/make-graph (set (vertices route)) (set (edges route))))

(defn graph-contains-route? [graph route]
  (g/subgraph? (route->graph route) graph))

(defn routes-same-graph? [r1 r2]
  (= (route->graph r1) (route->graph r2)))

(defn chain? [route]
  (all-distinct? (edges route)))

(defn- simple? [route]
  (or (all-distinct? (vertices route))
      (and (= (start route) (end route))
           (all-distinct? (rest (vertices route))))))

(defn simple-chain? [route]
  (and (chain? route) (simple? route)))

(defn cyclic? [route]
  (= (start route) (end route)))

(defn cycle? [route]
  (and (chain? route) (cyclic? route)))

(defn simple-cycle? [route]
  (and (cycle? route) (simple? route)))

(defn- extract-simple-chain-from [vertices]
  (loop [chain []
         remaining vertices]
    (if (empty? remaining)
      chain
      (let [v (first remaining)
            chain-without-v (take-while #(not= % v) chain)]
        (recur (conj chain-without-v v)
               (rest remaining))))))

(defn extract-simple-chain [route]
  {:pre  [(not (cyclic? route))]
   :post [(subroute? % route) (simple-chain? %)]}
  (make-route (extract-simple-chain-from (vertices route))))

(defn extract-simple-cycle [route]
  {:pre  [(cyclic? route)]
   :post [(subroute? % route) (simple-cycle? %)]}
  (let [vs (vertices route)]
    (make-route (cons (first vs)
                      (extract-simple-chain-from (rest vs))))))

(defn find-simple-cycle [graph]
  {:pre  [(every? (fn [d] (>= d 2)) (g/degrees graph))]
   :post [(graph-contains-route? graph %) (simple-cycle? %)]}
  (let [v0 (first (g/vertices graph))
        v1 (first (g/adjacent-vertices graph v0))]
    (loop [passed [v0]
           last v0
           current v1]
      (if (some #{current} passed)
        (make-route (concat (drop-while #(not= % current) passed) [current]))
        (recur (conj passed current)
               current
               (first (remove #{last} (g/adjacent-vertices graph current))))))))