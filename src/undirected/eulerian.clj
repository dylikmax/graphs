(ns undirected.eulerian
  (:require
    [clojure.set :as set]
    [undirected.connected :refer [connected-components connected?]]
    [undirected.edge :refer [edge]]
    [undirected.graph :as g]
    [undirected.route :as r :refer [make-route route]]))

(defn odd-vertices [graph]
  (map first (filter (fn [[_ d]] (odd? d)) (g/degrees graph))))

(defn eulerian? [graph]
  (and (connected? graph) (empty? (odd-vertices graph))))

(defn semi-eulerian? [graph]
  (and (connected? graph) (<= (count (odd-vertices graph)) 2)))

(defn- split-at-vertex [v vertices]
  (split-with (fn [u] (not= u v))
              vertices))

(defn rotate-cycle-to-start-from [route vertex]
  {:pre  [(r/contains-vertex? route vertex) (r/cycle? route)]
   :post [(= (r/length route) (r/length %)) (= vertex (r/start %))]}
  (if (= vertex (r/start route))
    route
    (let [[before after] (split-at-vertex vertex (r/vertices route))]
      (make-route (concat after (rest before) [(first after)])))))

(defn merge-routes-at [r1 r2 v]
  {:pre [(r/contains-vertex? r1 v) (r/contains-vertex? r2 v) (r/cycle? r2)]}
  (let [[before after] (split-at-vertex v (r/vertices r1))
        middle (r/vertices (rotate-cycle-to-start-from r2 v))]
    (make-route (concat before middle (rest after)))))

(defn merge-routes [r1 r2]
  (let [v (first (filter (fn [x] (r/contains-vertex? r1 x)) (r/vertices r2)))]
    (merge-routes-at r1 r2 v)))

(defn find-longest-route [graph start]
  {:pre [(g/contains-vertex? graph start)]}
  (loop [r (route start)
         v start
         gr graph]
    (if (g/isolated? gr v)
      r
      (let [next-v (first (g/adjacent-vertices gr v))]
        (recur (r/add-vertex r next-v)
               next-v
               (g/remove-edge gr (edge v next-v)))))))

(declare find-eulerian-cycle)

(defn- find-eulerian-path [graph start]
  {:pre [(g/contains-vertex? graph start)]}
  (if (= 1 (g/order graph))
    (make-route [start])
    (let [route (find-longest-route graph start)
          remaining-graph (g/difference graph (r/route->graph route))
          components (connected-components remaining-graph)]
      (reduce merge-routes route (map find-eulerian-cycle components)))))

(defn find-eulerian-cycle [graph]
  {:pre [(eulerian? graph)]}
  (find-eulerian-path graph (first (g/vertices graph))))

(defn find-eulerian-chain [graph]
  {:pre [(semi-eulerian? graph)]}
  (let [odd-vs (odd-vertices graph)
        start (if (empty? odd-vs)
                (first (g/vertices graph))
                (first odd-vs))]
    (find-eulerian-path graph start)))