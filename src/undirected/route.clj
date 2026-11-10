(ns undirected.route
  (:require [utils :refer [consecutive-pairs subsequence? all-distinct? ???]]
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
  (boolean (some #{vertex} (vertices route))))

(defn add-vertex [route vertex]
  (make-route (conj (vertices route) vertex)))

(defn subroute? [subroute route]
  (and (= (start subroute) (start route))
       (= (end subroute) (end route))
       (subsequence? (vertices subroute) (vertices route))
       (every? (set (edges route)) (edges subroute))))

(defn route->graph [route]
  (g/make-graph (distinct (vertices route)) (distinct (edges route))))

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
  (and (chain? route)
       (or (all-distinct? (vertices route))
           (and (= (start route) (end route))
                (all-distinct? (rest (vertices route)))))))

(defn cyclic? [route]
  (= (start route) (end route)))

(defn cycle? [route]
  (and (chain? route) (cyclic? route)))

(defn simple-cycle? [route]
  (and (simple-chain? route) (cyclic? route)))

(defn- extract-simple-chain-from [vertices]
  (loop [chain []
         remaining vertices]
    (if (empty? remaining)
      chain
      (let [v (first remaining)
            chain-without-v (take-while #(not= % v) chain)]
        (recur (conj (vec chain-without-v) v)
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
  (let [v0 (first (sort (g/vertices graph)))
        v1 (first (sort (g/adjacent-vertices graph v0)))]
    (loop [path [v0]
           visited #{v0}
           prev v0
           current v1]
      (if (contains? visited current)
        (let [idx (.indexOf (vec path) current)]
          (make-route (concat (subvec (vec path) idx) [current])))
        (let [neighbors (remove #{prev} (g/adjacent-vertices graph current))
              unvisited (sort (remove visited neighbors))]
          (cond
            (some #{v0} neighbors)
            (recur (conj path current)
                   (conj visited current)
                   current
                   v0)
            (seq unvisited)
            (recur (conj path current)
                   (conj visited current)
                   current
                   (first unvisited))
            :else
            (recur (conj path current)
                   (conj visited current)
                   current
                   (first (sort neighbors)))))))))