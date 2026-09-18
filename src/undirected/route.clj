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
  (???))

(defn start [route]
  (???))

(defn end [route]
  (???))

(defn contains-vertex? [route vertex]
  (???))

(defn add-vertex [route vertex]
  (???))

(defn subroute? [subroute route]
  (???))

(defn route->graph [route]
  (???))

(defn graph-contains-route? [graph route]
  (???))

(defn routes-same-graph? [r1 r2]
  (???))

(defn chain? [route]
  (???))

(defn- simple? [route]
  (or (all-distinct? (vertices route))
      (and (= (start route) (end route))
           (all-distinct? (rest (vertices route))))))

(defn simple-chain? [route]
  (???))

(defn cyclic? [route]
  (???))

(defn cycle? [route]
  (???))

(defn simple-cycle? [route]
  (???))

(defn- extract-simple-chain-from [vertices]
  (loop [chain []
         remaining vertices]
    (if (empty? remaining)
      chain
      (let [v (???)
            chain-without-v (???)]
        (recur (???)
               (???))))))

(defn extract-simple-chain [route]
  {:pre  [(not (cyclic? route))]
   :post [(subroute? % route) (simple-chain? %)]}
  (make-route (extract-simple-chain-from (???))))

(defn extract-simple-cycle [route]
  {:pre  [(cyclic? route)]
   :post [(subroute? % route) (simple-cycle? %)]}
  (let [vs (vertices route)]
    (make-route (cons (???)
                      (extract-simple-chain-from (???))))))

(defn find-simple-cycle [graph]
  {:pre  [(every? (fn [d] (>= d 2)) (g/degrees graph))]
   :post [(graph-contains-route? graph %) (simple-cycle? %)]}
  (let [v0 (first (g/vertices graph))
        v1 (first (g/adjacent-vertices graph v0))]
    (loop [passed [v0]
           last v0
           current v1]
      (if (some #{current} passed)
        (make-route (???))
        (recur (???)
               (???)
               (???))))))
