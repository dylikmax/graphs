(ns directed.shortest-path
  (:require [clojure.set :as set]
            [directed.graph :as g :refer [neighbours]]
            [directed.route :as r :refer [make-route]]
            [directed.weighted-graph :refer [arc-length]]
            [utils :refer [???]]))

(defn- error [message]
  (throw (Exception. ^String message)))

(defn path-from-to? [graph start end route]
  (???))

(defn path-info [distance-from-start previous-vertex]
  {:dist distance-from-start, :prev previous-vertex})

(defn distance [path-map v]
  (???))

(defn previous [path-map v]
  (???))

(defn update-distances [graph path-map v neighbours]
  (let [dv (distance path-map v)]
    (for [u neighbours
          :let [d (???)]
          :when (< d (???))]
      [u (path-info d v)])))

(defn backtrack-path [start end path-map]
  (loop [vertices (list end)]
    (let [v (first vertices)]
      (if (= start v)
        (???)
        (???)))))

(defn shortest-path-dijkstra [graph start end]
  {:pre  [(g/contains-vertex? graph start) (g/contains-vertex? graph end)]
   :post [(path-from-to? graph start end %)]}
  (loop [path-map {start (path-info 0 nil)}
         unmarked-vs (g/vertices graph)]
    (if (every? (???) unmarked-vs)
      (error "There is no path from start to end")
      (let [marked-v (apply min-key (???) unmarked-vs)
            next-unmarked-vs (???)]
        (if (= (???) (???))
          (backtrack-path start end path-map)
          (let [updates (update-distances graph path-map marked-v
                                          (???))]
            (recur (into path-map updates)
                   next-unmarked-vs)))))))

(defn shortest-path-modified-dijkstra [graph start end]
  {:pre  [(g/contains-vertex? graph start) (g/contains-vertex? graph end)]
   :post [(path-from-to? graph start end %)]}
  (loop [path-map (???)
         unmarked-vs (???)]
    (if (???)
      (error "There is no path from start to end")
      (let [marked-v (???)
            updates (update-distances graph path-map marked-v
                                      (???))
            updated-vs (set (map first updates))
            next-unmarked-vs (set/union (???) updated-vs)]
        (if (and (???) (???))
          (backtrack-path start end path-map)
          (recur (???)
                 next-unmarked-vs))))))
