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
  (some #{vertex} (vertices route)))

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

;; Алгоритм извлечения простой цепи:
;; Идем по списку вершин. Если встречаем вершину, которая уже есть в текущем пути,
;; мы отрезаем начало пути до первого вхождения этой вершины.
(defn- extract-simple-chain-from [vs]
  (loop [path []
         rem vs]
    (if (empty? rem)
      path
      (let [v (first rem)
            idx (first (keep-indexed (fn [i x] (when (= x v) i)) path))]
        (if idx
          ;; Если нашли повтор, обрезаем путь до этого повтора (включая его)
          (recur (subvec path 0 (inc idx)) (rest rem))
          ;; Иначе добавляем вершину в конец
          (recur (conj path v) (rest rem)))))))

(defn extract-simple-chain [route]
  {:pre  [(not (cyclic? route))]
   :post [(subroute? % route) (simple-chain? %)]}
  (make-route (extract-simple-chain-from (vertices route))))

(defn extract-simple-cycle [route]
  {:pre  [(cyclic? route)]
   :post [(subroute? % route) (simple-cycle? %)]}
  (let [vs (vec (vertices route))
        start-v (first vs)
        inner (subvec vs 1 (dec (count vs)))
        cleaned-inner (extract-simple-chain-from inner)
        idx (first (keep-indexed (fn [i x] (when (= x start-v) i)) cleaned-inner))]
    (if idx
      (make-route (concat [start-v] (subvec (vec cleaned-inner) 0 (inc idx))))
      (make-route (concat [start-v] cleaned-inner [start-v])))))

(defn find-simple-cycle [graph]
  {:pre  [(every? (fn [d] (>= d 2)) (g/degrees graph))]
   :post [(graph-contains-route? graph %) (simple-cycle? %)]}
  (let [v0 (first (g/vertices graph))
        v1 (first (g/adjacent-vertices graph v0))]
    (loop [path [v0]
           prev v0
           curr v1]
      (let [idx (first (keep-indexed (fn [i x] (when (= x curr) i)) path))]
        (if idx
          ;; Нашли цикл, возвращаем часть от первого вхождения до текущего + замыкание
          (make-route (concat (subvec (vec path) idx) [curr]))
          ;; Иначе идем дальше, выбирая соседа, который не является предыдущим
          (let [nexts (remove #{prev} (g/adjacent-vertices graph curr))
                next-v (first nexts)]
            ;; Защита от зацикливания, если neighbors пустые (хотя pre гарантирует deg >= 2)
            (if (nil? next-v)
              (throw (Exception. "No next vertex"))
              (recur (conj path curr) curr next-v))))))))