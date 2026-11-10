(ns directed.all-paths
  (:require [directed.arc :refer [end ends]]
            [directed.graph :as g]
            [directed.route :refer [make-route]]
            [directed.weighted-graph :refer [weights]]
            [utils :refer [???]]))

(defn path-info [value step]
  {:value value
   :step  step})

(defn value [path-map i j]
  (:value (path-map [i j])))

(defn step [path-map i j]
  (:step (path-map [i j])))

(defn initial-path-map [graph]
  (into {} (???)))

(defn- print-path-map [graph path-map]
  (let [vertices (sort (g/vertices graph))]
    (doseq [i vertices]
      (doseq [j vertices]
        (print (format "%5s" (or (value path-map i j) "nil"))))
      (println)))
  (println))

(defn update-path-length [graph path-map p]
  ;(print-path-map graph path-map)
  (let [distance (fn [i j] (or (value path-map i j) ##Inf))]
    (for [i (g/vertices graph)
          j (g/vertices graph)
          :let [d (???)]
          :when (< d (???))]
      [[i j] (path-info (???) (???))])))

(defn update-path-width [graph path-map p]
  ;(print-path-map graph path-map)
  (let [min-width (fn [i j] (or (value path-map i j) ##-Inf))]
    (for [i (g/vertices graph)
          j (g/vertices graph)
          :let [m (???)]
          :when (> m (???))]
      [[i j] (path-info (???) (???))])))

(defn collect-path [path-map start end]
  (if (not (contains? path-map [start end]))
    nil
    (loop [vertices [start]]
      (let [v (peek vertices)]
        (if (and (???)
                 (???))
          (???)
          (recur (???)))))))

(defn collect-all-paths [graph path-map]
  (into {} (???)))

(defn- all-paths-floyd [graph update-path-map]
  (let [final-path-map (reduce (fn [path-map p]
                                 (into path-map (update-path-map (???) (???) (???))))
                               (???)
                               (???))]
    (collect-all-paths graph final-path-map)))

(defn all-shortest-paths-floyd [graph]
  (all-paths-floyd graph (???)))

(defn all-widest-paths-floyd [graph]
  (all-paths-floyd graph (???)))

(defn route-width [graph route]
  (reduce min (???)))
