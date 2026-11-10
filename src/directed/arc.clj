(ns directed.arc)

(defn arc [a b]
  (assert (not= a b) "Дуга должна содержать две разные вершины")
  {:start a, :end b})

(defn make-arc [[a b]]
  (arc a b))

(defn start [arc]
  (:start arc))

(defn end [arc]
  (:end arc))

(defn ends [arc]
  [(start arc) (end arc)])
