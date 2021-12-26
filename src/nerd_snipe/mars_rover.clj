(ns nerd-snipe.mars-rover
  (:require [clojure.set :as set]))

(def compass-points "NESW")

(def direction->coords {\N [0 1]
                        \E [1 0]
                        \S [0 -1]
                        \W [-1 0]})

;; Movements

(def rotate-r (zipmap compass-points (rest (cycle compass-points))))

(def rotate-l (set/map-invert rotate-r))

(defn forward [coords direction]
  (mapv + coords (direction->coords direction)))

(defn wrap [coords]
  (mapv #(mod % 10) coords))

;; Process move command

(defn blocked? [{:keys [coords obstacles]}]
  ((set obstacles) coords))

(defn next-move-cmd [{:keys [direction] :as rover} move-cmd]
  (let [next-rover (case move-cmd
                     \M (update rover :coords (comp wrap forward) direction)
                     \L (update rover :direction rotate-l move-cmd)
                     \R (update rover :direction rotate-r move-cmd))]
    (if (blocked? next-rover)
      (reduced (assoc rover :blocked? true))
      next-rover)))

;; Final output

(defn format-rover [{[x y] :coords direction :direction blocked? :blocked?}]
  (str (when blocked? "0:") x ":" y ":" direction))

;; Go!

(defn run
  ([input] (run input nil))
  ([input obstacles]
   (format-rover (reduce next-move-cmd {:coords [0 0] :direction \N :obstacles obstacles} input))))

(defn -main [& _]
  (doseq [input [["MMRMMLM"]          ; 2:3:N
                 ["MMMMMMMMMM"]       ; 0:0:N
                 ["MMMM" #{[0 3]}]]]  ; 0:0:2:N
    (println input "->" (apply run input))))
