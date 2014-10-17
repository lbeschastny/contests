(ns crypto-square.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn- rotate
  "Rotates mat Matrix clockwise"
  [mat]
  (apply map list (reverse mat)))

(defn- compile-sector
  "Converts (k x k) Number matrix
   to (k x k) Boolean matrix"
  [mat ind]
  (map (partial map (partial = ind)) mat))

(defn- to-crypto-square
  "Builds (2k x 2k) Boolean matrix
   from (k x k) Number matrix"
  [mat]
  (let [[tl tr br bl] (map compile-sector
                           (iterate rotate mat)
                           (range))]
    (concat (map concat tl tr)
            (map concat bl br))))

(defn generate
  "Generates all (n x n) crypto-squares.
   Returns a Lazy Sequence."
  [^Long n]
  {:pre  [(even? n)]}
  (let [len   (/ n 2)
        comb  (- (* len len) 1)
        fmap  (if (< n 6) map pmap)]
    (fmap (comp to-crypto-square
                (partial partition len)
                (partial cons 0))
          (combo/selections (range 4) comb))))

(defn -main
  "Generates and prints all nxn crypto-squares."
  [^String n]
  (doseq [mat (generate (read-string n))]
    (doseq [row mat]
      (->>  (map #(if % "X" ".") row)
            (apply str)
            println))
    (println)))
