(ns advent2022.day07
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.zip :as z]))

(def sample-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn dir [name]
  {:name name :children []})

(defn file [name size]
  {:name name :size (parse-long size)})

(defn dir? [file]
  (contains? file :children))

(defn fs-zipper [root]
  (z/zipper dir? :children (fn [n coll] (assoc n :children coll)) root))

(defn insert-dir [loc [_ name]]
  (z/append-child loc (dir name)))

(defn insert-file [loc [_ size name]]
  (z/append-child loc (file name size)))

(defn dir-loc [name loc]
  (let [file (z/node loc)]
    (when (and (dir? file) (-> file :name #{name}))
      loc)))

(defn nav-to-dir [loc [_ name]]
  (some (partial dir-loc name) (->> loc z/down (iterate z/right))))

(defn parse
  [loc input]
  (condp re-find input
    #"\$ cd /" (fs-zipper (dir "/"))
    #"\$ ls" loc
    #"dir (.+)" :>> (partial insert-dir loc)
    #"(\d+) (.+)" :>> (partial insert-file loc)
    #"cd \.\." (z/up loc)
    #"cd (.+)" :>> (partial nav-to-dir loc)))

(defn total-size [file]
  (if-let [children (:children file)]
    (assoc file :size (apply + (map :size children)))
    file))

(defn calculate-dir-sizes [input]
  (->> input
       str/split-lines
       (reduce parse nil)
       z/root
       (walk/postwalk total-size)))

(defn size-of-smallest-dir-to-free [fs]
  (let [root (first fs)
        free-space (- 70000000 (:size root))]
    (->> fs
         (filter dir?)
         (map :size)
         (reduce (fn [smallest size]
                   (if (<= 30000000 (+ free-space size))
                     (min size smallest)
                     smallest))
                 Long/MAX_VALUE))))

(->> sample-input
     calculate-dir-sizes
     (tree-seq dir? :children)
     (filter (every-pred dir? #(<= (:size %) 100000)))
     (map :size)
     (apply +))
;; => 95437

(def puzzle-input (slurp "resources/advent2022/day07.txt"))

(->> puzzle-input
     calculate-dir-sizes
     (tree-seq dir? :children)
     (filter (every-pred dir? #(<= (:size %) 100000)))
     (map :size)
     (apply +))

;; => 1792222

;;;; Part Two
(->> sample-input
     calculate-dir-sizes
     (tree-seq dir? :children)
     size-of-smallest-dir-to-free)
;; => 24933642

(->> puzzle-input
     calculate-dir-sizes
     (tree-seq dir? :children)
     size-of-smallest-dir-to-free)
;; => 1112963
  