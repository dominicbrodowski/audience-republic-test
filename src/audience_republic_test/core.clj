(ns audience-republic-test.core
  (:require [clojure.set :as set]
            [clojure.test :refer :all])
  (:gen-class))


(def _graph
  {:1 [:2 :3],
   :2 [:4],
   :3 [:4],
   :4 []})


(def _weighted-graph
  {:1 {:2 1 :3 2},
   :2 {:4 4},
   :3 {:4 2},
   :4 {}})


(defn- valid-sparseness
  "Boolean test to check that the sparseness setting for generating a graph is correct. Expecting a value between N-1 and N(N-1)/2"
  [size sparseness]
  (let [minimum-edges (- size 1)
        maximum-edges (/ (* (- size 1) size) 2)]
    (and (<= minimum-edges sparseness) (>= maximum-edges sparseness))))


(defn- int-to-key
  [_int]
  (keyword (str _int)))


(defn- key-to-int
  [_key]
  (Integer/parseInt (name _key)))


(defn- construct-minimal-graph
  "Constructs the bare minimum of what is expected of a connected graph. Will connect each node to the other sequentially."
  [num-vertices]
  (let [integer-range (range 1 num-vertices)
        minimal-graph (reduce
                       (fn [coll key] (assoc coll (int-to-key key) {(int-to-key (inc key)) (inc (rand-int 10))})) {} integer-range)]
    (assoc minimal-graph (int-to-key num-vertices) {})))


(defn- generate-keys
  [size]
  (let [integer-range (range 1 (+ 1 size))
        string-range  (map str integer-range)
        keyword-range (map keyword string-range)
        hashmap-range (zipmap keyword-range (repeat {}))]
    hashmap-range))


(defn- generate-all-edge-possibilities
  [keys]
  (let [integer-range (range 1 (inc (count keys)))
        string-range  (map str integer-range)
        keyword-range (map keyword string-range)]
    (remove #(= (first %) (second %)) (apply concat (reduce (fn [coll key] (conj coll (map list (repeat key) keyword-range))) [] keys)))))


(defn generate-edges
  "Will attempt to generate the num-edges amount of edges, including the minimal set."
  [graph num-vertices num-edges]
  (let [current-graph   (construct-minimal-graph num-vertices)
        remaining-edges (inc (- num-edges num-vertices))
        edge-possibilities (generate-all-edge-possibilities (keys graph))
        edge-possibilities-without-minimal-graph (remove (fn [x] (let [key-int (key-to-int (first x))
                                                                       val-int (key-to-int (second x))]
                                                                   (= key-int (dec val-int)))) edge-possibilities)
        shuffled-edges (shuffle edge-possibilities-without-minimal-graph)
        num-intended-edges (take remaining-edges shuffled-edges)]
    (reduce (fn [coll x]
              (let [_key (first x)
                    _val (second x)]
                (update-in coll [_key] assoc _val (inc (rand-int 10))))) current-graph num-intended-edges)))


(defn generate-graph
  "Generates graph of size N and sparseness S, where S is the number of directed edges, from N-1 to N(N-1)/2.
  Graph must be connected."
  [N S]
  (if
    (valid-sparseness N S)
    (let [graph            {}
          graph-with-keys  (generate-keys N)]
      (generate-edges graph-with-keys N S))
    nil))


(deftest generate-graph-test
  (let [graph (generate-graph 5 4)]
    (is (= 4 (count (keys graph))))
    (is (= 4 (reduce + (map count (vals graph))))))
  (let [graph (generate-graph 5 10)]
    (is (<= 4 (count (keys graph))))
    (is (= 10 (reduce + (map count (vals graph)))))))


(defn- dijkstra-node
  "Recursive function that calculates shortest path via Dijkstra's algorithm."
  [graph source destination unvisited-set visited-set node-id neighbours distances]
  (let [node-distance                 (get-in distances [node-id :distance])
        node-path                     (get-in distances [node-id :path])
        new-path                      (conj node-path node-id)
        ; 2. Check ALL neighbours and calculate their distances via this node
        unvisited-neighbours          (remove (fn [neighbour] (contains? (set visited-set) (first neighbour))) neighbours)
        neighbour-tentative-distances (reduce (fn [coll v] (assoc coll (first v) (+ node-distance (second v)))) {} unvisited-neighbours)
        ; 3. For each neighbour, update the distance and path if it is shorter than the current path
        updated-distances             (reduce
                                       (fn [coll v]
                                         (let [current-key           (first v)
                                               current-record-holder (get-in distances [current-key :distance])
                                               current-contender     (neighbour-tentative-distances current-key)]
                                           (if (and (not (nil? current-contender)) (< current-contender current-record-holder))
                                             ; update the distance
                                             (assoc coll current-key {:distance current-contender :path new-path})
                                             ; otherwise leave as is
                                             (assoc coll current-key (distances current-key)))))
                                       {} distances)
        ; 4. Then set the current node to visited, and remove it from the unvisited set
        updated-visited-set           (conj visited-set node-id)
        updated-unvisited-set         (remove #{node-id} unvisited-set)]
    (if (or (contains? updated-visited-set destination) (empty? updated-unvisited-set))
      ; 5. IF the destination node is visited, then stop.
      updated-distances
      ; 6. ELSE select the unvisited node with the smallest tentative distance
      (let [distance-map (reduce (fn [coll v] (assoc coll v (get-in updated-distances [v :distance]))) {} updated-unvisited-set)
            smallest-key (reduce (fn [init v] (if (< (second v) (second init)) v init)) distance-map)
            new-key      (first smallest-key)]
        (dijkstra-node graph source destination updated-unvisited-set updated-visited-set new-key (graph new-key) updated-distances)))))


(defn dijkstras-algorithm
  "Shortest path from source to destination in the graph provided. Returns the nodes it passes through."
  [graph source destination]
  (let [unvisited-set        (keys graph)
        visited-set          []
        node-id              source
        unvisited-neighbours (graph node-id)
        raw-distances        (reduce
                              (fn [coll key] (assoc coll key {:distance (Integer/MAX_VALUE) :path []})) {} unvisited-set)
        distances            (update-in raw-distances [node-id] assoc :distance 0 :path [])]
    ; set all distances to infinity, except for the source which is zero
    (let [dijkstra-map     (dijkstra-node graph source destination unvisited-set visited-set node-id unvisited-neighbours distances)]
      (get-in dijkstra-map [destination :path]))))


(defn- eccentricity-node
  "Reverse of Dijkstra's algorithm."
  [graph source unvisited-set visited-set node-id neighbours distances]
  (let [node-distance                 (get-in distances [node-id :distance])
        node-path                     (get-in distances [node-id :path])
        new-path                      (conj node-path node-id)
        ; 2. Check ALL neighbours and calculate their distances via this node
        unvisited-neighbours          (remove (fn [neighbour] (contains? (set visited-set) (first neighbour))) neighbours)
        neighbour-tentative-distances (reduce (fn [coll v] (assoc coll (first v) (+ node-distance (second v)))) {} unvisited-neighbours)
        ; 3. For each neighbour, update the distance and path if it is larger than the current path
        updated-distances             (reduce
                                       (fn [coll v]
                                         (let [current-key           (first v)
                                               current-record-holder (get-in distances [current-key :distance])
                                               current-contender     (neighbour-tentative-distances current-key)]
                                           (if (and (not (nil? current-contender)) (> current-contender current-record-holder))
                                             ; update the distance
                                             (assoc coll current-key {:distance current-contender :path new-path})
                                             ; otherwise leave as is
                                             (assoc coll current-key (distances current-key)))))
                                       {} distances)
        ; 4. Then set the current node to visited, and remove it from the unvisited set
        updated-visited-set           (conj visited-set node-id)
        updated-unvisited-set         (remove #{node-id} unvisited-set)]
    ; 5. IF the destination node is visited, then stop.
    (if (or (empty? updated-unvisited-set) (empty? (set/union updated-unvisited-set (first unvisited-neighbours))))
      updated-distances
      ; 6. ELSE select the unvisited node with the smallest tentative distance
      (let [distance-map (reduce (fn [coll v] (assoc coll v (get-in updated-distances [v :distance]))) {} updated-unvisited-set)
            largest-key (reduce (fn [init v] (if (> (second v) (second init)) v init)) distance-map)
            new-key      (first largest-key)]
        (eccentricity-node graph source updated-unvisited-set updated-visited-set new-key (graph new-key) updated-distances)))))


(defn eccentricity
  "Longest path from source to any other vertex in the graph provided."
  [graph source]
  (let [unvisited-set (keys graph)
        visited-set []
        node-id source
        unvisited-neighbours (graph node-id)
        distances (reduce (fn [coll key] (assoc coll key {:distance 0 :path []})) {} unvisited-set)]
    (let [eccentricity-map (eccentricity-node graph source unvisited-set visited-set node-id unvisited-neighbours distances)
          distances (map #(:distance (second %)) eccentricity-map)]
      (apply max distances))))


(defn radius
  "The smallest eccentricity found in the graph provided."
  [graph]
  (apply min (map #(eccentricity graph %) (keys graph))))


(defn diameter
  "The largest eccentricity found in the graph provided."
  [graph]
  (apply max (map #(eccentricity graph %) (keys graph))))


(defn seq-graph [initial-collection graph start-node]
  ((fn rec-seq [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v         (peek frontier)
               neighbors (graph v)]
           (cons v
                 (rec-seq
                  (into explored neighbors)
                  (into (pop frontier) (remove explored neighbors))))))))
    #{start-node} (conj initial-collection start-node)))


(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args])
