(ns audience-republic-test.core
  (:require [clojure.set :as set])
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
  [size sparseness]
  (let [minimum-edges (- size 1)
        maximum-edges (/ (* (- size 1) size) 2)]
    (and (<= minimum-edges sparseness) (>= maximum-edges sparseness))))


(defn- construct-minimal-graph
  [graph]
  (loop [current-index 1
         final-index   (count graph)
         current-graph graph]
    (let [next-index  (inc current-index)
          current-key (keyword (str current-index))
          next-key    (keyword (str next-index))]
      (if (= current-index final-index)
        current-graph
        (recur next-index final-index
          (update-in current-graph [current-key] assoc next-key (inc (rand-int 10))))))))


(defn- generate-keys
  [size]
  (let [integer-range (range 1 (+ 1 size))
        string-range  (map str integer-range)
        keyword-range (map keyword string-range)
        hashmap-range (zipmap keyword-range (repeat {}))]
    hashmap-range))


(defn- generate-edges
  "Will attempt to generate the num-edges amount of edges. Will definitely achieve the minimum,
  but may not reach the desired number at higher levels because it does not check for existing mappings."
  [graph num-vertices num-edges]
  (loop [remaining-edges (inc (- num-edges num-vertices))
         current-graph   (construct-minimal-graph graph)]
    (if (zero? remaining-edges)
      current-graph
      (let [random-source          (keyword (str (inc (rand-int (dec num-vertices)))))
            new-destination        (keyword (str (inc (rand-int (dec num-vertices)))))
            new-destination-weight (inc (rand-int 10))]
        (recur ; pick a random key, then add a random destination with a random length
          (dec remaining-edges)
          (update-in current-graph [random-source] assoc new-destination new-destination-weight))))))


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


(defn- dijkstra-node
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
      ; 5. IF the destination node is visited, OR if the smallest distance among the nodes in the unvisited set is infinity, then stop.
      updated-distances
      ; 6. ELSE select the unvisited node with the smallest tentative distance
      (let [distance-map (reduce (fn [coll v] (assoc coll v (get-in updated-distances [v :distance]))) {} updated-unvisited-set)
            smallest-key (reduce (fn [init v] (if (< (second v) (second init)) v init))
                                 [:A (Integer/MAX_VALUE)] distance-map)
            new-key      (first smallest-key)]
        (dijkstra-node graph source destination updated-unvisited-set updated-visited-set new-key (graph new-key) updated-distances)))))


(defn dijkstras-algorithm
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
      (println graph)
      (get-in dijkstra-map [destination :path]))))


(defn- eccentricity-node
  [graph source unvisited-set visited-set node-id neighbours distances]
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
                                           (if (and (not (nil? current-contender)) (> current-contender current-record-holder))
                                             ; update the distance
                                             (assoc coll current-key {:distance current-contender :path new-path})
                                             ; otherwise leave as is
                                             (assoc coll current-key (distances current-key)))))
                                       {} distances)
        ; 4. Then set the current node to visited, and remove it from the unvisited set
        updated-visited-set           (conj visited-set node-id)
        updated-unvisited-set         (remove #{node-id} unvisited-set)]
    (if (empty? updated-unvisited-set)
      ; 5. IF the destination node is visited, OR if the smallest distance among the nodes in the unvisited set is infinity, then stop.
      updated-distances
      ; 6. ELSE select the unvisited node with the smallest tentative distance
      (let [distance-map (reduce (fn [coll v] (assoc coll v (get-in updated-distances [v :distance]))) {} updated-unvisited-set)
            largest-key (reduce (fn [init v] (if (> (second v) (second init)) v init)) [:A 0] distance-map)
            new-key      (first largest-key)]
        (eccentricity-node graph source updated-unvisited-set updated-visited-set new-key (graph new-key) updated-distances)))))


(defn eccentricity
  [graph source]
  (let [unvisited-set (keys graph)
        visited-set []
        node-id source
        unvisited-neighbours (graph node-id)
        distances (reduce (fn [coll key] (assoc coll key {:distance 0 :path []})) {} unvisited-set)]
    (let [eccentricity-map (eccentricity-node graph source unvisited-set visited-set node-id unvisited-neighbours distances)]
      (println graph)
      eccentricity-map)))


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
