(ns audience-republic-test.core
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


(defn valid-sparseness
  [size sparseness]
  (let [minimum-edges (- size 1)
        maximum-edges (/ (* (- size 1) size) 2)]
    (println "minimum:" minimum-edges "maximum:" maximum-edges)
    (and (<= minimum-edges sparseness) (>= maximum-edges sparseness))))


(defn construct-minimal-graph
  [graph]
  (loop [current-index 1
         final-index   (count graph)
         current-graph graph]
    (let [next-index  (+ current-index 1)
          current-key (keyword (str current-index))
          next-key    (keyword (str next-index))]
      (if (= current-index final-index)
        current-graph
        (recur next-index final-index
          (update-in current-graph [current-key] assoc next-key (inc (rand-int 10))))))))


(defn generate-keys
  [size]
  (let [integer-range (range 1 (+ 1 size))
        string-range  (map str integer-range)
        keyword-range (map keyword string-range)
        hashmap-range (zipmap keyword-range (repeat {}))]
    hashmap-range))


(defn generate-edges
  "Will attempt to generate the num-edges amount of edges. Will definitely achieve the minimum,
  but may not reach the desired number at higher levels because it does not check for existing mappings."
  [graph num-vertices num-edges]
  (loop [remaining-edges (inc (- num-edges num-vertices))
         current-graph   (construct-minimal-graph graph)]
    (if (zero? remaining-edges)
      current-graph
      (let [random-source          (keyword (str (inc (rand-int 10))))
            new-destination        (keyword (str (inc (rand-int 10))))
            new-destination-weight (inc (rand-int 10))]
        (recur ; pick a random key, then add a random destination with a random length
          (dec remaining-edges)
          (update-in current-graph [(keyword (str (inc (rand-int 10))))] assoc new-destination new-destination-weight))))))


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

(defn recursive-sequence [graph explored-nodes frontier current-weight current-history]
  (if (empty? frontier)
    nil
    (let [node                                          (peek frontier)
          node-id                                       (first node)
          node-weight                                   (second node)
          node-total-weight                             (+ current-weight node-weight)
          updated-explored-nodes                        (conj explored-nodes node-id)
          node-neighbours                               (graph node-id)
          node-neighbours-without-explored              (reduce
                                                         (fn [coll val]
                                                           (if (contains? updated-explored-nodes (first val)) coll (conj coll val)))
                                                         {} node-neighbours)
          updated-frontier-nodes                        (into (pop frontier) node-neighbours-without-explored)
          updated-history                               (conj current-history node-id)]
      (print "explored nodes:" explored-nodes)
      (print "\tfrontier nodes:" (seq frontier))
      (print "\tcurrent-weight:" current-weight)
      (print "\tcurrent-history:" current-history "\n")
      (print "node-id:" node-id)
      (print "\tnode-weight:" node-weight)
      (print "\tnode-total-weight:" node-total-weight)
      (print "\tnode-neighbours:" node-neighbours "\n")
      (print "Updated explored nodes:" updated-explored-nodes "\n")
      (print "Updated frontier nodes:" (seq updated-frontier-nodes) "\n")
      (cons [node-id node-total-weight current-history]
            (recursive-sequence
             graph
             updated-explored-nodes
             updated-frontier-nodes
             node-total-weight
             updated-history)))))


(defn seq-graph [initial-collection graph start-node start-weight]
  (recursive-sequence graph [] (conj initial-collection [start-node start-weight]) 0 []))

(defn seq-graph-let-weightlift [initial-collection graph start-node]
  ((fn rec-seq [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [next-node          (peek frontier)
               neighbours         (graph next-node)
               updated-explored   (into explored neighbours)
               updated-neighbours (remove explored neighbours)
               updated-frontier   (into (pop frontier) updated-neighbours)]
           (cons next-node
                 (rec-seq
                  updated-explored
                  updated-frontier))))))
    #{start-node} (conj initial-collection start-node)))

(defn seq-graph-original [initial-collection graph start-node]
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
  [& args]
  (println "Non-weighted:")
  ; => (:1 :2 :3 :4)
  ;  (println "BFS{{" (seq-graph-bfs _graph :1) "}}")
  ; => (:1 :3 :4 :2)
  ;  (println "DFS{{" (seq-graph-dfs _graph :1) "}}")
  (println "Weighted:")
  (println (seq-graph-bfs _weighted-graph :1 0))
  (println (seq-graph-dfs _weighted-graph :1 0)))
