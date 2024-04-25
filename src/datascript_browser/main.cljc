(ns datascript-browser.main
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]
            [fipp.edn :as fipp]
            [clojure.zip :as z]
            [clojure.set :as set]
            #?(:clj [datascript.core :as d])
            #?(:clj [datascript.transit :as dt])
            #?(:clj [clojure.java.io :as io])
            [clojure.edn :as edn]))

#?(:clj
   (def *conn-info (atom {:conn nil
                          :init-db nil
                          :tx-data-log nil
                          :tx-data-index nil
                          :tx-data-count nil
                          :!transacting (atom nil)
                          :file nil
                          :err  nil
                          :invalid-outliner-data-info []
                          })))

#?(:cljs
   (def *client-state (atom {:db-loaded false
                             :tx-data-index nil
                             :tx-data-count nil
                             :outliner-root-eid nil
                             :block-detail-eid nil})))

(defn pp-str
  [x & [opts]]
  (with-out-str (fipp/pprint x opts)))

(e/defn EdnView
  [m & {:keys [width] :or {width 20}}]
  (e/client
    (dom/pre (dom/text (pp-str m {:width width})))))


(e/defn StateInfo
  []
  (e/client
   (let [conn-info (e/server (select-keys (e/watch *conn-info)
                                          [:file :err :tx-data-index :tx-data-count :invalid-outliner-data-info]))
         client-state (e/watch *client-state)]
     (dom/div (dom/props {:class "row"})
              (EdnView. (assoc conn-info :type :server))
              (EdnView. (-> client-state
                            (assoc :type :client)))))))

#?(:clj
   (do
     (defn- load-db-file
       [filepath]
       (try
         (let [{:keys [init-db tx-log]} (dt/read-transit (io/input-stream (io/file filepath)))
               conn (d/conn-from-db init-db)]
           (swap! *conn-info assoc
                  :init-db init-db
                  :conn conn
                  :tx-data-log tx-log
                  :tx-data-index 0
                  :tx-data-count (count tx-log)
                  :file filepath :err nil)
           (select-keys @*conn-info [:tx-data-index :tx-data-count :file]))
         (catch Exception e
           (swap! *conn-info assoc :conn nil :file filepath :err (ex-message e))
           nil)))

     (defn- sort-by-left
       [blocks parent]
       (let [left->blocks (->> (reduce (fn [acc b] (assoc! acc (:db/id (:block/left b)) b))
                                       (transient {}) blocks)
                               (persistent!))
             r
             (loop [block parent
                    result (transient [])]
               (if-let [next (get left->blocks (:db/id block))]
                 (recur next (conj! result next))
                 (vec (persistent! result))))]
         (when (not= (count blocks) (count r))
           (swap! *conn-info assoc :invalid-outliner-data-info
                  {:ex-message "bad :block/left"
                   :ex-data {:index (:tx-data-index @*conn-info)
                             :diff (mapv
                                    (fn [b]
                                      {:db/id (:db/id b)
                                       :block/uuid (str (:block/uuid b))})
                                    (set/difference (set blocks) (set r)))}}))

         r))

     (defn- get-one-level-children-blocks
       [root-entity]
       (when-let [child-entities (seq (:block/_parent root-entity))]
         (sort-by-left child-entities root-entity)))

     (defn- get-block-entities-tree
       [db root-eid]
       (let [ent (d/entity db root-eid)]
         (loop [loc (z/vector-zip [ent])]
           (cond
             (z/end? loc) (z/root loc)
             (z/branch? loc) (recur (z/next loc))
             :else
             (let [node (z/node loc)
                   child-entities (get-one-level-children-blocks node)
                   loc* (if (seq child-entities)
                          (z/insert-right loc (vec child-entities))
                          loc)]
               (recur (z/next loc*)))))))

     (defn- entity-tree->map-tree
       [db tree]
       (loop [loc (z/vector-zip tree)]
         (cond
           (z/end? loc) (z/root loc)
           (z/branch? loc) (recur (z/next loc))
           :else
           (let [node (z/node loc)
                 v (some->> (:db/id node)
                            (d/pull db '[*]))
                 v (update v :block/uuid str)]
             (recur (z/next (z/replace loc v)))))))

     (defn- get-block-tree
       [root-eid]
       (when-let [conn (:conn @*conn-info)]
         (let [ent-tree (get-block-entities-tree @conn root-eid)]
           (entity-tree->map-tree @conn ent-tree))))))

(defn- tree->list-with-level
  [tree]
  (loop [loc (z/vector-zip tree) r [] level 0]
    (cond
      (nil? loc) r
      (z/end? loc) r
      (z/branch? loc)
      (if-let [loc* (z/down loc)]
        (recur loc* r (inc level))
        (recur (z/next loc) r level))
      :else
      (let [r* (conj r (assoc (z/node loc) :level level))]
        (if-let [right-loc (z/right loc)]
          (recur right-loc r* level)
          (let [[loc* level*]
                (loop [loc loc level level]
                  (when-let [loc* (z/up loc)]
                    (if-let [loc*-right (z/right loc*)]
                      [loc*-right (dec level)]
                      (recur loc* (dec level)))))]
            (recur loc* r* level*)))))))


(e/defn OutlinerTreeViewHelper
  [tree k]
  (e/client
   (let [list-with-level (tree->list-with-level tree)]
     (dom/div
      (dom/props {:id "outliner-tree"})
      (e/for-by
       hash [x list-with-level]
       (dom/pre (dom/props {:style {:padding-left (str (* (:level x) 10) "px")}})
                (dom/text "- " (str (get x k)))
                (dom/on! "click" (fn [e] (swap! *client-state assoc
                                                :block-detail-eid (:db/id x))))))))))


(e/defn OutlinerTreeView
  [!k]
  (e/client
   (let [client-state (e/watch *client-state)
         root-eid (:outliner-root-eid client-state)
         client-tx-data-index (:tx-data-index client-state)
         root-eid (if (string? root-eid) (edn/read-string root-eid) root-eid)
         k (or (edn/read-string (e/watch !k)) :db/id)]
     (when (and root-eid client-tx-data-index)
       (let [tree
             (e/server
              (let [server-tx-data-index (:tx-data-index (e/watch *conn-info))]
                (e/offload #(do (prn :server-get-block-tree server-tx-data-index)
                                (get-block-tree root-eid)))))]
         (OutlinerTreeViewHelper. tree k))))))

(e/defn InputSubmit
  [F placeholder]
  (e/client
   (dom/input (dom/props {:placeholder (str placeholder " (Enter)")
                          :style {:width "200px"}})
              (dom/on "keydown" (e/fn [e]
                                  (.stopPropagation e)
                                  (when (= "Enter" (.-key e))
                                    (when-let [v (not-empty (-> e .-target .-value))]
                                      (new F v))))))))

(e/defn TxLogProgressBar
  []
  (e/client
   (let [{:keys [tx-data-index tx-data-count]} (e/watch *client-state)
         !index-to-update (atom tx-data-index)]
     (dom/div
      (dom/props {:class "row"})
      (ui/range tx-data-index (e/fn [v] (swap! *client-state assoc :tx-data-index v))
                (dom/props {:max tx-data-count}))
      (ui/long tx-data-index (e/fn [v] (reset! !index-to-update v))
               (dom/props {:style {:height "10px" :width "50px"}})
               (dom/on "keydown" (e/fn [e]
                                   (.stopPropagation e)
                                   (when (= "Enter" (.-key e))
                                     (when-let [v (parse-long (-> e .-target .-value))]
                                       (swap! *client-state assoc :tx-data-index v))))))))))


#?(:clj
   (do
     (defn- reload-tx-data
       [init-db tx-data-coll]
       (let [conn (d/conn-from-db init-db)]
         (doseq [tx-data tx-data-coll]
           (let [tx-data* (mapv (fn [[e a v _t add?]]
                                  [(if add? :db/add :db/retract) e a v])
                                tx-data)]
             (d/transact! conn tx-data*)))
         conn))

     (defn- incremental-load-tx-data
       [conn tx-data-coll reverse?]
       (doseq [tx-data (if reverse? (reverse tx-data-coll) tx-data-coll)]
         (let [tx-data* (mapv (fn [[e a v _ add?]]
                                (let [add? (if reverse? (not add?) add?)]
                                  [(if add? :db/add :db/retract) e a v]))
                              tx-data)]
           (d/transact! conn tx-data*))))))

(e/defn LoadTxData
  []
  (e/server
    (when-let [client-tx-data-index (e/client (:tx-data-index (e/watch *client-state)))]
      (let [client-tx-data-index* (new (m/latest identity (e/throttle 500 (e/flow client-tx-data-index))))
            server-state (e/watch *conn-info)]
        (e/offload
         #(e/discard
            (when (compare-and-set! (:!transacting server-state) nil client-tx-data-index*)
              (when-let [server-tx-data-index (:tx-data-index server-state)]
                (when (and (not= client-tx-data-index* server-tx-data-index)
                           (<= client-tx-data-index* (:tx-data-count server-state)))
                  (if (> 10 (abs (- client-tx-data-index* server-tx-data-index)))
                    (let [conn (:conn @*conn-info)
                          reverse? (< client-tx-data-index* server-tx-data-index)
                          start (if reverse? client-tx-data-index* server-tx-data-index)
                          end (if reverse? server-tx-data-index client-tx-data-index*)
                          tx-data-coll (subvec (:tx-data-log @*conn-info) start end)]
                      (incremental-load-tx-data conn tx-data-coll reverse?)
                      (swap! *conn-info assoc
                             :tx-data-index client-tx-data-index*))
                    (let [init-db (:init-db @*conn-info)
                          tx-data-coll (subvec (:tx-data-log @*conn-info) 0 client-tx-data-index*)
                          new-conn (reload-tx-data init-db tx-data-coll)]
                      (swap! *conn-info assoc
                             :conn new-conn
                             :tx-data-index client-tx-data-index*)))))
              (reset! (:!transacting server-state) nil))))))))


(e/defn BlockDetailView
  []
  (e/server
   (when-let [conn (:conn (e/watch *conn-info))]
     (let [db (e/watch conn)]
       (when-let [eid (e/client (:block-detail-eid (e/watch *client-state)))]
         (let [block (some-> (d/pull db '[*] eid)
                             (update :block/uuid str))]
           (e/client
            (EdnView. block))))))))

(e/defn TXDataView
  []
  (e/server
   (let [server-state (e/watch *conn-info)
         last-tx-data-index (some-> (:tx-data-index server-state) dec)
         tx-data-log (:tx-data-log server-state)]
     (when (<= 0 last-tx-data-index)
       (let [tx-data (mapv (partial into []) (nth tx-data-log last-tx-data-index :not-found))]
         (e/client
           (EdnView. tx-data :width 100)))))))


(e/defn OutlinerBrowser
  []
  (e/client
    (dom/div (dom/props {:class "column"})
             (dom/div
               (let [!filepath (atom "tx-log-0.json") filepath (e/watch !filepath)]
                 (ui/input filepath (e/fn [v] (reset! !filepath v))
                   (dom/props {:placeholder "DB filepath"})
                   (dom/on "keydown" (e/fn [e] (.stopPropagation e))))
                 (ui/button (e/fn []
                              (when-let [{:keys [tx-data-index tx-data-count file]}
                                         (e/server (e/offload #(load-db-file filepath)))]
                                (swap! *client-state assoc
                                       :db-loaded true
                                       :tx-data-index tx-data-index
                                       :tx-data-count tx-data-count)))
                   (dom/text "Load"))
                 (dom/strong
                   (dom/props {:style {:padding "10px"}})
                   (dom/text "⇄ to load more(or less) tx-data, ↑ to view parent-tree. "))))
             (StateInfo.)
             (when (e/server (some? (:conn (e/watch *conn-info))))
               (let [!k (atom nil)]
                 (InputSubmit. (e/fn [v] (swap! *client-state assoc :outliner-root-eid (edn/read-string v))) "eid, try [:block/name \"test\"]")
                 (InputSubmit. (e/fn [v] (reset! !k v)) "key to display")
                 (TxLogProgressBar.)
                 (LoadTxData.)
                 (dom/div (dom/props {:class "row"})
                          (OutlinerTreeView. !k)
                          (BlockDetailView.)
                          (TXDataView.)))))))




(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (dom/on dom/node "keydown"
             (e/fn [e]
               (case (-> e .-key)
                 "ArrowUp"
                 (when (:db-loaded @*client-state)
                   (when-let [root-eid (:outliner-root-eid @*client-state)]
                     (let [up-eid
                           (e/server
                            (when-let [conn (:conn @*conn-info)]
                              (:db/id (:block/parent (d/entity @conn root-eid)))))]
                       (when up-eid
                         (swap! *client-state assoc :outliner-root-eid up-eid)))))

                 "ArrowLeft"
                 (when (:db-loaded @*client-state)
                   (swap! *client-state update :tx-data-index (fn [idx] (if (pos? idx) (dec idx) 0))))
                 "ArrowRight"
                 (when (:db-loaded @*client-state)
                   (swap! *client-state update :tx-data-index (fn [idx]
                                                                (if (< idx (:tx-data-count @*client-state))
                                                                  (inc idx)
                                                                  (:tx-data-count @*client-state)))))

                 (prn :press (-> e .-key))

                 )))
     (OutlinerBrowser.))))
