(ns electric-starter-app.main
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [fipp.edn :as fipp]
            [clojure.zip :as z]
            #?(:clj [datascript.core :as d])
            #?(:clj [datascript.transit :as dt])
            #?(:clj [clojure.java.io :as io])
            [clojure.edn :as edn]))

#?(:clj
   (def *conn-info (atom {:conn nil
                          :file nil
                          :err  nil})))

#?(:cljs
   (def *client-state (atom {:db-loaded false
                             :outliner-root-eid nil
                             :block-detail-eid nil
                             :block-detail nil})))

(defn pp-str
  [x & [opts]]
  (with-out-str (fipp/pprint x opts)))

(e/defn EdnView
  [m]
  (e/client
    (dom/pre (dom/text (pp-str m {:width 20})))))


(e/defn StateInfo
  []
  (e/client
   (let [conn-info (e/server (select-keys (e/watch *conn-info) [:file :err]))
         client-state (e/watch *client-state)]
     (dom/div (dom/props {:class "row"})
              (EdnView. (assoc conn-info :type :server))
              (EdnView. (-> client-state
                            (assoc :type :client)
                            (dissoc :block-detail)))))))

#?(:clj
   (do
     (defn- load-db-file
       [filepath]
       (try
         (let [conn (d/conn-from-db (dt/read-transit (io/input-stream (io/file filepath))))]
           (swap! *conn-info assoc :conn conn :file filepath :err nil)
           true)
         (catch Exception e
           (swap! *conn-info assoc :conn nil :file filepath :err (ex-message e))
           false)))

     (defn- sort-by-left
       [blocks parent]
       (let [left->blocks (->> (reduce (fn [acc b] (assoc! acc (:db/id (:block/left b)) b))
                                       (transient {}) blocks)
                               (persistent!))]
         (loop [block parent
                result (transient [])]
           (if-let [next (get left->blocks (:db/id block))]
             (recur next (conj! result next))
             (vec (persistent! result))))))

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
                 v (update (d/pull db '[*] (:db/id node))
                           :block/uuid str)]
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
            :db/id [x list-with-level]
            (dom/pre (dom/props {:style {:padding-left (str (* (:level x) 10) "px")}})
                     (dom/text "- " (str (get x k)))
                     (dom/on "click" (e/fn [e] (swap! *client-state assoc
                                                      :block-detail-eid (:db/id x)
                                                      :block-detail x)))))))))


(e/defn OutlinerTreeView
  [!k]
  (e/client
    (let [root-eid (:outliner-root-eid (e/watch *client-state))
          root-eid (if (string? root-eid) (edn/read-string root-eid) root-eid)
          k (or (edn/read-string (e/watch !k)) :db/id)]
      (when root-eid
        (let [tree
              (e/server
                (e/offload #(get-block-tree root-eid)))]
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

(e/defn OutlinerBrowser
  []
  (e/client
   (dom/div (dom/props {:class "column"})
            (dom/div
             (let [!filepath (atom nil) filepath (e/watch !filepath)]
               (ui/input filepath (e/fn [v] (reset! !filepath v))
                 (dom/props {:placeholder "DB filepath"})
                 (dom/on "keydown" (e/fn [e] (.stopPropagation e))))
               (ui/button (e/fn []
                            (when (seq filepath)
                              (let [succ? (e/server (e/offload #(load-db-file filepath)))]
                                (swap! *client-state assoc :db-loaded succ?))))
                          (dom/text "Load"))))
            (StateInfo.)
            (when (e/server (some? (:conn (e/watch *conn-info))))
              (let [!k (atom nil)]
                (InputSubmit. (e/fn [v] (swap! *client-state assoc :outliner-root-eid (edn/read-string v))) "eid")
                (InputSubmit. (e/fn [v] (reset! !k v)) "key to display")
                (dom/div (dom/props {:class "row"})
                 (OutlinerTreeView. !k)
                 (when-let [block-detail (:block-detail (e/watch *client-state))]
                   (EdnView. block-detail))))))))




(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (dom/on dom/node "keydown"
             (e/fn [e]
               (when (and (= "ArrowUp" (-> e .-key))
                          (:db-loaded @*client-state))
                 (when-let [root-eid (:outliner-root-eid @*client-state)]
                   (let [up-eid
                         (e/server
                          (when-let [conn (:conn @*conn-info)]
                            (:db/id (:block/parent (d/entity @conn root-eid)))))]
                     (when up-eid
                       (swap! *client-state assoc :outliner-root-eid up-eid)))))))
     (OutlinerBrowser.))))
