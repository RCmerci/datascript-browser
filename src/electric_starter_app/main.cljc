(ns electric-starter-app.main
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [fipp.edn :as fipp]
            #?(:clj [clojure.zip :as z])
            #?(:clj [datascript.core :as d])
            #?(:clj [datascript.transit :as dt])
            #?(:clj [clojure.java.io :as io])
            [clojure.edn :as edn]))

#?(:clj
   (def *conn-info (atom {:conn nil
                          :file nil
                          :err  nil
                          :root-eid nil})))

(defn pp-str
  [x & [opts]]
  (with-out-str (fipp/pprint x opts)))


(e/defn ConnInfo
  []
  (e/client
    (let [conn-info (e/server (select-keys (e/watch *conn-info) [:file :err :root-eid]))]
      (dom/pre (dom/text (pp-str conn-info {:width 20}))))))

#?(:clj
   (do
     (defn- load-db-file
       [filepath]
       (try
         (let [conn (d/conn-from-db (dt/read-transit (io/input-stream (io/file filepath))))]
           (swap! *conn-info assoc :conn conn :file filepath :err nil))
         (catch Exception e
           (swap! *conn-info assoc :conn nil :file filepath :err (ex-message e)))))

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

     (defn- select-key-for-tree
       [tree k]
       (loop [loc (z/vector-zip tree)]
         (cond
           (z/end? loc) (z/root loc)
           (z/branch? loc) (recur (z/next loc))
           :else
           (let [node (z/node loc)
                 v (if (sequential? k)
                     (get-in node k)
                     (get node k))]
             (recur (z/next (z/replace loc v)))))))

     (defn- get-block-tree
       [root-eid k]
       (when-let [conn (:conn @*conn-info)]
         (let [ent-tree (get-block-entities-tree @conn root-eid)]
           (select-key-for-tree ent-tree k))))))



(e/defn DisplayOutlinerTree
  [!root-eid !k]
  (e/client
   (let [root-eid (e/watch !root-eid)
         root-eid (if (string? root-eid) (edn/read-string root-eid) root-eid)
         k (or (edn/read-string (e/watch !k)) :db/id)]
     (when root-eid
       (let [tree
             (e/server
              (e/offload #(get-block-tree root-eid k)))]
         (dom/pre (dom/text (pp-str tree {:width 10}))))))))

(e/defn InputSubmit
  [F placeholder]
  (e/client
   (dom/input (dom/props {:placeholder (str placeholder " (Enter)")
                          :style {:width "200px"}})
              (dom/on "keydown" (e/fn [e]
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
                         (dom/props {:placeholder "DB filepath"}))
               (ui/button (e/fn []
                            (when (seq filepath)
                              (e/server (e/offload #(load-db-file filepath)))))
                          (dom/text "Load"))))
            (ConnInfo.)
            (when (e/server (some? (:conn (e/watch *conn-info))))
              (let [!root-eid (atom nil)
                    !k (atom nil)]
                (InputSubmit. (e/fn [v] (reset! !root-eid v)) "eid")
                (InputSubmit. (e/fn [v] (reset! !k v)) "key to display")
                (DisplayOutlinerTree. !root-eid !k))))))


(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (OutlinerBrowser.))))
