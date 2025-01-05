(ns jigsaw.subs
  (:require
   [re-frame.core :as re-frame]
   ["@xyflow/react" :refer [getIncomers getOutgoers]]))

(re-frame/reg-sub
 ::nodes
 (fn [db]
   (or (vec (vals (:nodes db))) [])))

(re-frame/reg-sub
 ::node
 (fn [db [_ id]]
   (get-in db [:nodes id])))

(re-frame/reg-sub
 ::edges
 (fn [db]
   (or (vec (vals (:edges db))) [])))

(defn find-db-node [clj-nodes js-node]
  (first (filter (fn [clj-node] (= (:id js-node) (:id clj-node))) clj-nodes)))

(re-frame/reg-sub
 ::incoming
 :<- [::nodes]
 :<- [::edges]
 (fn [[nodes edges] [_ node]]
   (let [js-nodes (js->clj (getIncomers (clj->js node)
                                        (clj->js nodes)
                                        (clj->js edges))
                           :keywordize-keys true)]
     (map (partial find-db-node nodes) js-nodes))))

(re-frame/reg-sub
 ::outgoing
 :<- [::nodes]
 :<- [::edges]
 (fn [[nodes edges] [_ node]]
   (let [js-nodes (js->clj (getOutgoers (clj->js node)
                                        (clj->js nodes)
                                        (clj->js edges))
                           :keywordize-keys true)]
     (map (partial find-db-node nodes) js-nodes))))

(re-frame/reg-sub
 ::active-notes
 (fn [db [_ id]]
   (or (get-in db [:nodes id :data :notes]) #{})))

(re-frame/reg-sub
 ::active-midis
 (fn [db [_ id]]
   (or (get-in db [:nodes id :data :midis]) #{})))

(re-frame/reg-sub
 ::pitch
 (fn [db [_ id]]
   (get-in db [:nodes id :data :pitch])))

(re-frame/reg-sub
 ::name
 (fn [db [_ id]]
   (get-in db [:nodes id :data :name])))
