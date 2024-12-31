(ns jigsaw.subs
  (:require
   [re-frame.core :as re-frame]
   ["@xyflow/react" :refer [getIncomers getOutgoers]]))

(re-frame/reg-sub
 ::nodes
 (fn [db]
   (or (vec (vals (:nodes db))) [])))

(re-frame/reg-sub
 ::edges
 (fn [db]
   (or (vec (vals (:edges db))) [])))

(re-frame/reg-sub
 ::incoming
 :<- [::nodes]
 :<- [::edges]
 (fn [[nodes edges] [_ node]]
   (js->clj (getIncomers (clj->js node)
                         (clj->js nodes)
                         (clj->js edges))
            :keywordize-keys true)))

(re-frame/reg-sub
 ::outgoing
 :<- [::nodes]
 :<- [::edges]
 (fn [[nodes edges] [_ node]]
   (js->clj (getOutgoers (clj->js node)
                         (clj->js nodes)
                         (clj->js edges))
            :keywordize-keys true)))

(re-frame/reg-sub
 ::active-midis
 (fn [db [_ id]]
   (or (get-in db [:nodes id :data :midis]) #{})))
