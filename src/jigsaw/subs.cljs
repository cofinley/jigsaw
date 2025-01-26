(ns jigsaw.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::nodes
 (fn [db]
   (:nodes db)))

(re-frame/reg-sub
 ::edges
 (fn [db]
   (:edges db)))

(re-frame/reg-sub
 ::incoming
 (fn [db [_ id]]
   (let [edges (:edges db)]
     (map #(get-in db [:node-data (:source (js->clj % :keywordize-keys true))])
          (filter #(= (:target (js->clj % :keywordize-keys true)) id) edges)))))

(re-frame/reg-sub
 ::outgoing
 (fn [db [_ id]]
   (let [edges (vals (:edges db))]
     (map #(get-in db [:node-data (:source %)])
          (filter #(= (:source %) id) edges)))))

(re-frame/reg-sub
 ::active-notes
 (fn [db [_ id]]
   (or (get-in db [:node-data id :notes]) #{})))

(re-frame/reg-sub
 ::active-midis
 (fn [db [_ id]]
   (or (get-in db [:node-data id :midis]) #{})))

(re-frame/reg-sub
 ::data
 (fn [db [_ id]]
   (get-in db [:node-data id])))
