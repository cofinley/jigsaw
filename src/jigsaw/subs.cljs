(ns jigsaw.subs
  (:require
   [re-frame.core :as re-frame]))

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

(re-frame/reg-sub
 ::incoming
 (fn [db [_ id]]
   (let [edges (vals (:edges db))]
     (map #(get-in db [:nodes (:source %)])
          (filter #(= (:target %) id) edges)))))

(re-frame/reg-sub
 ::outgoing
 (fn [db [_ id]]
   (let [edges (vals (:edges db))]
     (map #(get-in db [:nodes (:source %)])
          (filter #(= (:source %) id) edges)))))

(re-frame/reg-sub
 ::active-notes
 (fn [db [_ id]]
   (or (get-in db [:nodes id :data :notes]) #{})))

(re-frame/reg-sub
 ::active-midis
 (fn [db [_ id]]
   (or (get-in db [:nodes id :data :midis]) #{})))

(re-frame/reg-sub
 ::data
 (fn [db [_ id]]
   (get-in db [:nodes id :data])))

(re-frame/reg-sub
 ::pitch
 (fn [db [_ id]]
   (get-in db [:nodes id :data :pitch])))

(re-frame/reg-sub
 ::name
 (fn [db [_ id]]
   (get-in db [:nodes id :data :name])))
