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

(defn get-parent-id [db id]
  (let [sources (filter #(= (.-target %) id) (:edges db))]
    (when (seq sources)
      (.-source (first sources)))))

(re-frame/reg-sub
 ::parent-id
 (fn [db [_ id]]
   (get-parent-id db id)))

(re-frame/reg-sub
 ::parent-data
 (fn [db [_ id]]
   (get-in db [:node-data (get-parent-id db id)])))

(re-frame/reg-sub
 ::multi-parent-data
 (fn [db [_ id]]
   (let [sources (filter #(= (.-target %) id) (:edges db))
         source-ids (map #(.-source %) sources)]
     (map #(get-in db [:node-data %]) source-ids))))

(re-frame/reg-sub
 ::outgoing
 (fn [db [_ id]]
   (let [edges (vals (:edges db))]
     (map #(get-in db [:node-data (:source %)])
          (filter #(= (:source %) id) edges)))))

(defn get-ancestor-ids [db id]
  (loop [ancestor-ids []
         db db
         id id]
    (if (nil? id)
      ancestor-ids)
    (let [parent-id (get-parent-id db id)]
      (recur (conj ancestor-ids parent-id) db parent-id))))

(re-frame/reg-sub
 ::data
 (fn [db [_ id]]
   (get-in db [:node-data id])))
