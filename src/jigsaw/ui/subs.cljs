(ns jigsaw.ui.subs
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
   (assoc (get-in db [:node-data (get-parent-id db id)]) :id id)))

(re-frame/reg-sub
 ::multi-parent-data
 (fn [db [_ id]]
   (let [sources (filter #(= (.-target %) id) (:edges db))
         source-ids (map #(.-source %) sources)]
     (map #(assoc (get-in db [:node-data %]) :id %) source-ids))))

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

(re-frame/reg-sub
 ::node-loading?
 (fn [db [_ id]]
   (get-in db [:node-loading id] false)))

;; Simple subscriptions for function results
(re-frame/reg-sub
 ::function-result
 (fn [db [_ id]]
   (get-in db [:function-results id])))

;; MIDI

(re-frame/reg-sub
 ::midi-access
 :-> :midi-access)

(re-frame/reg-sub
 ::midi-input
 (fn [db]
   (-> db :settings :midi-input)))

(re-frame/reg-sub
 ::midi-output
 (fn [db]
   (-> db :settings :midi-output)))

(re-frame/reg-sub
 ::play-chords-broken?
 (fn [db]
   (-> db :settings :play-chords-broken?)))

(re-frame/reg-sub
 ::midi-triggers
 (fn [db]
   (-> db :settings :midi-triggers)))

(re-frame/reg-sub
 ::new-node-midi-trigger
 :-> :new-node-midi-trigger)

(re-frame/reg-sub
 ::stop-recording-midi-trigger
 :-> :stop-recording-midi-trigger)

(re-frame/reg-sub
 ::recording-id
 (fn [db [_]]
   (:recording-id db)))

(re-frame/reg-sub
 ::recording?
 (fn [db [_ id]]
   (= id (:recording-id db))))

(re-frame/reg-sub
 ::clustering?
 (fn [db [_ id]]
   (= id (:clustering-id db))))
