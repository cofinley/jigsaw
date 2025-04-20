(ns jigsaw.events
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.db :as db]
   [jigsaw.algo :as algo]
   [jigsaw.utils :as utils]))

(re-frame/reg-event-fx
 ::initialize-db
 (fn [_ _]
   {:db db/default-db
    :fx [[:dispatch [::add-node {:id "a" :type :input-piano :position {:x 0 :y 0} :data {:notes #{:Gb4 :A4 :C5 :E5}}}]]
         [:dispatch [::add-node {:id "b" :type :input-piano :position {:x 0 :y 400} :data {:notes #{:Gb4 :A4 :B4 :Eb5}}}]]
         [:dispatch [::add-node {:id "c" :type :input-piano :position {:x 0 :y 800} :data {:notes #{:E4 :G4 :B4}}}]]
         [:dispatch [::add-node {:id "d" :type :function-connect-shapes :position {:x 900 :y 200} :data {:view-type :output-piano}}]]
         [:dispatch [::add-edge {:source "a" :target "d"}]]
         [:dispatch [::add-edge {:source "b" :target "d"}]]
         [:dispatch [::add-edge {:source "c" :target "d"}]]]}))

(re-frame/reg-event-db
 ::set-nodes
 (fn [db [_ nodes]]
   (assoc db :nodes nodes)))

(re-frame/reg-event-db
 ::set-edges
 (fn [db [_ edges]]
   (assoc db :edges edges)))

(defn add-edge [db edge]
  (assoc db :edges (.concat (:edges db) (clj->js edge))))

(re-frame/reg-event-db
 ::add-edge
 (fn [db [_ edge]]
   (add-edge db edge)))

(defn create-node [db _node-props & [parent-id]]
  (let [node-type (keyword (:type _node-props))
        node-props (merge _node-props {:type node-type})
        parent-node (when parent-id
                      (assoc (js->clj (first (filter #(= parent-id (.-id %)) (:nodes db))) :keywordize-keys true)
                             :data (get-in db [:node-data parent-id])))
        node (db/->node (dissoc node-props :data) parent-node)
        node-data (assoc (:data node-props) :type node-type)
        id (:id node)]
    (cond-> db
      true (assoc :nodes (.concat (:nodes db) (clj->js node)))
      true (assoc-in [:node-data id] node-data)
      (some? parent-id) (add-edge #js {:source parent-id :target id}))))

(re-frame/reg-event-db
 ::add-node
 (fn [db [_ node-props & [parent-id]]]
   (create-node db node-props parent-id)))

(defn delete-node [db id]
  (-> db
      (assoc :nodes (.filter (:nodes db) #(not= id (.-id %))))
      (update :node-data dissoc id)))

(re-frame/reg-event-db
 ::delete-node
 (fn [db [_ id]]
   (delete-node db id)))

(re-frame/reg-event-db
 ::toggle-note
 (fn [db [_ id midi]]
   (let [notes-path [:node-data id :notes]
         notes (set (or (get-in db notes-path) #{}))
         note (algo/midi->note midi nil)]
     (assoc-in db notes-path ((if (some? (some #{note} notes)) disj conj) notes note)))))

;; TODO: do this in output piano node (reactive), not on shape node change (stale on piano re-render)
(defn calculate-shape [node]
  (let [shape-type (if (utils/in? [:input-chord :function-scale-chords] (:type node)) :chord :scale)
        {:keys [pitch name]} node]
    (when (and (some? pitch) (some? name))
      (algo/resolve-shape (algo/pitch->note pitch) shape-type (keyword name)))))

(re-frame/reg-event-db
 ::calculate-shape
 (fn [db [_ id]]
   (let [node (get-in db [:node-data id])
         shape (calculate-shape node)]
     (cond-> db
       (some? shape) (update-in [:node-data id] merge shape)))))

(re-frame/reg-event-db
 ::update-node-data
 (fn [db [_ id data]]
   (update-in db [:node-data id] merge data)))

(re-frame/reg-event-db
 ::set-selected-shape
 (fn [db [_ id shape-type selected-shape]]
   (let [node (get-in db [:node-data id])
         ;; TODO: create clear-shape fn to remove any scale/chord keys, like :degrees, before setting new shape
         new-node (merge (update node :data dissoc :degrees) (merge selected-shape {:selected-shape-type shape-type}))]
     (assoc-in db [:node-data id] new-node))))
