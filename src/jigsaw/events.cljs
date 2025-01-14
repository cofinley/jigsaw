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
    :fx [[:dispatch [::calculate-shape "1"]]
         [:dispatch [::calculate-shape "3"]]]}))

(defn js-node->clj-node
  "Converts js node to clj (keywords, sets)"
  [node]
  (let [{:keys [notes name pitch pitches intervals selected-shape-type match-type view-type]} (:data node)
        type (:type node)]
    (cond-> node
      (some? notes) (assoc-in [:data :notes] (map keyword notes))
      (some? name) (assoc-in [:data :name] (keyword name))
      (some? pitch) (assoc-in [:data :pitch] (keyword pitch))
      (some? pitches) (assoc-in [:data :pitches] (map keyword pitches))
      (some? intervals) (assoc-in [:data :intervals] (map keyword intervals))
      (some? selected-shape-type) (assoc-in [:data :selected-shape-type] (keyword selected-shape-type))
      (some? match-type) (assoc-in [:data :match-type] (keyword match-type))
      (some? view-type) (assoc-in [:data :view-type] (keyword view-type))
      (some? type) (assoc :type (keyword type)))))

(re-frame/reg-event-db
 ::set-nodes
 (fn [db [_ nodes]]
   (assoc db :nodes
          (reduce (fn [m node]
                    (assoc m (:id node) (js-node->clj-node node)))
                  {}
                  nodes))))

(re-frame/reg-event-db
 ::set-edges
 (fn [db [_ edges]]
   (assoc db :edges
          (reduce (fn [m edge]
                    (assoc m (:id edge) edge))
                  {}
                  edges))))

(defn add-edge [db edge]
  (let [id (str (:source edge) "->" (:target edge))]
    (assoc-in db [:edges id] (merge {:id id} edge))))

(re-frame/reg-event-db
 ::add-edge
 (fn [db [_ edge]]
   (add-edge db edge)))

(defn create-node [db node-type & [parent-id]]
  (let [parent-node (when parent-id (get-in db [:nodes parent-id]))
        node (db/->node {:type (keyword node-type) :data {}} parent-node)
        id (:id node)]
    (cond-> db
      true (assoc-in [:nodes id] node)
      (some? parent-id) (add-edge {:source parent-id :target id}))))

(re-frame/reg-event-db
 ::add-node
 (fn [db [_ node-type & [parent-id]]]
   (create-node db node-type parent-id)))

(re-frame/reg-event-db
 ::toggle-note
 (fn [db [_ id midi]]
   (let [notes-path [:nodes id :data :notes]
         notes (set (or (get-in db notes-path) #{}))
         note (algo/midi->note midi nil)]
     (assoc-in db notes-path ((if (some? (some #{note} notes)) disj conj) notes note)))))

;; TODO: do this in output piano node (reactive), not on shape node change (stale on piano re-render)
(defn calculate-shape [node]
  (let [shape-type (if (utils/in? [:input-chord :function-scale-chords] (keyword (:type node))) :chord :scale)
        {:keys [pitch name]} (:data node)]
    (when (and (some? pitch) (some? name))
      (algo/resolve-shape (algo/pitch->note pitch) shape-type (keyword name)))))

(re-frame/reg-event-db
 ::calculate-shape
 (fn [db [_ id]]
   (let [node (get-in db [:nodes id])
         shape (calculate-shape node)]
     (cond-> db
       (some? shape) (update-in [:nodes id :data] merge (utils/strip-ns shape))))))

(re-frame/reg-event-db
 ::update-node-data
 (fn [db [_ id data]]
   (update-in db [:nodes id :data] merge data)))

(re-frame/reg-event-fx
 ::set-pitch
 (fn [cofx [_ id pitch]]
   (let [db (:db cofx)
         node (get-in db [:nodes id])
         new-node (assoc-in node [:data :pitch] pitch)]
     {:db (assoc-in db [:nodes id] new-node)
      :fx [[:dispatch [::calculate-shape id]]]})))

(re-frame/reg-event-fx
 ::set-name
 (fn [cofx [_ id name]]
   (let [db (:db cofx)
         node (get-in db [:nodes id])
         new-node (assoc-in node [:data :name] name)]
     {:db (assoc-in db [:nodes id] new-node)
      :fx [[:dispatch [::calculate-shape id]]]})))

(re-frame/reg-event-fx
 ::set-selected-chord
 (fn [cofx [_ id selected-chord]]
   (let [db (:db cofx)
         node (get-in db [:nodes id])
         new-node (assoc-in node [:data :selected-chord] selected-chord)]
     {:db (assoc-in db [:nodes id] new-node)
      :fx [[:dispatch [::calculate-shape id]]]})))

(re-frame/reg-event-db
 ::set-selected-shape
 (fn [db [_ id shape-type selected-shape]]
   (let [node (get-in db [:nodes id])
         ;; TODO: create clear-shape fn to remove any scale/chord keys, like :degrees, before setting new shape
         new-node (update (update node :data dissoc :degrees) :data merge (merge selected-shape {:selected-shape-type shape-type}))]
     (assoc-in db [:nodes id] new-node))))
