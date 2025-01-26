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
    ; :fx [[:dispatch [::calculate-shape "1"]]
    ;      [:dispatch [::calculate-shape "3"]]]
    }))

(defn js-node->clj-node
  "Converts js node to clj (keywords, sets)"
  [node]
  (let [{:keys [notes name pitch pitches intervals selected-shape-type match-type view-type degree selected-pitch selected-degree]} (:data node)
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
      (some? degree) (assoc-in [:data :degree] (keyword degree))
      (some? selected-pitch) (assoc-in [:data :selected-pitch] (keyword selected-pitch))
      (some? selected-degree) (assoc-in [:data :selected-degree] (keyword selected-degree))
      (some? type) (assoc :type (keyword type)))))

(re-frame/reg-event-db
 ::set-nodes
 (fn [db [_ nodes]]
   (assoc db :nodes nodes)))

(re-frame/reg-event-db
 ::set-edges
 (fn [db [_ edges]]
   (assoc db :nodes edges)))

(defn add-edge [db edge]
  (-> db
      (assoc :edges (.concat (:edges db) edge))))

(re-frame/reg-event-db
 ::add-edge
 (fn [db [_ edge]]
   (add-edge db edge)))

(defn create-node [db node-type & [parent-id]]
  (let [;parent-node (when parent-id (get-in db [:nodes parent-id]))
        parent-node nil
        node (db/->node {:type (keyword node-type) :data {}} parent-node)
        id (:id node)]
    (cond-> db
      true (assoc :nodes (.concat (:nodes db)
                                  (clj->js {:id id
                                            :position {:x 0 :y 0}
                                            :type node-type
                                            :data {}})))
      true (assoc-in [:node-data id] {:type node-type})
      (some? parent-id) (add-edge {:source parent-id :target id}))))

(re-frame/reg-event-db
 ::add-node
 (fn [db [_ node-type & [parent-id]]]
   (create-node db node-type parent-id)))

(re-frame/reg-event-db
 ::toggle-note
 (fn [db [_ id midi]]
   (let [notes-path [:node-data id :notes]
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
   (let [node (get-in db [:node-data id])
         shape (calculate-shape node)]
     (cond-> db
       (some? shape) (update-in [:node-data id] merge (utils/strip-ns shape))))))

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
