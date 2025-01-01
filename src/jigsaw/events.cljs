(ns jigsaw.events
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.db :as db]
   [jigsaw.algo :as algo]
   [jigsaw.spec :as specs]))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::set-nodes
 (fn [db [_ nodes]]
   (assoc db :nodes
          (reduce (fn [m node]
                    (assoc m (:id node) node))
                  {}
                  nodes))))

(re-frame/reg-event-db
 ::set-edges
 (fn [db [_ edges]]
   (assoc db :edges
          (reduce (fn [m edge]
                    (assoc m (str (:source edge) "->" (:target edge)) edge))
                  {}
                  edges))))

(re-frame/reg-event-db
 ::add-node
 (fn [db [_ node-type]]
   (let [node (case node-type
                "input-piano" (db/->input-piano-node)
                "input-chord" (db/->input-chord-node)
                "input-scale" (db/->input-scale-node)
                "output-piano" (db/->output-piano-node)
                "output-debug" (db/->output-debug-node))
         id (:id node)]
     (assoc-in db [:nodes id] node))))

(re-frame/reg-event-db
 ::add-edge
 (fn [db [_ edge]]
   (assoc-in db [:edges (str (:source edge) "->" (:target edge))] edge)))

(re-frame/reg-event-db
 ::toggle-midi
 (fn [db [_ id midi]]
   (let [path [:nodes id :data :midis]
         midis (set (or (get-in db path) []))]
     (assoc-in db path
               ((if (some? (some #{midi} midis)) disj conj) midis midi)))))  ;; Reinforcing set in case of conversion to vector

;; TODO: do this in output piano node (reactive), not on shape node change (stale on piano re-render)
(defn calculate-shape-notes [node]
  (let [shape-type (if (= :input-chord (keyword (:type node))) :chord :scale)
        {:keys [pitch name]} (:data node)]
    (when (and (some? pitch) (some? name))
      (::specs/notes (algo/resolve-shape (algo/pitch->note pitch) shape-type (keyword name))))))

(re-frame/reg-event-db
 ::set-pitch
 (fn [db [_ id pitch]]
   (let [node (get-in db [:nodes id])
         new-node (assoc-in node [:data :pitch] pitch)
         notes (calculate-shape-notes new-node)]
     (cond-> db
       true (assoc-in [:nodes id] new-node)
       (some? notes) (assoc-in [:nodes id :data :notes] notes)
       (some? notes) (assoc-in [:nodes id :data :midis] (map algo/note->midi notes))))))

(re-frame/reg-event-db
 ::set-name
 (fn [db [_ id name]]
   (let [node (get-in db [:nodes id])
         new-node (assoc-in node [:data :name] name)
         notes (calculate-shape-notes new-node)]
     (cond-> db
       true (assoc-in [:nodes id] new-node)
       (some? notes) (assoc-in [:nodes id :data :notes] notes)
       (some? notes) (assoc-in [:nodes id :data :midis] (map algo/note->midi notes))))))
