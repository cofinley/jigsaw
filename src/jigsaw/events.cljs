(ns jigsaw.events
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.db :as db]))

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
                "output-piano" (db/->output-piano-node))
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
