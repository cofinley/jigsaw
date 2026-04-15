(ns jigsaw.ui.events
  (:require
   ["soundfont-player" :as soundfont]
   [clojure.string :as str]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.db :as db]
   [jigsaw.ui.midi :as midi]
   [re-frame.core :as re-frame]))

#_(re-frame/reg-event-fx
   ::initialize-db
   (fn [_ _]
     {:db db/default-db
      :fx [[:dispatch [::add-node {:id "a" :type :input-piano :position {:x 0 :y 0} :data {:notes #{:Gb4 :A4 :C5 :E5}}}]]
           [:dispatch [::add-node {:id "b" :type :input-piano :position {:x 0 :y 400} :data {:notes #{:Gb4 :A4 :B4 :Eb5}}}]]
           [:dispatch [::add-node {:id "c" :type :input-piano :position {:x 0 :y 800} :data {:notes #{:E4 :G4 :B4}}}]]
           [:dispatch [::add-node {:id "d" :type :function-cluster-shapes :position {:x 900 :y 200} :data {:view-type :output-piano}}]]
           [:dispatch [::add-edge {:id "a->d" :source "a" :target "d"}]]
           [:dispatch [::add-edge {:id "b->d" :source "b" :target "d"}]]
           [:dispatch [::add-edge {:id "c->d" :source "c" :target "d"}]]
         ;[:dispatch [::update-edge-props "c->d" {:data {:highlighted? true}}]]
           ]}))

(defn get-parent-data-for-node [db node-id]
  (let [node-type (get-in db [:node-data node-id :type])]
    (case node-type
      ; Multiple parents
      (:function-fit-shape :function-cluster-shapes)
      (let [sources (filter #(= (.-target %) node-id) (:edges db))
            source-ids (map #(.-source %) sources)]
        (map #(get-in db [:node-data %]) source-ids))
      ;; Single parent for other function nodes
      (let [sources (filter #(= (.-target %) node-id) (:edges db))]
        (when (seq sources)
          (get-in db [:node-data (.-source (first sources))]))))))

(defn stale-function-ancestors
  "Find ancestor function nodes for recomputing"
  [db]
  (let [fn-node? #(str/includes? (.-type %) "function")
        input-node? #(and % (:type %) (str/includes? (name (:type %)) "input"))
        ancestor? #(let [parent-node (get-parent-data-for-node db (.-id %))]
                     (if (seq? parent-node)
                       (every? input-node? parent-node)
                       (input-node? parent-node)))]
    (->> (:nodes db)
         (filter #(and (fn-node? %) (ancestor? %)))
         (map #(.-id %)))))

(re-frame/reg-event-fx
 ::initialize-stale-nodes
 (fn [{:keys [db]} [_]]
   {:fx (for [id (stale-function-ancestors db)]
          [:dispatch [::recompute id]])}))

(re-frame/reg-event-fx  ;; Use -fx over -db to access cofx
 ::initialize-db
 [(re-frame/inject-cofx :local-store-data)]  ;; Custom interceptor using cofx (defined in db.cljs), read from localStorage on init
 (fn [{:keys [_ local-store-data]} _]
   {:db (merge db/default-db
               (if (keys local-store-data) local-store-data {}))
    :fx [[:dispatch [::initialize-stale-nodes]]]}))

(def db->local-store [(re-frame/after (fn [db _] (db/data->local-store db)))])

;; Computation declarations
(defmulti should-compute? (fn [parent-data data] (:type data)))
(defmulti compute-node (fn [parent-data data] (:type data)))

;; Graph functions
(re-frame/reg-event-db
 ::set-nodes
 db->local-store
 (fn [db [_ nodes]]
   (assoc db :nodes nodes)))

(re-frame/reg-event-db
 ::set-edges
 db->local-store
 (fn [db [_ edges]]
   (assoc db :edges edges)))

(defn add-edge [db edge]
  (assoc db :edges (clj->js (conj (js->clj (:edges db))
                                  (clj->js (assoc edge :type :custom-edge))))))

(re-frame/reg-event-fx
 ::add-edge
 db->local-store
 (fn [{:keys [db]} [_ edge]]
   (let [new-db (add-edge db edge)
         target-id (:target edge)
         target-data (get-in new-db [:node-data target-id])
         parent-data (get-parent-data-for-node new-db target-id)]
     {:db new-db
      :fx (when (should-compute? parent-data target-data)
            [[:dispatch ^:flush-dom [::recompute target-id]]])})))

(re-frame/reg-event-db
 ::update-edge-props
 (fn [db [_ id props]]
   (let [edges (:edges db)
         edge (first (.filter edges #(= id (.-id %))))
         new-edge (clj->js (merge (js->clj edge :keywordize-keys true) props))]
     (-> db
         (assoc :edges (.map (:edges db)
                             (fn [js-edge]
                               (if (= id (.-id js-edge))
                                 new-edge
                                 js-edge))))))))

(re-frame/reg-event-db
 ::clear-edge-highlighting
 (fn [db [_]]
   (-> db
       (assoc :edges (.map (:edges db)
                           (fn [js-edge]
                             (let [clj-edge (js->clj js-edge :keywordize-keys true)
                                   data (:data clj-edge)]
                               (clj->js (assoc clj-edge :data (dissoc data :highlighted?))))))))))

(defn create-node [db _node-props & [parent-id]]
  (let [node-type (keyword (:type _node-props))
        ;; Convert screen coordinates to flow coordinates if available
        flow-position (when (and (:mouse-x _node-props) (:mouse-y _node-props) (:flow-instance _node-props))
                        (let [flow-instance (:flow-instance _node-props)
                              screen-to-flow-pos (.-screenToFlowPosition flow-instance)]
                          (when screen-to-flow-pos
                            (let [flow-pos (screen-to-flow-pos #js {:x (:mouse-x _node-props) :y (:mouse-y _node-props)})]
                              {:x (.-x flow-pos) :y (.-y flow-pos)}))))
        node-props (cond-> _node-props
                     true (assoc :type node-type)
                     flow-position (assoc :position flow-position))
        parent-node (when parent-id
                      (assoc (js->clj (first (filter #(= parent-id (.-id %)) (:nodes db))) :keywordize-keys true)
                             :data (get-in db [:node-data parent-id])))
        node (db/->node (dissoc node-props :data :mouse-x :mouse-y :flow-instance) parent-node)
        node-data (assoc (:data node-props) :type node-type)
        id (:id node)]
    [id (cond-> db
          true (assoc :nodes (clj->js (conj (js->clj (:nodes db))
                                            (clj->js node))))
          true (assoc-in [:node-data id] node-data)
          (some? parent-id) (add-edge {:id (str parent-id "->" id) :source parent-id :target id}))]))

(re-frame/reg-event-fx
 ::add-node
 db->local-store
 (fn [{:keys [db]} [_ node-props & [parent-id]]]
   (let [[node-id new-db] (create-node db node-props parent-id)
         parent-data (when (some? parent-id) (get-parent-data-for-node new-db node-id))
         node-data (get-in new-db [:node-data node-id])]
     (cond-> {:db new-db}
       (and (some? parent-data) (should-compute? parent-data node-data))
       (assoc :fx [[:dispatch ^:flush-dom [::recompute node-id]]])))))

(defn delete-node [db id]
  (-> db
      (assoc :nodes (clj->js (remove #(= id (get % "id"))
                                     (js->clj (:nodes db)))))
      (assoc :edges (.filter (:edges db)
                             #(and (not= id (.-source %))
                                   (not= id (.-target %)))))
      (update :node-data dissoc id)
      (update :function-results dissoc id)
      (update :node-loading dissoc id)))

(re-frame/reg-event-db
 ::delete-node
 db->local-store
 (fn [db [_ id]]
   (delete-node db id)))

(defn get-child-nodes [db parent-id]
  (let [edges (:edges db)
        child-edges (filter #(= (.-source %) parent-id) edges)
        child-ids (map #(.-target %) child-edges)]
    child-ids))

(re-frame/reg-event-fx
 ::update-node-data
 db->local-store
 (fn [{:keys [db]} [_ id data]]
   (let [old-data (get-in db [:node-data id])
         new-data (merge old-data data)
         new-db (assoc-in db [:node-data id] new-data)]
     {:db new-db
      :fx [[:dispatch ^:flush-dom [::recompute id]]]})))

;; Computation

(defmethod should-compute? :function-scale-chords [parent-data data]
  (and parent-data (contains? parent-data :degrees)))
(defmethod compute-node :function-scale-chords [parent-data data]
  (let [shape-refs (jigsaw/scale->chords parent-data)]
    (map #(merge % (jigsaw/->shape (assoc % :note (theory/pitch->note (:pitch %))))) shape-refs)))

(defmethod should-compute? :function-chord-scales [parent-data data]
  (and parent-data (contains? parent-data :intervals)))
(defmethod compute-node :function-chord-scales [parent-data data]
  (let [selected-degree (:selected-degree data)
        shape-refs (jigsaw/chord->scales (jigsaw/->shape parent-data) :degree selected-degree)]
    (map #(merge % (jigsaw/->shape (assoc % :note (theory/pitch->note (:pitch %))))) shape-refs)))

(defmethod should-compute? :function-find-shape [parent-data data]
  (and parent-data (seq (:notes parent-data))))
(defmethod compute-node :function-find-shape [parent-data data]
  (let [notes (:notes parent-data)
        incoming-shape-type (cond
                              (contains? parent-data :degrees) :scale
                              (contains? parent-data :intervals) :chord
                              :else :notes)
        selected-shape-type (or (:selected-shape-type data) (if (= :chord incoming-shape-type) :scale :chord))
        selected-pitch (or (:selected-pitch data) "")
        heuristic (or (:heuristic data) :overlap)
        max-shapes (or (:max-shapes data) 10)
        shapes (jigsaw/notes->shapes-memo notes
                                          selected-shape-type
                                          :max-shapes max-shapes
                                          :heuristic (keyword heuristic)
                                          :selected-pitch (if (= selected-pitch :all) nil selected-pitch))
        resolved-shapes (map #(merge % (jigsaw/->shape (theory/pitch->note (:pitch %)) (:name %))) shapes)]
    resolved-shapes))

(defmethod should-compute? :function-cluster-shapes [parent-data data]
  (and (> (count parent-data) 1)
       (every? #(contains? % :notes) parent-data)))
(defmethod compute-node :function-cluster-shapes [parent-data data]
  (let [max-results (or (:max-results data) 1)
        max-shapes (or (:max-shapes data) 3)
        note-seqs (map :notes parent-data)
        max-clusters (or (:max-clusters data) (dec (count note-seqs)) 2)]
    (jigsaw/cluster note-seqs
                    :max-results max-results
                    :max-shapes max-shapes
                    :max-clusters max-clusters)))

(defmethod should-compute? :function-fit-shape [parent-data data]
  (= (count parent-data) 2))
(defmethod compute-node :function-fit-shape [parent-data data]
  (let [target-shape (first (filter #(contains? % :name) parent-data))
        candidate-input (first (filter #(not= % target-shape) parent-data))
        max-shapes (or (:max-shapes data) 1)
        shapes (jigsaw/fit target-shape (:notes candidate-input) :max-shapes max-shapes)
        resolved-shapes (map #(merge % (jigsaw/->shape (theory/pitch->note (:pitch %)) (:name %))) shapes)]
    resolved-shapes))

(defmethod should-compute? :function-transpose [parent-data data]
  (some? parent-data))
(defmethod compute-node :function-transpose [parent-data data]
  (let [interval (keyword (or (:interval data) "P1"))
        multiplier (or (:multiplier data) 1)
        result (theory/transpose (select-keys parent-data [:pitch :pitches :intervals :degrees :name :notes]) interval multiplier)]
    result))

(defmethod should-compute? :function-chords-by-degrees [parent-data data]
  (and parent-data (theory/scale? parent-data)))
(defmethod compute-node :function-chords-by-degrees [parent-data data]
  (let [scale parent-data
        chord-degrees (map #(keyword "chord-degree" %) (str/split (or (:chord-degrees-str data) "") #"\s+"))]
    (map #(merge % (jigsaw/->shape (theory/pitch->note (:pitch %)) (:name %))) (jigsaw/->progression scale chord-degrees))))

; Recompute current node, kick off recomputation for children
(re-frame/reg-event-fx
 ::recompute
 (fn [{:keys [db]} [_ id]]
   (let [data (get-in db [:node-data id])
         parent-data (get-parent-data-for-node db id)
         #_#_opt-changed? (not-any? #(contains? data %) [:pitch :note :name])
         should-update? (and #_opt-changed?
                         (contains? (methods compute-node) (:type data))
                             (should-compute? parent-data data))
         this-node-fx (when should-update?
                        [[:dispatch ^:flush-dom [::toggle-loading id true]]
                         [:dispatch ^:flush-dom [::execute-function-computation id]]
                         [:dispatch ^:flush-dom [::toggle-loading id false]]])
         child-fx (for [child-id (get-child-nodes db id)]
                    [:dispatch [::recompute child-id]])]
     {:fx (concat this-node-fx child-fx)})))

(re-frame/reg-event-db
 ::toggle-loading
 (fn [db [_ id loading?]]
   (assoc-in db [:node-loading id] loading?)))

(re-frame/reg-event-db
 ::execute-function-computation
 (fn [db [_ id]]
   (let [data (get-in db [:node-data id])
         parent-data (get-parent-data-for-node db id)]
     (try
       (let [result (compute-node parent-data data)]
         (cond-> db
           true (assoc-in [:function-results id] result)
           ; If scalar result, use it automatically
           (not (sequential? result)) (update-in [:node-data id] merge result)))
       (catch js/Error e
         (js/console.error "Function computation error:" e)
         db)))))

;; Audio state management
(defonce audio-state (atom {:instruments {} :audio-context nil}))

(defn init-audio-context! []
  (when-not (:audio-context @audio-state)
    (let [ctx (js/AudioContext.)]
      (swap! audio-state assoc :audio-context ctx)
      ctx)))

(defn load-instrument! [instrument-name]
  (let [ctx (init-audio-context!)]
    (when-not (get-in @audio-state [:instruments instrument-name])
      (-> (soundfont/instrument ctx instrument-name)
          (.then (fn [instrument]
                   (swap! audio-state assoc-in [:instruments instrument-name] instrument)))))))

(defn play-notes [notes note-offset-ms]
  (let [instrument-name "acoustic_grand_piano"]
    (load-instrument! instrument-name)
    (js/setTimeout
     (fn []
       (when-let [instrument (get-in @audio-state [:instruments instrument-name])]
         (doseq [[i note] (map-indexed vector notes)]
           (js/setTimeout
            (fn []
              (.play instrument (theory/note->midi note)))
            (* i note-offset-ms)))))
     100) ; Small delay to ensure instrument is loaded
    {}))

(re-frame/reg-event-fx
 ::play-shape
 (fn [{:keys [_]} [_ shape]]
   (let [note-offset-ms (if (theory/chord? shape) 30 300)]
     (play-notes (:notes shape) note-offset-ms))))

(re-frame/reg-event-fx
 ::play-notes
 (fn [{:keys [_]} [_ notes]]
   (let [note-offset-ms 30]
     (play-notes notes note-offset-ms))))

;; Drag and drop functionality
(re-frame/reg-event-db
 ::create-node-from-drag
 db->local-store
 (fn [db [_ shape-data position]]
   (when (theory/shape-ref? shape-data)
     (let [node-type (cond
                       (contains? theory/chords (:name shape-data)) :input-chord
                       (contains? theory/scales (:name shape-data)) :input-scale
                       :else nil)
           shape (jigsaw/->shape (theory/pitch->note (:pitch shape-data)) (:name shape-data))]
       (if node-type
         (second (create-node db {:type node-type
                                  :position position
                                  :data (assoc shape :view-type :output-piano)}))
         db)))))

;; MIDI

(defn add-note [db note]
  (let [recording-id (:recording-id db)
        current-notes (get-in db [:node-data recording-id :notes])
        current-pcis (get-in db [:node-data recording-id :pcis])
        pci (-> note theory/parts :pci)]
    (re-frame/dispatch [::update-node-data recording-id {:notes (set (conj current-notes note))
                                                         :pcis (set (conj current-pcis pci))}])
    db
    #_(-> db (update-in db [:node-data recording-id :notes] conj note))))

(defn add-new-input-midi-node [db]
  (let [child-id (:clustering-id db)
        [id new-db] (create-node db {:type :input-piano})]
    (cond-> new-db
      true (assoc :recording-id id)
      (some? child-id) (add-edge {:id (str id "->" child-id) :source id :target child-id}))))

(defn add-new-input-and-find-shapes-node [db]
  (let [[id db'] (create-node db {:type :input-piano})
        [_ db''] (create-node db' {:type :function-find-shape} id)]
    (-> db''
        (assoc :recording-id id))))

(defn toggle-clustering [db]
  (let [clustering-id (:clustering-id db)]
    (if clustering-id
      (assoc db :clustering-id nil)
      (let [[id db'] (create-node db {:type :function-cluster-shapes})]
        (assoc db' :clustering-id id)))))

(re-frame/reg-event-db
 ::toggle-recording
 (fn [db [_ id]]
   (assoc db :recording-id (if (= id (:recording-id db))
                             nil
                             id))))

(defn update-trigger [db trigger note]
  (-> db
      (assoc :recording-id nil)
      (assoc-in [:settings :midi-triggers trigger] note)))

(re-frame/reg-event-fx
 ::on-midi-message
 db->local-store
 (fn [{:keys [db]} [_ msg]]
   (let [event (midi/parse-midi-data (.-data msg))
         note-on? (= :note-on (:command event))
         ; note-off? (= :note-off (:command event))
         ; control-change? (= :control-change (:command event))
         recording-id (:recording-id db)
         note (theory/midi->note (:note event))
         new-node-midi-trigger? (= note (-> db :settings :midi-triggers :new-node))
         toggle-clustering-midi-trigger? (= note (-> db :settings :midi-triggers :toggle-clustering))
         new-node-find-shapes-midi-trigger? (= note (-> db :settings :midi-triggers :new-node-find-shapes))
         stop-recording-midi-trigger? (= note (-> db :settings :midi-triggers :stop-recording))
         ;; TODO process db changes in separate function, maybe all of this, maybe just live-note part
         new-db (cond
                  note-on? (cond
                             stop-recording-midi-trigger? (assoc db :recording-id nil)
                             new-node-midi-trigger? (add-new-input-midi-node db)
                             new-node-find-shapes-midi-trigger? (add-new-input-and-find-shapes-node db)
                             toggle-clustering-midi-trigger? (toggle-clustering db)
                             recording-id (case recording-id
                                            :new-node (update-trigger db :new-node note)
                                            :new-node-find-shapes (update-trigger db :new-node-find-shapes note)
                                            :toggle-clustering (update-trigger db :toggle-clustering note)
                                            :stop-recording (update-trigger db :stop-recording note)
                                            (add-note db note)))
                  #_#_note-off? (when-not recording-id
                                  (-> db
                                      (update-in [:live-notes :active] disj note)
                                      (cond->
                                       currently-sustained? (update-in [:live-notes :finished] conj note))))
                  #_#_control-change? (let [sustain? (and (= 64 (:cc event)) (= 127 (:value event)))]
                                        (-> db
                                            (assoc :sustain? sustain?)
                                            (cond->
                                             sustain? (assoc-in [:live-notes :finished] (get-in db [:live-notes :active]))
                                             :else (assoc-in [:live-notes :finished] #{}))))
                  :else db)]
     {:db new-db
      #_#_:dispatch-debounce [::live-block [::on-live-block-add (:live-notes new-db)] live-block-debounce]})))

(re-frame/reg-event-db
 ::on-midi-access
 (fn [db [_ access]]
   (assoc db :midi-access access)))

(re-frame/reg-fx
 :watch-midi-input  ;; Custom effect for event handler below
 (fn [input]
   (when input
     (js/console.log "Listening to MIDI " (.-name input))
     (set! (.-onmidimessage input) #(re-frame/dispatch ^:flush-dom [::on-midi-message %])))))

(re-frame/reg-fx
 ::play-notes
 (fn [{:keys [output notes broken? individual-notes?]
       :or {broken? false}}]
   (let [sorted-notes (sort notes)]
     (if individual-notes?
       (midi/play-scale output sorted-notes)
       (midi/play-chord output sorted-notes :broken? broken?)))))

(re-frame/reg-event-fx
 ::play-notes-midi
 (fn
   ([{:keys [db]} [_ notes & {:keys [broken? individual-notes?]
                              :or {broken? (get-in db [:settings :play-chords-broken?])
                                   individual-notes? false}}]]

    (let [midis (map theory/note->midi notes)
          output-name (get-in db [:settings :midi-output])
          outputs (some->> db :midi-access .-outputs .values)
          output (some->> outputs (filter #(= (.-name %) output-name)) first)]
      {::play-notes {:output output :notes midis :broken? broken? :individual-notes? individual-notes?}}))))

;; Settings

(re-frame/reg-event-fx
 ::on-midi-select-input
 db->local-store
 (fn [{:keys [db]} [_ input-name]]
   (let [input-name (or input-name (get-in db [:settings :midi-input]))
         inputs (some->> db :midi-access .-inputs .values)
         input (some->> inputs (filter #(= (.-name %) input-name)) first)]
     {:db (assoc-in db [:settings :midi-input] input-name)
      :watch-midi-input input})))

(re-frame/reg-event-db
 ::on-midi-select-output
 db->local-store
 (fn [db [_ output-name]]
   (assoc-in db [:settings :midi-output] output-name)))

(re-frame/reg-event-db
 ::on-play-chords-broken-change
 db->local-store
 (fn [db [_ broken?]]
   (assoc-in db [:settings :play-chords-broken?] broken?)))

(re-frame/reg-event-db
 ::reset-settings
 db->local-store
 (fn [db _]
   (assoc db
          :settings {:midi-input nil
                     :midi-output nil
                     :midi-triggers {:new-node nil
                                     :stop-recording nil}})))
