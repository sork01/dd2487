(ns firestone.events
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [clojure.pprint :refer [pprint]]
            [firestone.spec :as fspec]
            [clojure.spec.alpha :as spec]
            [firestone.info :refer
             [get-secrets get-entity has-battlecry?
              get-function-from-effect-description]]
            [firestone.construct :refer
             [create-game create-minion create-card create-secret get-minions
              add-minion-to-board]]))

(defn event-key [event-name] (keyword (str "on-" event-name)))

(defn- id-to-integer
  "Transforms an id to an integer. Ids should have the form [a-zA-Z]*\\d+. Other IDs are rejected."
  {:test (fn [] (is= (id-to-integer "m11") 11) (is= (id-to-integer "1") 1))}
  [id]
  {:pre [(string? id) (re-matches #"[a-zA-Z]*[0-9]+$" id)]}
  (Integer/parseInt (re-find #"[0-9]+$" id)))



(defn get-all-triggerable-entities
  "Return all entities that trigger from the event. Returns a sorted-map "
  ; TODO add weapons
  {:test
     (fn []
       (is= (as-> (create-game [{:secrets
                                   [(create-secret "Snake Trap" :id "s1")]}]) $
              (get-all-triggerable-entities $ :on-attack)
              (count $))
            1)
       (is= (as-> (create-game
                    [{:minions [(create-minion "Acolyte of Pain" :id "a100")
                                (create-minion "Acolyte of Pain" :id "x20")]}
                     {:minions [(create-minion "Acolyte of Pain" :id "m1")]}]) $
              (get-all-triggerable-entities $ :on-damage)
              (map #(:id %) $))
            '("a100" "x20" "m1"))
       ; tests for play-order
       (is= (as-> (create-game) $
              (add-minion-to-board $
                                   {:player-id "p1",
                                    :minion
                                      (create-minion "Acolyte of Pain" :id "5"),
                                    :position 0})
              (add-minion-to-board
                $
                {:player-id "p1",
                 :minion (create-minion "Frothing Berserker" :id "2"),
                 :position 0})
              (get-all-triggerable-entities $ :on-damage)
              (map #(:id %) $))
            '("5" "2"))
       (is= (as-> (create-game) $
              (add-minion-to-board
                $
                {:player-id "p1",
                 :minion (create-minion "Frothing Berserker" :id "2"),
                 :position 0})
              (add-minion-to-board $
                                   {:player-id "p1",
                                    :minion
                                      (create-minion "Acolyte of Pain" :id "5"),
                                    :position 0})
              (get-all-triggerable-entities $ :on-damage)
              (map #(:id %) $))
            '("2" "5")))}
  [state event-name]
  {:pre [(map? state)]}
  (as-> (concat (get-minions state) (get-secrets state)) $
    (filter (fn [d] (contains? (get-in d [:states :effect]) event-name)) $)
    (sort-by :play-order $)))

;(defn get-event-reaction
;  "Get the event actuation function from a minion."
;  [minion event-name]
;  (get minion (event-key event-name)))

(defn do-battlecry
  "Perform the cards battlecry if it has one, otherwise does nothing."
  ([state entity-id target]
   {:pre [(map? state) (string? entity-id)], :post [(map? %)]}
   (let [entity (get-entity state entity-id)]
     (if (has-battlecry? state entity-id)
       ((:battlecry (get-definition (:name entity)))
         state
         entity
         {:target-id target})
       state)))
  ([state entity-id] (do-battlecry entity-id nil)))


(defn do-deathrattle
  "Does the deathrattle(s) of a minion if they have them, otherwise return the state."
  [state entity]
  (if (contains? (get entity :states) :deathrattle)
    (loop [state state
           deathrattles (get-in entity [:states :deathrattle])]
      ; If all deathrattles have been completed, return the state
      (if (empty? deathrattles)
        state
        ; Deathrattles only take the state and themselves
        (-> ((get-function-from-effect-description (first deathrattles))
              state
              entity)
            (recur (rest deathrattles)))))
    ; Return state if nothing is to be changed
    state))

(defn- resolve-event-for-entity
  [state entity event]
  (loop [state state
         entity (get-entity state (:id entity))
         reactions (get-in entity [:states :effect (:name event)])]
    ; Entity might have destroyed itself
    (if (or (empty? reactions) (nil? entity))
      state
      ; Get the first effect description that the entity has
      (let [effect-description (first reactions)
            ; Get the function from that effect
            effect-function (get-function-from-effect-description
                              effect-description)
            ; Apply the function on the state
            state (effect-function state entity event)]
        ; Update the entity in case it got killed
        (recur state (get-entity state (:id entity)) (rest reactions))))))

(defn fire-event
  "Fires the event and resolves the state."
  [state event]
  ;{:pre [(spec/explain ::fspec/event event)]} ;[(spec/valid? ::fspec/event
  ;event)]}
  (loop [state state
         entities (get-all-triggerable-entities state (:name event))]
    (if (empty? entities)
      ; No one is triggered
      state
      ; At least one entity is triggered
      (let [entity (first entities)]
        (recur (resolve-event-for-entity state entity event)
               (rest entities))))))

(defn fire-end-of-turn-event
  "Triggers at end of turn"
  [state player-id]
  (fire-event state {:name :on-end-of-turn, :player-id player-id}))

; TODO test this
(defn fire-start-of-turn-event
  "Triggers at the start of a turn"
  [state player-id]
  (fire-event state {:name :on-start-of-turn, :player-id player-id}))

(defn fire-on-spell-card-played-event
  [state card player-id target-id]
  (fire-event state
              {:name :on-spell-card-played,
               :card card,
               :player-id player-id,
               :target-id target-id}))

(defn fire-on-attack-event
  "Fires the on-attack event"
  [state attacker-id target-entity]
  (fire-event
    state
    {:name :on-attack, :attacker-id attacker-id, :target target-entity}))

(defn fire-on-damage-event
  "Fires the on-damage event"
  [state minion-id damage]
  (fire-event state
              {:name :on-damage, :character-id minion-id, :amount damage}))
