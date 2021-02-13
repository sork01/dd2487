(ns firestone.damage-entity
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [clojure.pprint :refer [pprint]]
            [firestone.events :refer
             [fire-on-damage-event fire-event get-all-triggerable-entities
              do-deathrattle]]
            [firestone.info :refer
             [get-health is-secret? get-total-spell-damage get-owner get-minions
              is-alive? get-entity is-minion? is-hero?]]
            [firestone.construct :refer
             [create-game create-hero create-minion update-minion remove-minion
              remove-secret]]))

(defn destroy-entity
  "Destroy an entity (minion or secret"
  [state id]
  ; TODO add this back if needed (fire-event state {:name
  ; "destroyed-entities-event", :character-id id})
  (as-> (do-deathrattle state (get-entity state id)) $
    (cond ; TODO implement similar things for weapons
          (is-minion? $ id) (remove-minion $ id)
          (is-secret? $ id) (remove-secret $ id)
          :else (do (pprint state)
                    (throw (Exception.
                             (str "Entity id \"" id "\" does not exist.")))))))

(defn- damage-minion
  "Damages minion. "
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (damage-minion "i" 2)
                    (get-health "i"))
                0))}
  [state minion-id damage]
  (-> (update-minion state
                     minion-id
                     :health
                     (fn [previous-health] (max 0 (- previous-health damage))))
      (fire-on-damage-event minion-id damage)))

(defn- damage-hero
  "Damage the hero the specified amount."
  {:test (fn []
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (damage-hero "h1" 10)
                    (damage-hero "h1" 1)
                    (get-health "h1"))
                19))}
  [state hero-id damage]
  ; TODO trigger on damage
  (-> (update-in state
                 [:players (get-owner state hero-id) :hero :health]
                 -
                 damage)
      (fire-on-damage-event hero-id damage)))

(defn damage-entities
  "Damage multiple entities at once. Should be used if multiple entities are damaged simultaniously, otherwise undefined behaivour using damage-entity."
  [state entity-ids damages]
  {:pre [(or (list? damages) (vector? damages))]}
  (as-> (reduce
          (fn [state [id damage]]
            (-> (cond (is-minion? state id) (damage-minion state id damage)
                      (is-hero? state id) (damage-hero state id damage)
                      :else (throw (Exception. (str "Entity id \""
                                                    id
                                                    "\" does not exist."))))))
          state
          (map vector entity-ids damages)) $
    (let [dead-minions (map #(get-entity state %)
                         (filter (fn [id]
                                   (and (is-minion? $ id)
                                        (not (is-alive? (get-entity $ id)))))
                           entity-ids))]
      ; Remove all dead minions
      (as-> (reduce (fn [state minion] (remove-minion state (:id minion)))
              $
              dead-minions) $
        ; Trigger all deathrattles of the dead minions
        (reduce (fn [state minion] (do-deathrattle state minion))
          $
          (sort-by :play-order dead-minions))))))

(defn damage-entity
  "Damages an entity. If health is less than zero, remove it. Trigger on-damage and indirectly on-death through remove-minion if minion dies."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}])
                    (damage-entity "i1" 4)
                    (get-minions)
                    (count))
                0)
           ; damage a hero
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (damage-entity "h1" 4)
                    (get-health "h1"))
                26))}
  [state entity-id damage]
  (cond (is-minion? state entity-id)
          (as-> (damage-minion state entity-id damage) $
            (if (<= (get-health $ entity-id) 0) (destroy-entity $ entity-id) $))
        (is-hero? state entity-id) (damage-hero state entity-id damage)
        ; TODO maybe more types of entities are needed.
        :else (throw (Exception.
                       (str "Entity id \"" entity-id "\" does not exist.")))))

(defn damage-entity-with-spell
  "Applies damage including spell damage to an entity. "
  {:test (fn []
           (is= (-> (create-game
                      [{:minions [(create-minion "Ogre Magi")]}
                       {:minions
                          [(create-minion "War Golem" :health 7 :id "wg1")]}])
                    (damage-entity-with-spell "p1" "wg1" 3)
                    (get-health "wg1"))
                3))}
  [state spell-caster-player-id entity-id base-damage-of-spell]
  (let [spell-damage (get-total-spell-damage state spell-caster-player-id)
        total-damage (+ spell-damage base-damage-of-spell)]
    (damage-entity state entity-id total-damage)))
