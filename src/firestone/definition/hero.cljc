(ns firestone.definition.hero
  (:require [firestone.definitions :as definitions]
            [firestone.core :refer [heal-entity]]
            [firestone.info :refer
             [get-minions get-heroes get-hero get-opponent-player-id
              get-default-hero-power-targets]]
            [firestone.construct :refer [create-minion add-minion-to-board]]
            [firestone.damage-entity :refer [damage-entity]]))

(def hero-definitions
  {"Anduin Wrynn" {:name "Anduin Wrynn",
                   :type :hero,
                   :class :priest,
                   :health 30,
                   :hero-power "Lesser Heal"},
   "Jaina Proudmoore" {:name "Jaina Proudmoore",
                       :type :hero,
                       :class :mage,
                       :health 30,
                       :hero-power "Fireblast"},
   "Rexxar" {:name "Rexxar",
             :type :hero,
             :class :hunter,
             :health 30,
             :hero-power "Steady Shot"},
   "Uther Lightbringer" {:name "Uther Lightbringer",
                         :type :hero,
                         :class :paladin,
                         :health 30,
                         :hero-power "Reinforce"},
   "Fireblast" {:name "Fireblast",
                :mana-cost 2,
                :type :hero-power,
                :description "Deal 1 damage.",
                :valid-targets (fn [state player-id] (get-default-hero-power-targets state player-id)),
                :hero-power-effect (fn [state {target-id :target-id}]
                                     (damage-entity state target-id 1))},
   "Lesser Heal" {:name "Lesser Heal",
                  :mana-cost 2,
                  :type :hero-power,
                  :description "Restore 2 health.",
                  :valid-targets (fn [state player-id] (get-default-hero-power-targets state player-id)),
                  :hero-power-effect (fn [state {target-id :target-id}]
                                       (heal-entity state target-id 2))},
   "Reinforce" {:name "Reinforce",
                :mana-cost 2,
                :type :hero-power,
                :description "Summon a 1/1 Silver Hand Recruit.",
                :valid-targets nil,
                :hero-power-effect (fn [state {player-id :player-id}]
                                     (add-minion-to-board
                                       state
                                       {:player-id player-id,
                                        :minion (create-minion
                                                  "Silver Hand Recruit"),
                                        :position 0}))},
   "Steady Shot"
     {:name "Steady Shot",
      :mana-cost 2,
      :type :hero-power,
      :description "Deal 2 damage to the enemy hero.",
      :valid-targets nil,
      :hero-power-effect
        (fn [state {player-id :player-id}]
          (damage-entity
            state
            (:id (get-hero state (get-opponent-player-id state player-id)))
            2))}})

(definitions/add-definitions! hero-definitions)
