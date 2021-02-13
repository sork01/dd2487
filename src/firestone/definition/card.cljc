(ns firestone.definition.card
  (:require
    [ysera.random :refer [random-nth get-random-int]]
    [firestone.definitions :as definitions]
    [clojure.pprint :refer [pprint]]
    [firestone.damage-entity :refer [destroy-entity damage-entity-with-spell]]
    [firestone.info :refer
     [is-minion? get-hero get-heroes get-minions get-opponent-player-id
      get-attack is-injured? get-owner get-entity get-secrets get-seed
      get-hand]]
    [firestone.mana :refer [increase-max-mana increase-mana]]
    [firestone.construct :refer
     [add-minion-to-board create-minion add-card-to-hand create-card
      add-secret-to-board create-secret remove-minion]]
    [firestone.core :refer
     [change-stats-minion buff-attack buff-health heal-entity switch-side-minion
      give-effect remove-one-buff remove-one-effect freeze-entity update-seed
      remove-secrets map-minions remove-stealth add-buff]]
    [firestone.api :refer [draw-card play-minion-card remove-card-from-hand]]))

(def card-definitions
  {"Dalaran Mage" {:name "Dalaran Mage",
                   :mana-cost 3,
                   :health 4,
                   :attack 1,
                   :type :minion,
                   :set :basic,
                   :rarity :none,
                   :auras [{:spell-damage 1}],
                   :description "Spell Damage +1"},
   "Defender" {:name "Defender",
               :attack 2,
               :health 1,
               :mana-cost 1,
               :set :classic,
               :class :paladin,
               :type :minion,
               :rarity :common},
   "Imp" {:name "Imp",
          :attack 1,
          :health 1,
          :mana-cost 1,
          :rarity :common,
          :set :classic,
          :type :minion,
          :race :demon},
   "Silver Hand Recruit" {:name "Silver Hand Recruit",
                          :attack 1,
                          :health 1,
                          :mana-cost 1,
                          :class :paladin,
                          :type :minion,
                          :set :basic,
                          :rarity :none},
   "Ogre Magi" {:name "Ogre Magi",
                :attack 4,
                :health 4,
                :mana-cost 4,
                :type :minion,
                :auras [{:spell-damage 1}],
                :set :basic,
                :description "Spell Damage +1"},
   "War Golem" {:name "War Golem",
                :attack 7,
                :health 7,
                :mana-cost 7,
                :type :minion,
                :set :basic,
                :rarity :none},
   "Big Game Hunter"
     {:name "Big Game Hunter",
      :attack 4,
      :health 2,
      :mana-cost 5,
      :type :minion,
      :set :classic,
      :valid-targets-when-card-played
        (fn [state player-id]
          (let [targets (filter #(>= (get-attack state (:id %)) 7)
                          (get-minions state))]
            (if (empty? targets) nil (map #(:id %) targets)))),
      :rarity :epic,
      :description "Battlecry: Destroy a minion with an Attack of 7 or more.",
      :battlecry
        (fn [state this {target-id :target-id}]
          ; If it has a target destroy it
          (if (not (nil? target-id)) (destroy-entity state target-id) state))},
   "Eater of Secrets"
     {:name "Eater of Secrets",
      :attack 2,
      :health 4,
      :mana-cost 4,
      :type :minion,
      :set :whispers-of-the-old-gods,
      :rarity :rare,
      :description "Battlecry: Destroy all enemy Secrets. Gain +1/+1 for each.",
      :battlecry
        (fn [state minion {}]
          (let [opponent (get-opponent-player-id state (get-owner minion))]
            (loop [state state
                   secrets (get-secrets state opponent)]
              (if (empty? secrets)
                state
                (recur (-> (destroy-entity state (:id (first secrets)))
                           (change-stats-minion (:id minion) "Secret food" 1 1))
                       (rest secrets))))))},
   "Arcane Golem" {:name "Arcane Golem",
                   :attack 4,
                   :health 4,
                   :mana-cost 3,
                   :type :minion,
                   :set :classic,
                   :rarity :rare,
                   :description "Battlecry: Give your opponent a Mana Crystal.",
                   :battlecry (fn [state minion {}]
                                ; Increase mana for opponent
                                (let [opponent (get-opponent-player-id
                                                 state
                                                 (get-owner state
                                                            (:id minion)))]
                                  (increase-max-mana state opponent)))},
   "Acolyte of Pain"
     {:name "Acolyte of Pain",
      :attack 1,
      :health 3,
      :mana-cost 3,
      :type :minion,
      :set :classic,
      :rarity :common,
      :description "Whenever this minion takes damage, draw a card.",
      :states {:effect {:on-damage [{:effect-source "Acolyte of Pain",
                                     :effect-key :on-damage}]}},
      :on-damage (fn [state this-minion
                      {damaged-minion-id :character-id, damage :amount}]
                   (if (not= (get this-minion :id) damaged-minion-id)
                     state
                     (draw-card state (get this-minion :owner-id))))},
   "Snake" {:name "Snake",
            :attack 1,
            :health 1,
            :mana-cost 1,
            :type :minion,
            :rarity :rare,
            :set :classic},
   "Ancient Watcher" {:name "Ancient Watcher",
                      :attack 4,
                      :health 5,
                      :mana-cost 2,
                      :type :minion,
                      :set :classic,
                      :auras [],
                      :states {:can-attack? [{:effect-source "Ancient Watcher",
                                              :effect-key :can-attack}]},
                      :rarity :rare,
                      :can-attack (fn [state {}] false),
                      :description "Can't attack."},
   "Sneed's Old Shredder"
     {:name "Sneed's Old Shredder",
      :attack 5,
      :health 7,
      :mana-cost 8,
      :type :minion,
      :set :goblins-vs-gnomes,
      :rarity :legendary,
      :description "Deathrattle: Summon a random Legendary minion.",
      :states {:deathrattle [{:effect-source "Sneed's Old Shredder",
                              :effect-key :on-own-death}]},
      :on-own-death
        (fn [state this]
          (let [seed (get-seed state)
                [new-seed minion-name]
                  (random-nth seed
                              (map :name
                                (filter (fn [x]
                                          (and (= (:type x) :minion)
                                               (= (:rarity x) :legendary)))
                                  (vals card-definitions))))]
            (-> state
                (add-minion-to-board {:player-id (get-owner this),
                                      :minion (create-minion minion-name),
                                      :position 0})
                (update-seed new-seed))))},
   "King Mukla"
     {:name "King Mukla",
      :attack 5,
      :health 5,
      :mana-cost 3,
      :type :minion,
      :set :classic,
      :rarity :legendary,
      :description "Battlecry: Give your opponent 2 Bananas.",
      :battlecry
        (fn [state minion {}]
          (let [opponent
                  (get-opponent-player-id state (get-owner state (:id minion)))]
            (-> (add-card-to-hand state opponent (create-card "Bananas"))
                (add-card-to-hand opponent (create-card "Bananas")))))},
   "Frostbolt" {:name "Frostbolt",
                :mana-cost 2,
                :type :spell,
                :set :basic,
                :spell-effect
                  (fn [state {target-id :target-id, player-id :player-id}]
                    (-> (freeze-entity state target-id)
                        (damage-entity-with-spell player-id target-id 3))),
                :valid-targets-when-card-played
                  (fn [state player-id]
                    (->> (concat (get-minions state) (get-heroes state))
                         (map #(:id %)))),
                :rarity :none,
                :class :mage,
                :description "Deal 3 damage to a character and Freeze it."},
   "Cabal Shadow Priest"
     {:name "Cabal Shadow Priest",
      :attack 4,
      :health 5,
      :mana-cost 6,
      :type :minion,
      :set :classic,
      :rarity :epic,
      :battlecry (fn [state this {target-id :target-id}]
                   ; If it has a target AND the card played is this mininon
                   (if (not (nil? target-id))
                     (switch-side-minion state target-id)
                     state)),
      :valid-targets-when-card-played
        (fn [state player-id]
          (let [targets (filter #(<= (:attack %) 2) (get-minions state))]
            (if (empty? targets) nil (map #(:id %) targets)))),
      :description
        "Battlecry: Take control of an enemy minion that has 2 or less Attack."},
   "Mind Control"
     {:name "Mind Control",
      :mana-cost 10,
      :type :spell,
      :set :basic,
      :rarity :none,
      :spell-effect (fn [state {target-id :target-id}]
                      (switch-side-minion state target-id)),
      :valid-targets-when-card-played
        (fn [state player-id]
          (->> (get-minions state (get-opponent-player-id state player-id))
               (map #(:id %)))),
      :description "Take control of an enemy minion."},
   "Deranged Doctor"
     {:name "Deranged Doctor",
      :attack 8,
      :health 8,
      :mana-cost 8,
      :type :minion,
      :set :the-witchwood,
      :rarity :common,
      :description "Deathrattle: Restore 8 Health to your hero.",
      :states {:deathrattle [{:effect-source "Deranged Doctor",
                              :effect-key :on-own-death}]},
      :on-own-death
        (fn [state this]
          (heal-entity state (:id (get-hero state (get-owner this))) 8))},
   "Sylvanas Windrunner"
     {:name "Sylvanas Windrunner",
      :attack 5,
      :health 5,
      :mana-cost 6,
      :type :minion,
      :set :hall-of-fame,
      :rarity :legendary,
      :description "Deathrattle: Take control of a random enemy minion.",
      :states {:deathrattle [{:effect-source "Sylvanas Windrunner",
                              :effect-key :on-own-death}]},
      :on-own-death
        (fn [state this]
          (let [opponent (get-opponent-player-id state (get-owner this))
                opponents-minions (get-minions state opponent)]
            (if (empty? opponents-minions)
              ; If opponent does not have any minions, do nothing
              state
              ; Otherwise, steal the minion with minion-index
              (let [seed (get-seed state)
                    [new-seed minion-index]
                      (get-random-int seed (count opponents-minions))]
                (switch-side-minion
                  state
                  (:id (nth (get-in state [:players opponent :minions])
                            minion-index)))))))},
   "Frothing Berserker"
     {:name "Frothing Berserker",
      :attack 2,
      :health 4,
      :mana-cost 3,
      :type :minion,
      :set :classic,
      :rarity :rare,
      :description "Whenever a minion takes damage, gain +1 Attack.",
      :states {:effect {:on-damage [{:effect-source "Frothing Berserker",
                                     :effect-key :on-damage}]}},
      :on-damage (fn [state this-minion
                      {damaged-entity-id :character-id, damage :amount}]
                   (if (is-minion? state damaged-entity-id)
                     (buff-attack state (:id this-minion) "Berserk" 1)
                     ; If it is not a minion that is being damaged, don't
                     ; increase attack
                     state))},
   "Bananas" {:name "Bananas",
              :mana-cost 1,
              :type :spell,
              :set :classic,
              :description "Give a minion +1/+1.",
              :spell-effect
                (fn [state {target-id :target-id}]
                  (change-stats-minion state target-id "Bananas" 1 1)),
              :valid-targets-when-card-played (fn [state player-id]
                                                (->> (get-minions state)
                                                     (map #(:id %))))},
   "Loot Hoarder" {:name "Loot Hoarder",
                   :attack 2,
                   :health 1,
                   :mana-cost 2,
                   :type :minion,
                   :set :classic,
                   :rarity :common,
                   :description "Deathrattle: Draw a card.",
                   :states {:deathrattle [{:effect-source "Loot Hoarder",
                                           :effect-key :on-own-death}]},
                   :on-own-death (fn [state this]
                                   (draw-card state (get-owner this)))},
   "Snake Trap"
     {:name "Snake Trap",
      :mana-cost 2,
      :type :spell,
      :set :classic,
      :class :hunter,
      :rarity :epic,
      :spell-effect
        (fn [state {player-id :player-id, card :card}]
          (add-secret-to-board state player-id (create-secret "Snake Trap"))),
      :states {:effect {:on-attack [{:effect-source "Snake Trap",
                                     :effect-key :trap-triggered}]}},
      :trap-triggered
        (fn [state this {target :target}]
          (let [secret-owner (get-owner state (:id this))]
            (if (= (:owner-id target) secret-owner)
              (-> (add-minion-to-board state
                                       {:player-id secret-owner,
                                        :minion (create-minion "Snake"),
                                        :position 0})
                  (add-minion-to-board {:player-id secret-owner,
                                        :minion (create-minion "Snake"),
                                        :position 0})
                  (add-minion-to-board {:player-id secret-owner,
                                        :minion (create-minion "Snake"),
                                        :position 0})
                  ; Destroy self
                  (destroy-entity (:id this)))
              state))),
      :description
        "Secret: When one of your minions is attacked summon three 1/1 Snakes."},
   ;; Sprint 3
   "Moroes"
     {:name "Moroes",
      :attack 1,
      :health 1,
      :type :minion,
      :mana-cost 3,
      :set :one-night-in-karazhan,
      :rarity :legendary,
      :description "Stealth. At the end of your turn, summon a 1/1 Steward.",
      :states {:stealth true,
               :effect {:on-end-of-turn [{:effect-source "Moroes",
                                          :effect-key :on-end-of-turn}]}},
      :on-end-of-turn (fn [state this {player-id :player-id}]
                        (if (= (:owner-id this) player-id)
                          (add-minion-to-board state
                                               {:player-id player-id,
                                                :minion (create-minion
                                                          "Steward"),
                                                :position 0})
                          state))},
   "Shrinkmeister"
     {:name "Shrinkmeister",
      :attack 3,
      :health 2,
      :type :minion,
      :mana-cost 2,
      :class :priest,
      :battlecry (fn [state this {target-id :target-id}]
                   ; If it has a target
                   (if (not (nil? target-id))
                     (-> (buff-attack state target-id "Shrink Ray" -2)
                         (give-effect target-id
                                      :on-end-of-turn
                                      {:effect-source "Shrinkmeister",
                                       :effect-key :remove-buff-effect}))
                     state)),
      :valid-targets-when-card-played
        (fn [state player-id]
          (let [targets (get-minions state)]
            (if (empty? targets) nil (map #(:id %) targets)))),
      :set :goblins-vs-gnomes,
      :remove-buff-effect (fn [state this {}]
                            (-> (remove-one-buff state (:id this) "Shrink Ray")
                                (remove-one-effect
                                  (:id this)
                                  :on-end-of-turn
                                  {:effect-source "Shrinkmeister",
                                   :effect-key :remove-buff-effect}))),
      :rarity :common,
      :description "Battlecry: Give a minion -2 Attack this turn."},
   "Fireball" {:name "Fireball",
               :type :spell,
               :mana-cost 4,
               :class :mage,
               :set :basic,
               :rarity :none,
               :spell-effect
                 (fn [state {target-id :target-id, player-id :player-id}]
                   (damage-entity-with-spell state player-id target-id 6)),
               :valid-targets-when-card-played
                 (fn [state player-id]
                   (->> (concat (get-minions state) (get-heroes state))
                        (map #(:id %)))),
               :description "Deal 6 damage."},
   "Abusive Sergeant"
     {:name "Abusive Sergeant",
      :attack 1,
      :health 1,
      :type :minion,
      :mana-cost 1,
      :battlecry (fn [state this {target-id :target-id}]
                   ; If it has a target
                   (if (not (nil? target-id))
                     (-> (buff-attack state target-id "'Inspired'" 2)
                         (give-effect target-id
                                      :on-end-of-turn
                                      {:effect-source "Abusive Sergeant",
                                       :effect-key :remove-buff-effect}))
                     state)),
      :valid-targets-when-card-played
        (fn [state player-id]
          (let [targets (get-minions state)]
            (if (empty? targets) nil (map #(:id %) targets)))),
      :set :classic,
      :rarity :common,
      :remove-buff-effect (fn [state this {}]
                            (-> (remove-one-buff state (:id this) "'Inspired'")
                                (remove-one-effect
                                  (:id this)
                                  :on-end-of-turn
                                  {:effect-source "Abusive Sergeant",
                                   :effect-key :remove-buff-effect}))),
      :description "Battlecry: Give a minion +2 Attack this turn."},
   "Competitive Spirit"
     {:name "Competitive Spirit",
      :type :spell,
      :mana-cost 1,
      :class :paladin,
      :set :the-grand-tournament,
      :rarity :rare,
      :spell-effect (fn [state {player-id :player-id}]
                      (add-secret-to-board state
                                           player-id
                                           (create-secret
                                             "Competitive Spirit"))),
      :states {:effect {:on-start-of-turn [{:effect-source "Competitive Spirit",
                                            :effect-key :on-start-of-turn}]}},
      :on-start-of-turn (fn [state this {player-id :player-id}]
                          (if (not= player-id (get-owner this))
                            state
                            (-> (map-minions state
                                             add-buff
                                             (fn [minion]
                                               (= (get-owner minion) player-id))
                                             {:name "Competitive Spirit",
                                              :attack-buff 1,
                                              :max-health-buff 1})
                                (destroy-entity (:id this))))),
      :description "Secret: When your turn starts give your minions +1/+1."},
   ; TODO
   "Alarm-o-Bot"
     {:name "Alarm-o-Bot",
      :attack 0,
      :health 3,
      :type :minion,
      :mana-cost 3,
      :race :mech,
      :set :classic,
      :rarity :rare,
      :description
        "At the start of your turn swap this minion with a random one in your hand.",
      :states {:effect {:on-start-of-turn [{:effect-source "Alarm-o-Bot",
                                            :effect-key :on-start-of-turn}]}},
      :on-start-of-turn
        (fn [state this {player-id :player-id}]
          (if (not= player-id (get-owner this))
            state
            ; get available cards in hand and filter out the minion cards
            (let [available-cards (filter (fn [card]
                                            (= (:type
                                                 (definitions/get-definition
                                                   (:name card)))
                                               :minion))
                                    (get-hand state player-id))]
              (if (= (count available-cards) 0)
                state
                ; chose a random card from the minion cards in hand
                (let [seed (get-seed state)
                      [new-seed chosen-card] (random-nth seed available-cards)]
                  ; add chosen minion to board
                  (-> (add-minion-to-board state
                                           {:player-id player-id,
                                            :minion (create-minion
                                                      (:name chosen-card)),
                                            :position 6})
                      (remove-card-from-hand player-id (:id chosen-card))
                      (add-card-to-hand player-id (create-card "Alarm-o-Bot"))
                      (remove-minion (:id this))
                      (update-seed new-seed)))))))},
   "Archmage Antonidas"
     {:name "Archmage Antonidas",
      :attack 5,
      :health 7,
      :type :minion,
      :mana-cost 7,
      :class :mage,
      :set :classic,
      :rarity :legendary,
      :states {:effect {:on-spell-card-played
                          [{:effect-source "Archmage Antonidas",
                            :effect-key :on-spell-card-played}]}},
      :description
        "Whenever you cast a spell, add a 'Fireball' spell to your hand.",
      :on-spell-card-played
        (fn [state this {player-id :player-id}]
          (if (= player-id (get-owner state (:id this)))
            (add-card-to-hand state player-id (create-card "Fireball"))
            state))},
   "Unpowered Mauler"
     {:name "Unpowered Mauler",
      :attack 2,
      :health 4,
      :type :minion,
      :mana-cost 2,
      :set :the-boomday-project,
      :rarity :rare,
      :states {:can-attack? [{:effect-source "Unpowered Mauler",
                              :effect-key :can-attack}]},
      :can-attack (fn [state {}]
                    (< 0 (count (get state :spells-cast-this-turn)))),
      :description "Can only attack if you cast a spell this turn."},
   "Lorewalker Cho"
     {:name "Lorewalker Cho",
      :attack 0,
      :health 4,
      :type :minion,
      :mana-cost 2,
      :set :classic,
      :rarity :legendary,
      :states {:effect {:on-spell-card-played [{:effect-source "Lorewalker Cho",
                                                :effect-key
                                                  :on-spell-card-played}]}},
      :on-spell-card-played (fn [state this {player-id :player-id, card :card}]
                              (add-card-to-hand
                                state
                                (get-opponent-player-id state player-id)
                                ; TODO: should cho copy any instance values of
                                ; the card?
                                (create-card (:name card)))),
      :description
        "Whenever a player casts a spell, put a copy into the other player's hand."},
   "Doomsayer"
     {:name "Doomsayer",
      :attack 0,
      :health 7,
      :type :minion,
      :mana-cost 2,
      :set :classic,
      :rarity :epic,
      :states {:effect {:on-start-of-turn [{:effect-source "Doomsayer",
                                            :effect-key :on-start-of-turn}]}},
      :on-start-of-turn (fn [state this {player-id :player-id}]
                          (if (not= player-id (get-owner this))
                            ; if player starting the turn is not my owner, do
                            ; nothing
                            state
                            ; otherwise destroy all minions
                            (reduce (fn [state minion-id]
                                      (destroy-entity state minion-id))
                              state
                              (sort (map #(:id %) (get-minions state)))))),
      :description "At the start of your turn destroy ALL minions."},
   "Rampage"
     {:name "Rampage",
      :type :spell,
      :mana-cost 2,
      :class :warrior,
      :set :classic,
      :spell-effect (fn [state {target-id :target-id}]
                      (add-buff
                        state
                        target-id
                        {:name "Rampage", :attack-buff 3, :max-health-buff 3})),
      :valid-targets-when-card-played (fn [state player-id]
                                        (->> (map :id (get-minions state))
                                             (filter #(is-injured? state %)))),
      :rarity :common,
      :description "Give a damaged minion +3/+3."},
   "Trade Prince Gallywix"
     {:name "Trade Prince Gallywix",
      :attack 5,
      :health 8,
      :type :minion,
      :mana-cost 6,
      :class :rogue,
      :set :goblins-vs-gnomes,
      :rarity :legendary,
      :states {:effect {:on-spell-card-played
                          [{:effect-source "Trade Prince Gallywix",
                            :effect-key :on-spell-card-played}]}},
      :description
        "Whenever your opponent casts a spell, gain a copy of it and give them a Coin.",
      :on-spell-card-played
        (fn [state this {player-id :player-id, card :card}]
          (if (and (not= (:owner-id this) player-id)
                   (not= (:name card) "Gallywix's Coin"))
            (-> state
                (add-card-to-hand (:owner-id this) (create-card (:name card)))
                (add-card-to-hand player-id (create-card "Gallywix's Coin")))
            state))},
   ; Test for this is with gallywix in minion_tests
   "Gallywix's Coin"
     {:name "Gallywix's Coin",
      :type :spell,
      :mana-cost 0,
      :set :goblins-vs-gnomes,
      :spell-effect (fn [state {player-id :player-id}]
                      (increase-mana state player-id)),
      :description
        "Gain 1 Mana Crystal this turn only. (Won't trigger Gallywix.)"},
   "Blood Imp"
     {:name "Blood Imp",
      :attack 0,
      :health 1,
      :type :minion,
      :mana-cost 1,
      :race :demon,
      :class :warlock,
      :set :classic,
      :rarity :common,
      :states {:stealth true,
               :effect {:on-end-of-turn [{:effect-source "Blood Imp",
                                          :effect-key :on-end-of-turn}]}},
      :on-end-of-turn (fn [state this {player-id :player-id}]
                        (if (not= player-id (get-owner this))
                          ; If player ending turn is not my owner, do nothing
                          state
                          ; Otherwise ...
                          ; Compute a list of available friendly minions
                          ; excluding myself
                          (let [available-minions
                                  (map :id
                                    (filter (fn [x] (not= (:id x) (:id this)))
                                      (get-minions state player-id)))]
                            (if (= (count available-minions) 0)
                              ; If there are no available minions, do nothing
                              state
                              ; Otherwise, buff a random available minion with
                              ; +1 health
                              (let [seed (get-seed state)
                                    [new-seed chosen-minion-id]
                                      (random-nth seed available-minions)]
                                (-> (add-buff state
                                              chosen-minion-id
                                              {:name "Blood Pact",
                                               :attack-buff 0,
                                               :max-health-buff 1})
                                    (update-seed new-seed))))))),
      :description
        "Stealth. At the end of your turn give another random friendly minion +1 Health."},
   "The Coin" {:name "The Coin",
               :external-id "GAME_005",
               :type :spell,
               :mana-cost 0,
               :set :basic,
               :spell-effect (fn [state {player-id :player-id}]
                               (increase-mana state player-id)),
               :description "Gain 1 Mana Crystal this turn only."},
   "Malygos" {:name "Malygos",
              :attack 4,
              :health 12,
              :type :minion,
              :mana-cost 9,
              :auras [{:spell-damage 5}],
              :race :dragon,
              :set :classic,
              :rarity :legendary,
              :description "Spell Damage +5"},
   "Steward" {:name "Steward",
              :attack 1,
              :health 1,
              :type :minion,
              :mana-cost 1,
              :set :one-night-in-karazhan,
              :rarity :none,
              :description ""},
   "Flare"
     {:name "Flare",
      :type :spell,
      :mana-cost 2,
      :class :hunter,
      :set :classic,
      :rarity :rare,
      :spell-effect (fn [state {player-id :player-id}]
                      (-> (map-minions state remove-stealth)
                          (remove-secrets (get-opponent-player-id state
                                                                  player-id))
                          (draw-card player-id))),
      :description
        "All minions lose Stealth. Destroy all enemy Secrets. Draw a card."}})

(definitions/add-definitions! card-definitions)
