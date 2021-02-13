(ns firestone.api
  (:require
    [ysera.test :refer [is is-not is= error?]]
    [ysera.collections :refer [seq-contains?]]
    [firestone.definitions :refer [get-definition]]
    [clojure.pprint :refer [pprint]]
    [firestone.events :refer
     [fire-event do-battlecry fire-on-attack-event
      fire-on-spell-card-played-event fire-start-of-turn-event
      fire-end-of-turn-event]]
    [firestone.info :refer
     [get-health get-entity get-minions get-max-health get-spell-effect stealth?
      is-minion? has-card? get-opponent-player-id valid-attack?
      valid-card-target? get-attack get-hand get-deck get-minion get-hero
      is-hero? get-secrets get-mana get-hero-power-def has-enough-mana?
      can-use-hero-power? sleepy? get-max-mana frozen?]]
    [firestone.construct :refer
     [create-card create-hero create-game create-minion update-minion
      add-secret-to-board add-card-to-deck add-card-to-hand
      add-minion-to-board]]
    [firestone.damage-entity :refer [damage-entity damage-entities]]
    [firestone.core :refer
     [overdraw-card pop-top-card-from-deck unfreeze-entities freeze-entity
      remove-stealth]]
    [firestone.mana :refer [spend-mana refill-mana increase-max-mana]]))

(declare end-turn)

(defn draw-card
  "
  If :deck is empty, increment :cards-overdrawn and add damage equal to it to the player
  else if :hand has 10 cards, destroy a card from the :deck.
  otherwise move a card from :deck to :hand
  "
  {:test (fn []
           ; Try to draw two cards from an empty deck
           (is= (-> (create-game)
                    (draw-card "p1")
                    (draw-card "p1")
                    (get-in [:players "p1" :cards-overdrawn]))
                2)
           ; While drawing two cards from an empty deck, 3 damage should be
           ; applied to the hero
           (is= (-> (draw-card (create-game) "p1")
                    (draw-card "p1")
                    (get-health "h1"))
                27)
           ; Drawing a card should put it in your hand
           (is= (-> (create-game)
                    (add-card-to-deck "p1" "Imp")
                    (draw-card "p1")
                    (get-hand "p1")
                    (count))
                1)
           ; Having 10 cards in your hand should decrease the number of cards in
           ; your deck but you should still have the same hand
           (let [full-hand-state (->
                                   (create-game)
                                   ; TODO make this nicer
                                   (add-card-to-deck "p1" "Defender")
                                   (add-card-to-deck "p1" "War Golem")
                                   (add-card-to-deck "p1" "Defender")
                                   (add-card-to-deck "p1" "Imp")
                                   (add-card-to-deck "p1" "Imp")
                                   (add-card-to-deck "p1" "Imp")
                                   (add-card-to-deck "p1" "Imp")
                                   (add-card-to-deck "p1" "Imp")
                                   (add-card-to-deck "p1" "Defender")
                                   (add-card-to-deck "p1" "Defender")
                                   (add-card-to-deck "p1" "Dalaran Mage")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1")
                                   (draw-card "p1"))]
             ; After drawing 10 cards from a deck with an original size of 11,
             ; there should be 1 left in the deck and 10 cards in hand
             (is= (-> (get-in full-hand-state [:players "p1" :deck])
                      (count))
                  1)
             (is= (-> (get-in full-hand-state [:players "p1" :hand])
                      (count))
                  10)
             ; Drawing a card when having 10 cards in hand should result in
             ; there still being the same 10 cards in your hand and the card
             ; being destroyed.
             ; This means that there should be 0 cards left in deck and the same
             ; cards should be in the hand
             (is= (-> (draw-card full-hand-state "p1")
                      (get-in [:players "p1" :deck])
                      (count))
                  0)
             ; Cards in hand should not change when the hand is already full
             (is= (-> (draw-card full-hand-state "p1")
                      (get-in [:players "p1" :hand]))
                  (get-in full-hand-state [:players "p1" :hand]))))}
  [state player-id]
  {:pre [(map? state) (string? player-id)]}
  (if (empty? (get-deck state player-id))
    (overdraw-card state player-id)
    ; Take the top card
    (let [[state card] (pop-top-card-from-deck state player-id)]
      (if (= 10 (count (get-hand state player-id)))
        ; Discard top card
        state
        ; Add card to deck
        (add-card-to-hand state player-id card)))))

(defn attack-entity
  ; TODO make compatible with weapons
  "Attacks entity. Can attack both heroes and minions."
  {:test
     (fn []
       (is=
         (-> (create-game [{:minions [(create-minion "Dalaran Mage" :id "i")]}
                           {:minions [(create-minion "Dalaran Mage" :id "i2")]}]
                          :minion-ids-summoned-this-turn
                          [])
             (attack-entity "i" "i2")
             (get-minion "i2")
             (:health))
         3)
       (let [post-attack-state
               (-> (create-game
                     [{:minions [(create-minion "Dalaran Mage" :id "i")]}
                      {:minions [(create-minion "Dalaran Mage" :id "i2")]}]
                     :minion-ids-summoned-this-turn
                     [])
                   (attack-entity "i" "i2")
                   (end-turn)
                   (end-turn))]
         ; After "i" attacks "i2" they should have "i2"::dmg-taken =
         ; "i"::attack
         ; and "i"::dmg-taken = "i2"::attack
         (is= (-> (get-minion post-attack-state "i2")
                  (:health))
              (- (get-max-health post-attack-state "i2")
                 (get-attack post-attack-state "i")))
         (is= (->> (get-minion post-attack-state "i")
                   (:health)
                   (- (get-max-health post-attack-state "i")))
              (get-attack post-attack-state "i2"))
         ; Attack once more and see if damage is doubled
         (let [post-attack-state (attack-entity post-attack-state "i" "i2")]
           (is= (-> (get-minion post-attack-state "i2")
                    (get-health))
                2)
           (is= (-> (get-minion post-attack-state "i")
                    (get-health))
                2)))
       ; Minion should lose stealth after attacking
       (as-> (create-game
               [{:minions [(create-minion "Blood Imp" :id "bi1" :attack 1)]}]) $
         (do (is (stealth? $ "bi1"))
             (as-> (end-turn $) $
               (end-turn $)
               (attack-entity $ "bi1" "h2")
               (is-not (stealth? $ "bi1")))))
       ; Test attacking heroes
       (is= (-> (create-game [{:minions [(create-minion "Ogre Magi" :id "i")]}
                              {:hero (create-hero "Jaina Proudmoore")}]
                             :minion-ids-summoned-this-turn
                             [])
                (attack-entity "i" "h1")
                (get-health "h1"))
            26))}
  [state attacker-id target-id]
  {:pre [(valid-attack? state attacker-id target-id)]}
  (let [target (get-entity state target-id)]
    (as-> (fire-on-attack-event state attacker-id target) $
      ; Attacker should lose stealth
      (remove-stealth $ attacker-id)
      ; If it is a minion, increase attacks performed this turn
      (if (is-minion? $ attacker-id)
        (update-minion $ attacker-id :attacks-performed-this-turn inc)
        $)
      (if (is-hero? $ target-id)
        ; if target is a hero, don't damage the attacker
        (damage-entity $ target-id (get-attack state attacker-id))
        ; otherwise, both should be damaged
        (damage-entities $
                         [attacker-id target-id]
                         [(get-attack $ target-id)
                          (get-attack $ attacker-id)])))))
;(-> (damage-entity $ target-id (get-attack state attacker-id))
;    (damage-entity attacker-id (get-attack state target-id)))))))



(defn get-card-from-hand
  " Get a card with the given id from the player with player-id"
  {:test (fn []
           (comment (error? (-> (create-game)
                                (get-card-from-hand "p1" "c100"))))
           (is= (-> (create-game [{:hand [(create-card "Imp" :id "c1")]}])
                    (get-card-from-hand "p1" "c1")
                    (:name))
                "Imp"))}
  [state player-id card-id]
  (->> (get-in state [:players player-id :hand])
       (filter (fn [card] (= (get card :id) card-id)))
       (first)))

(defn remove-card-from-hand
  "Removing a card from hand, used when playing a card"
  {:test (fn []
           (is-not (-> (create-game [{:hand [(create-card "Imp")]}])
                       (remove-card-from-hand "p1" "c1")
                       (has-card? "p1" "c1"))))}
  [state player-id card-id]
  (update-in state
             [:players player-id :hand]
             (fn [hand]
               (filter (fn [card] (not (= card-id (:id card)))) hand))))

(defn play-spell-card
  "Plays a spell card. Will check possible targets for you. Subtracts mana and removes the card from the hand.
   Events fired: after-spell-card-played"
  ; More tests can be found in tests/firestone/spells
  {:test (fn []
           (as-> (create-game
                   [{:hand [(create-card "Frostbolt" :id "fb1")], :mana 5}
                    {:minions
                       [(create-minion "War Golem" :health 7 :id "wg1")]}]) $
             (play-spell-card $ "p1" "fb1" "wg1")
             (do ; minion should be damaged
                 (is= (get-health $ "wg1") 4)
                 ; The card should be played
                 (is= (count (get-hand $ "p1")) 0)
                 ; Mana should be subtracted
                 (is= (get-mana $ "p1") 3))))}
  ([state player-id card-id] (play-spell-card state player-id card-id nil))
  ([state player-id card-id target-id]
   {:pre (has-card? state player-id card-id)}
   (let [card (get-card-from-hand state player-id card-id)]
     ; Check that the target is valid
     (if (not (valid-card-target? state player-id (:name card) target-id))
       (do ;(pprint state)
           (throw (Exception. (str "Cannot target "
                                     (if (nil? target-id) "nothing" target-id)
                                   " with id " card-id))))
       (-> (remove-card-from-hand state player-id card-id)
           (spend-mana player-id (:mana-cost card))
           (update :spells-cast-this-turn #(conj % card))
           ((get-spell-effect card)
             {:target-id target-id, :player-id player-id, :card card})
           (fire-on-spell-card-played-event card player-id target-id))))))

(defn play-secret-card
  "Play a secret."
  {:pre (fn []
          (as-> (create-game [{:hand [(create-card "Snake Trap" :id "secret1")],
                               :mana 5}])
            $
            (play-secret-card $ "p1" "secret1")
            (do ; there should be one secret in play
                (is= (count (get-secrets $ "p1")) 1)
                ; the player should have spent 2 mana and have 5 - 2 = 3 mana
                (is= (get-mana $ "p1") 3))))}
  [state player-id card-id]
  {:pre [(has-card? state player-id card-id)
         (has-enough-mana? state
                           player-id
                           (:mana-cost
                             (get-card-from-hand state player-id card-id)))]}
  (let [card (get-card-from-hand state player-id card-id)]
    (-> (spend-mana state player-id (:mana-cost card))
        (add-secret-to-board state player-id card))))

(defn play-minion-card
  "Play minion card and add it to board"
  {:test (fn []
           (is=
             (-> (create-game [{:hand [(create-card "Arcane Golem" :id "c1")]}])
                 (play-minion-card "p1" "c1" 0)
                 (get-minions "p1")
                 (first)
                 (:name))
             "Arcane Golem")
           (let [post-play-state (-> (create-game
                                       [{:hand [(create-card "Imp" :id "c1")
                                                (create-card "Imp" :id "c2")
                                                (create-card "Imp" :id "c3")]}
                                        {:hand [(create-card "Imp" :id "c4")
                                                (create-card "Imp")
                                                (create-card "Imp")]}])
                                     (play-minion-card "p1" "c1" 0)
                                     (play-minion-card "p2" "c4" 0))]
             (is= (-> (get-in post-play-state [:players "p1" :minions])
                      (count))
                  1)
             ; Should have spent 1 mana each
             (is= (get-mana post-play-state "p1") 9)
             (is= (get-mana post-play-state "p2") 9)
             (is= (count (get-in post-play-state [:players "p1" :hand])) 2)
             (is-not (valid-attack? post-play-state "c1" "c2"))
             (is-not (has-card? post-play-state "p1" "c1"))))}
  ; Playing a minion without a target
  ([state player-id card-id position-on-board]
   (play-minion-card state player-id card-id position-on-board nil))
  ([state player-id card-id position-on-board target-id]
   {:pre [(has-card? state player-id card-id)
          (has-enough-mana? state
                            player-id
                            (:mana-cost
                              (get-card-from-hand state player-id card-id)))]}
   (let [card (get-card-from-hand state player-id card-id)]
     ; Check that the target is valid
     (if (not (valid-card-target? state player-id (:name card) target-id))
       (do ;(pprint state)
           (throw (Exception. (str "Cannot target "
                                     (if (nil? target-id) "nothing" target-id)
                                   " with id " card-id))))
       (let [minion (create-minion card)]
         ; Play the card
         ; remove card from hand
         (-> (remove-card-from-hand state player-id card-id)
             ; Spend mana
             (spend-mana player-id
                         (:mana-cost
                           (get-card-from-hand state player-id card-id)))
             ; add minion to board
             (add-minion-to-board {:player-id player-id,
                                   :minion minion,
                                   :position position-on-board})
             ; If it has a battlecry, do it
             (do-battlecry card-id target-id)
             ; Fire an event that a card has been played
             (fire-event {:name "after-minion-card-played",
                          :card card,
                          :target-id target-id})))))))

(defn end-turn
  "End turn for player, let opponent play.
   Events fired: start-of-turn, end-of-turn"
  ; TODO maybe minions shouldn't be unfrozen directly when a turn ends but when
  ; their owners turn begins.
  {:test
     (fn []
       ; Test if minions are unfrozen
       (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}]) $
         (freeze-entity $ "i1")
         (do (is (frozen? $ "i1"))
             (as-> (end-turn $) $ (is-not (frozen? $ "i1")))))
       (is= (-> (create-game [{:hero
                                 (create-hero "Jaina Proudmoore" :id "h1")}])
                (end-turn)
                (:player-id-in-turn))
            "p2")
       (is= (-> (create-game [{:mana 1, :max-mana 1} {:mana 1, :max-mana 1}])
                (end-turn)
                (get-mana "p2"))
            1)
       (is=
         (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
             (end-turn)
             (end-turn)
             (:player-id-in-turn))
         "p1")
       (as->
         (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1"),
                        :mana 1,
                        :max-mana 1}
                       {:hero (create-hero "Jaina Proudmoore" :id "h2"),
                        :mana 1,
                        :max-mana 1}]) $
         (end-turn $)
         (end-turn $)
         (do (is= (get-max-mana $ "p1") 2) (is= (get-max-mana $ "p2") 2)))
       ; test if minions stop being sleepy after end turn
       (as-> (create-game) $
         (add-minion-to-board $
                              {:player-id "p1",
                               :minion (assoc (create-minion "Imp" :id "i1")
                                         :owner-id "p1"),
                               :position 0})
         (do (is (sleepy? $ "i1"))
             (as-> (end-turn $) $
               (do (is-not (sleepy? $ "i1")) (valid-attack? $ "i1" "h2")))))
       (is=
         (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
             (assoc :minion-ids-summoned-this-turn ["i"])
             (end-turn)
             (:minion-ids-summoned-this-turn)
             (count))
         0)
       ; test that :attacks-performed-this-turn  is reset
       (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                (attack-entity "i" "h2")
                (end-turn)
                (get-entity "i")
                (:attacks-performed-this-turn))
            0)
       ; test that :attacks-performed-this-turn  is reset
       (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}]
                             :spells-cast-this-turn
                             ["TEST"])
                (attack-entity "i" "h2")
                (end-turn)
                (:spells-cast-this-turn))
            [])
       ; Test the resetting of :times-hero-power-used
       (is=
         (as-> (create-game []) $
           (assoc-in $ [:players "p1" :hero :times-hero-power-used] 1)
           (assoc-in $ [:players "p2" :hero :times-hero-power-used] 1)
           (end-turn $)
           (and (= (get-in $ [:players "p1" :hero :times-hero-power-used]) 0)
                (= (get-in $ [:players "p2" :hero :times-hero-power-used]) 0)))
         true))}
  [state]
  (let [next-player-id (get-opponent-player-id state
                                               (:player-id-in-turn state))]
    (as-> (fire-end-of-turn-event state (:player-id-in-turn state)) $
      (if (= next-player-id "p1")
        (-> $
            (increase-max-mana "p1" 1)
            (increase-max-mana "p2" 1))
        $)
      (assoc $ :minion-ids-summoned-this-turn [])
      (assoc $ :spells-cast-this-turn [])
      (assoc-in $ [:players "p1" :hero :times-hero-power-used] 0)
      (assoc-in $ [:players "p2" :hero :times-hero-power-used] 0)
      (refill-mana $ next-player-id)
      ; Set :attacks-performed-this-turn to zero for all minions
      (reduce
        (fn [state minion]
          (update-minion state (:id minion) :attacks-performed-this-turn 0))
        $
        (get-minions $))
      (unfreeze-entities $ (:player-id-in-turn $))
      (assoc $ :player-id-in-turn next-player-id)
      (draw-card $ (get-in $ [:player-id-in-turn]))
      (fire-start-of-turn-event $ next-player-id))))

(defn play-hero-power
  "Play a hero power."
  {:test
     (fn []
       (as-> (create-game [{:mana 5, :hero (create-hero "Jaina Proudmoore")}
                           {:hero (create-hero "Rexxar" :health 10)}]) $
         (play-hero-power $ "p1" "h2")
         (do ; Rexxar should be damaged
             (is= (get-health $ "h2") 9)
             (is= (get-mana $ "p1") 3)
             ; Jaina should have :times-hero-power-used = 1
             (is= (get-in $ [:players "p1" :hero :times-hero-power-used]) 1)
             (is= (can-use-hero-power? $ "p1") false)
             ; Jaina should not be able to use the hero power again
             (is (thrown? AssertionError (play-hero-power $ "p1" "h2")))
             ; Rexxar should have :times-hero-power-used = 0
             (is= (get-in $ [:players "p2" :hero :times-hero-power-used]) 0)
             (is (-> (end-turn $)
                     (can-use-hero-power? "p2")))
             $)
         (end-turn $) ; Test the resetting of :times-hero-power-used
         (do (is= (get-in $ [:players "p1" :hero :times-hero-power-used]) 0)
             (is= (get-in $ [:players "p2" :hero :times-hero-power-used]) 0))))}
  ([state player-id target-id]
   {:pre [(has-enough-mana? state player-id 2)
          (can-use-hero-power? state player-id)]}
   (-> (spend-mana state player-id 2)
       ; TODO: extract this part of play-hero-power into its own function? e.g
       ; (inc-hero-power-used [state id])
       (update-in [:players player-id :hero :times-hero-power-used] #(+ 1 %))
       ((:hero-power-effect (get-hero-power-def state player-id))
         {:player-id player-id, :target-id target-id})))
  ([state player-id] (play-hero-power state player-id nil)))
