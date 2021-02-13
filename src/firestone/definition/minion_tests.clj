; *neode.onsave* setgo cd ../../.. && lein test

(ns firestone.definition.minion-tests
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [clojure.pprint :refer [pprint]]
            [firestone.spec :as fspec]
            [clojure.spec.alpha :as spec]
            [firestone.api :refer
             [end-turn attack-entity play-minion-card play-spell-card]]
            ; [firestone.mana :refer []]
            [firestone.damage-entity :refer [damage-entity destroy-entity]]
            [firestone.core :refer :all]
            [firestone.info :refer :all]
            ;[get-max-mana get-mana valid-attack? sleepy? get-attack get-minions
            ;get-hand
            ;get-health get-secrets get-owner]]
            [firestone.construct :refer
             [create-game create-minion create-card create-hero
              create-secret]]))

(clojure.test/deftest shrinkmeister
  ; If no targets, nothing should happen (check that no errors occur)
  (-> (create-game [{:hand [(create-card "Shrinkmeister" :id "s1")]}])
      (play-minion-card "p1" "s1" 0))
  ; Check that it does not overwrite other effects (like blood imps effect)
  (as-> (create-game [{:minions [(create-minion "Blood Imp" :id "bi1")],
                       :hand [(create-card "Shrinkmeister" :id "s1")]}]) $
    (remove-stealth $ "bi1")
    (play-minion-card $ "p1" "s1" 0 "bi1")
    (do (is= (get-attack $ "bi1") 0)
        (as-> (end-turn $) $
          (do ; attack should be gone
              (is= (get-attack $ "bi1") 0)
              ; shrinkmeister should have one more health
              (is= (get-health $ "s1") 3)
              (as-> (end-turn $) $
                (end-turn $)
                (do ; The imp should only have one end-turn effect
                    (is= (count (get-in (get-entity $ "bi1")
                                        [:states :effect :on-end-of-turn]))
                         1)
                    ; abusive should have one more health
                    (is= (get-health $ "s1") 4)))))))
  ; Buffing a minion twice should give it +4 attack and remove it at the end of
  ; the turn
  (as-> (create-game
          [{:minions [(create-minion "War Golem" :attack 7 :id "i1")],
            :hand [(create-card "Shrinkmeister" :id "s1")
                   (create-card "Shrinkmeister" :id "s2")]}]) state
    (play-minion-card state "p1" "s1" 0 "i1")
    (play-minion-card state "p1" "s2" 0 "i1")
    (do (is= (count (get-minions state)) 3)
        ; 1 + 2 + 2
        (is= (get-attack state "i1") 3)
        (as-> (end-turn state) state
          (do ; Check that they have no buffs or effects on them
              (is= (get-in (get-entity state "i1") [:states :effect]) nil)
              (is= (get-attack state "i1") 7))))))

(clojure.test/deftest abusive-sergeant
  ; If no targets, nothing should happen (check that no errors occur)
  (-> (create-game [{:hand [(create-card "Abusive Sergeant" :id "as1")]}])
      (play-minion-card "p1" "as1" 0))
  ; Check that it does not overwrite other effects (like blood imps effect)
  (as-> (create-game [{:minions [(create-minion "Blood Imp" :id "bi1")],
                       :hand [(create-card "Abusive Sergeant" :id "as1")]}]) $
    (remove-stealth $ "bi1")
    (play-minion-card $ "p1" "as1" 0 "bi1")
    (do (is= (get-attack $ "bi1") 2)
        (as-> (end-turn $) $
          (do ; attack should be gone
              (is= (get-attack $ "bi1") 0)
              ; abusive should have one more health
              (is= (get-health $ "as1") 2)
              (as-> (end-turn $) $
                (end-turn $)
                (do ; The imp should only have one end-turn effect
                    (is= (count (get-in (get-entity $ "bi1")
                                        [:states :effect :on-end-of-turn]))
                         1)
                    ; abusive should have one more health
                    (is= (get-health $ "as1") 3)))))))
  ; Buffing a minion twice should give it +4 attack and remove it at the end of
  ; the turn
  (as-> (create-game [{:minions [(create-minion "Imp" :attack 1 :id "i1")],
                       :hand [(create-card "Abusive Sergeant" :id "as1")
                              (create-card "Abusive Sergeant" :id "as2")]}])
    state
    (play-minion-card state "p1" "as1" 0 "i1")
    (play-minion-card state "p1" "as2" 0 "i1")
    (do (is= (count (get-minions state)) 3)
        ; 1 + 2 + 2
        (is= (get-attack state "i1") 5)
        (as-> (end-turn state) state
          (do ; Check that they have no buffs or effects on them
              (is= (get-in (get-entity state "i1") [:states :effect]) nil)

              (is= (get-attack state "i1") 1))))))

(clojure.test/deftest unpowered-mauler
  (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}
                      {:minions [(create-minion "Unpowered Mauler" :id "um1")],
                       :hand [(create-card "Snake Trap" :id "st1")]}]) $
    (end-turn $)
    (do (is-not (valid-attack? $ "um1" "i1"))
        (as-> (play-spell-card $ "p2" "st1") $
          (do (is (valid-attack? $ "um1" "i1"))
              (as-> (end-turn $) $
                (end-turn $)
                (is-not (valid-attack? $ "um1" "i1"))))))))

(clojure.test/deftest sylvanas
  ; Specific test case that didn't work in view
  (as-> (create-game [{:minions
                         [(create-minion "Sylvanas Windrunner" :id "sw1")]}
                      {:minions [(create-minion "Deranged Doctor" :id "dd2")]}])
    $
    (attack-entity $ "sw1" "dd2")
    (do (is= (count (get-minions $ "p1")) 1)
        (is= (count (get-minions $ "p2")) 0)
        (is= (:name (first (get-minions $ "p1"))) "Deranged Doctor")))
  (as-> (create-game
          [{:minions [(create-minion "Sylvanas Windrunner" :id "sw1")]}
           {:minions [(create-minion "Imp" :id "i3")
                      (create-minion "Deranged Doctor" :id "dd2" :health 4)]}]
          :seed
          1) $
    (attack-entity $ "dd2" "sw1")
    (do (is= (count (get-minions $ "p1")) 1)
        (is= (count (get-minions $ "p2")) 0)
        (is= (:name (first (get-minions $ "p1"))) "Imp")))
  ; If no minions are on the board, nothing should happen
  (as-> (create-game [{:minions
                         [(create-minion "Sylvanas Windrunner" :id "1")]}]) $
    (destroy-entity $ "1")
    (is= (count (get-minions $ "p1")) 0))
  ; Three possible minions, should be able to steal every one
  (as-> (create-game
          [{:minions [(create-minion "Sylvanas Windrunner" :id "s1")]}
           {:minions [(create-card "Imp" :id "i1")
                      (create-card "War Golem" :id "wg1")
                      (create-card "Ogre Magi" :id "om1")]}]) $
    (do (as-> (update-seed $ 0) a
          (destroy-entity a "s1")
          (do ; p2 only has 2 minion
              (is= (count (get-minions a "p2")) 2)
              ; p1 has one minion
              (is= (count (get-minions a "p1")) 1)
              ; The minion that was stolen was an Imp
              (is= (:name (nth (get-minions a "p1") 0)) "Imp")))
        (as-> (update-seed $ 1) b
          (destroy-entity b "s1")
          (do ; The minion that was stolen was a War Golem
              (is= (:name (nth (get-minions b "p1") 0)) "War Golem")))
        (as-> (update-seed $ 2) c
          (destroy-entity c "s1")
          (do ; The minion that was stolen was an Ogre Magi
              (is= (:name (nth (get-minions c "p1") 0)) "Ogre Magi"))))))

(clojure.test/deftest ancient-watcher
  (as-> (create-game [{:hand [(create-card "Ancient Watcher" :id "aw1")]}]) $
    (play-minion-card $ "p1" "aw1" 0)
    (do ; Can't attack the same turn it was played, just like all other cards.
        (is (sleepy? $ "aw1"))
        (is-not (valid-attack? $ "aw1" "h2"))
        (as-> (end-turn $) $
          ; Attack should not be valid second turn even though it is not sleepy
          (do (is-not (sleepy? $ "aw1"))
              (is-not (valid-attack? $ "aw1" "h2")))))))


(clojure.test/deftest cabal-shadow-priest
  ; Standard case
  (as-> (create-game [{:hand [(create-card "Cabal Shadow Priest" :id "csp1")]}
                      {:minions [(create-minion "Imp" :id "i1")]}]) $
    (play-minion-card $ "p1" "csp1" 0 "i1")
    (do (is= (count (get-minions $ "p2")) 0)
        (is= (count (get-minions $ "p1")) 2)))
  ; Should not be able to steal buffed minion
  (as-> (create-game [{:hand [(create-card "Cabal Shadow Priest" :id "csp1")]}
                      {:minions [(create-minion "Imp" :id "i1" :attack 10)]}]) $
    (error? (play-minion-card $ "p1" "csp1" 0 "i1")))
  ; Should be able to steal debuffed minion
  (as-> (create-game [{:hand [(create-card "Cabal Shadow Priest" :id "csp1")]}
                      {:minions
                         [(create-minion "War Golem" :id "i1" :attack 2)]}]) $
    (play-minion-card $ "p1" "csp1" 0 "i1")
    (do (is= (count (get-minions $ "p2")) 0)
        (is= (count (get-minions $ "p1")) 2)))
  ; Must steal a minion if there is one available
  (error? (-> (create-game
                [{:hand [(create-card "Cabal Shadow Priest" :id "csp1")]}
                 {:minions [(create-minion "War Golem" :id "i1" :attack 2)]}])
              (play-minion-card "p1" "csp1" 0)))
  ; Nothing to steal, mostly to check for error
  (as-> (create-game [{:hand [(create-card "Cabal Shadow Priest" :id "csp1")]}])
    $
    (play-minion-card $ "p1" "csp1" 0)
    (is= (count (get-minions $ "p1")) 1)))

(clojure.test/deftest big-game-hunter
  (as-> (create-game [{:hand [(create-card "Big Game Hunter" :id "bgh1")
                              (create-card "Big Game Hunter" :id "bgh2")]}
                      {:minions [(create-minion "Imp" :id "i1")
                                 (create-minion "War Golem" :id "wg1")]}]) $
    (do ; If a valid target is present, can't target nothing
        (error? (play-minion-card $ "p1" "bgh1" 0))
        ; Can't target minion with less than 7 attack
        (error? (play-minion-card $ "p1" "bgh1" 0 "i1"))
        (as-> (play-minion-card $ "p1" "bgh1" 0 "wg1") $
          (do ; Only one minion should be left, the imp
              ;(pprint $)
              (is= (:name (first (get-minions $ "p2"))) "Imp")
              (is= (count (get-in $ [:players "p2" :minions])) 1)
              ; Should be able to play the other BGH without any target or
              ; problems
              (play-minion-card $ "p1" "bgh2" 0))))))

(clojure.test/deftest eater-of-secrets
  ; Test so that it doesn't eat own secrets
  (as-> (create-game [{:hand [(create-card "Eater of Secrets" :id "eos1")],
                       :secrets [(create-secret "Snake Trap" :id "st1")]}]) $
    (play-minion-card $ "p1" "eos1" 0)
    (is= (count (get-secrets $ "p1")) 1))
  ; Test normal use
  (as-> (create-game [{:hand [(create-card "Eater of Secrets" :id "eos1")]}
                      {:secrets [(create-secret "Snake Trap" :id "st1")
                                 (create-secret "Snake Trap" :id "st2")]}]) $
    (play-minion-card $ "p1" "eos1" 0)
    (do (is= (count (get-secrets $)) 0)
        (is= (get-health $ "eos1") 6)
        (is= (get-attack $ "eos1") 4))))


(clojure.test/deftest loot-hoarder
  (is=
    (as-> (create-game [{:minions [(create-minion "Loot Hoarder" :id "l1")],
                         :deck [(create-card "Imp")]}]) $
      (destroy-entity $ "l1")
      (get-in $ [:players "p1" :hand])
      (count $))
    1))

(clojure.test/deftest deranged-doctor
  (as-> (create-game [{:hero (create-hero "Rexxar" :health 20),
                       :minions [(create-minion "Deranged Doctor" :id "m1")]}]) $
    (destroy-entity $ "m1")
    (do (is= (get-health $ "h1") 28) (is= (count (get-minions $)) 0))))

(clojure.test/deftest arcane-golem
  ; If the other player has 10 max-mana, nothing should happen
  (is= (-> (create-game [{:hand [(create-card "Arcane Golem" :id "ag1")]}
                         {:max-mana 10}])
           (play-minion-card "p1" "ag1" 0)
           (get-max-mana "p2"))
       10)
  (is= (-> (create-game [{:hand [(create-card "Arcane Golem")
                                 (create-card "Arcane Golem")]} {:max-mana 1}])
           (play-minion-card "p1" "1" 0) ; id1
           (play-minion-card "p1" "2" 0) ; id2
           (get-max-mana "p2"))
       3))

(clojure.test/deftest king-mukla
  (is= (-> (create-game [{:hand [(create-card "King Mukla")]}])
           (play-minion-card "p1" "1" 0) ; id1
           (get-hand "p2")
           (count))
       2)
  (is= (-> (create-game [{:hand [(create-card "King Mukla")
                                 (create-card "King Mukla")]}])
           (play-minion-card "p1" "1" 0) ; id1
           (play-minion-card "p1" "2" 0) ; id2
           (get-hand "p2")
           (count))
       4)
  (is=
    (-> (create-game [{:hand [(create-card "King Mukla") (create-card "Imp")]}])
        (play-minion-card "p1" "1" 0) ; id1
        (play-minion-card "p1" "2" 0) ; id2
        (get-hand "p2")
        (count))
    2)
  ; test that the cards added to the hand really are bananas
  (is= (-> (create-game [{:hand [(create-card "King Mukla" :id "km1")]}])
           (play-minion-card "p1" "km1" 0)
           (get-hand "p2")
           (first)
           (:name))
       "Bananas"))



; Acolyte of pain
(clojure.test/deftest acolyte-of-pain
  (is=
    (-> (create-game [{:minions [(create-minion "Acolyte of Pain" :id "a100")],
                       :deck [(create-card "Imp")]}])
        (damage-entity "a100" 1)
        (get-hand "p1")
        (count))
    1)
  ; If acolyte dies, he still draws a card
  (is=
    (-> (create-game [{:minions [(create-minion "War Golem" :id "wg1")]}
                      {:minions [(create-minion "Acolyte of Pain" :id "a10")],
                       :deck [(create-card "Imp")]}])
        (attack-entity "wg1" "a10")
        (get-hand "p2")
        (count))
    1)
  (is= (-> (create-game [{:minions [(create-minion "Acolyte of Pain" :id "a100")
                                    (create-minion "Imp" :id "i")],
                          :deck [(create-card "Imp")]}])
           (damage-entity "i" 1)
           (get-hand "p1")
           (count))
       0))

(clojure.test/deftest frothing-berserker
  ; test that berserker gains attack when enemy minions are damaged
  (is=
    (-> (create-game [{:minions [(create-minion "Frothing Berserker" :id "fb1")
                                 (create-minion "Imp" :id "i1")]}
                      {:minions [(create-minion "War Golem" :id "wg1")]}])
        (attack-entity "i1" "wg1")
        (get-attack "fb1"))
    4)
  ; test that berserker gains attack when enemy minions are damaged
  (is= (-> (create-game [{:minions
                            [(create-minion "Frothing Berserker" :id "fb1")]}
                         {:minions [(create-minion "Imp" :id "i1")]}])
           (attack-entity "fb1" "i1")
           (get-attack "fb1"))
       4)
  (is=
    (-> (create-game [{:minions [(create-minion "Frothing Berserker" :id "a100")
                                 (create-minion "Imp" :id "m1")
                                 (create-minion "Imp" :id "m2")]}]
                     :minion-ids-summoned-this-turn
                     [])
        (damage-entity "m1" 1)
        (damage-entity "m2" 1)
        (get-attack "a100"))
    4)
  ; when a hero is attacked he should not get attack
  (is= (-> (create-game [{:minions
                            [(create-minion "Frothing Berserker" :id "a100")]}]
                        :minion-ids-summoned-this-turn
                        [])
           (attack-entity "a100" "h1")
           (get-attack "a100"))
       2))

(clojure.test/deftest sneeds-old-shredder
  (as-> (create-game [{:minions
                         [(create-minion "Sneed's Old Shredder" :id "a100")]}]
                     :seed
                     123) $
    (destroy-entity $ "a100")
    (let [minions (get-minions $ "p1")]
      (do ; p1 does not have a minion with id a100 anymore
          (is= (count (filter #(= (:id %) "a100") minions)) 0)
          ; we have one legendary minion
          (is= (count (filter #(= (:rarity %) :legendary)
                        (map #(get-definition (:name %)) minions)))
               1)))))

; TODO test that only the one who owns anton should get a fireball
(clojure.test/deftest archmage-antonidas
  (do (as-> (create-game [{:minions [(create-minion "Archmage Antonidas")],
                           :hand [(create-card "Frostbolt" :id "c1")]}]) $
        (play-spell-card $ "p1" "c1" "h2")
        (do ; p1 has one card in hand
            (is= (count (get-hand $ "p1")) 1)
            ; and its a fireball
            (is= (:name (first (get-hand $ "p1"))) "Fireball")))
      (as-> (create-game [{:minions [(create-minion "Archmage Antonidas")],
                           :hand [(create-card "Frostbolt" :id "c1")
                                  (create-card "Fireball" :id "c2")
                                  (create-card "Gallywix's Coin" :id "c3")]}]) $
        (play-spell-card $ "p1" "c1" "h2")
        (play-spell-card $ "p1" "c2" "h2")
        (play-spell-card $ "p1" "c3")
        (do ; p1 has three cards in hand
            (is= (count (get-hand $ "p1")) 3)
            ; and they're all fireballs
            (is= (count (filter #(= (:name %) "Fireball") (get-hand $ "p1")))
                 3)))))

(clojure.test/deftest trade-prince-gallywix
  (do
    (as-> (create-game [{:minions [(create-minion "Trade Prince Gallywix")]}
                        {:mana 2, :hand [(create-card "Frostbolt" :id "c1")]}]) $
      (play-spell-card $ "p2" "c1" "h1")
      (do ; p1 has one card in hand
          (is= (count (get-hand $ "p1")) 1)
          ; and its a copy of the frostbolt
          (is= (:name (first (get-hand $ "p1"))) "Frostbolt")
          ; p2 has one card in hand
          (is= (count (get-hand $ "p2")) 1)
          ; and its a gallywix coin
          (is= (:name (first (get-hand $ "p2"))) "Gallywix's Coin")
          ; Check that player 2s mana is 0 from playing frost bolt
          (is= (get-mana $ "p2") 0)
          (as-> (let [coin-id (:id (first (get-hand $ "p2")))]
                  ; Player two plays the coin they just got
                  (play-spell-card $ "p2" coin-id)) $
            (do ; Should now have one mana
                (is= (get-mana $ "p2") 1)
                ; Player one should still only have one card
                (is= (count (get-hand $ "p1")) 1)
                ; That one card should still be a frostbolt
                (is= (:name (first (get-hand $ "p1"))) "Frostbolt")))))))

(clojure.test/deftest moroes
  (as-> (create-game [{:minions [(create-minion "Moroes" :id "m1")]}]) $
    (end-turn $)
    (do ; p1 now has two minions
        (is= (count (get-minions $ "p1")) 2)
        ; one of them is the Moroes
        (is= (count (filter #(= (:name %) "Moroes") (get-minions $ "p1"))) 1)
        ; the other is a Steward
        (is= (count (filter #(= (:name %) "Steward") (get-minions $ "p1"))) 1)
        ; Moroes has stealth
        (is (stealth? $ "m1"))
        $)
    (end-turn $)
    (do ; p1 still has two minions
        (is= (count (get-minions $ "p1")) 2)
        ; one of them is still the Moroes
        (is= (count (filter #(= (:name %) "Moroes") (get-minions $ "p1"))) 1)
        ; the other is still a Steward
        (is= (count (filter #(= (:name %) "Steward") (get-minions $ "p1"))) 1)
        ; Moroes still has stealth
        (is (stealth? $ "m1"))
        $)
    (end-turn $)
    (do ; p1 now has three minions
        (is= (count (get-minions $ "p1")) 3)
        ; one of them is still the Moroes
        (is= (count (filter #(= (:name %) "Moroes") (get-minions $ "p1"))) 1)
        ; the others are Stewards
        (is= (count (filter #(= (:name %) "Steward") (get-minions $ "p1")))
             2))))

(clojure.test/deftest alarm-o-bot
  ; having exactly one available card in hand Alarm-o-Bot should switch with
  ; that card
  (as-> (create-game [{:hand [(create-card "Imp" :id "i1")],
                       :minions [(create-minion "Alarm-o-Bot" :id "a1")]}]) $
    (end-turn $)
    (end-turn $)
    (do (is= (:name (first (get-hand $ "p1"))) "Alarm-o-Bot")
        (is= (count (get-minions $ "p1")) 1)
        (is= (count (filter #(= (:name %) "Imp") (get-minions $ "p1"))) 1)))
  ; having only a spell card in hand Alarm-o-Bot should do nothing
  (as-> (create-game [{:hand [(create-card "Fireball" :id "f1")],
                       :minions [(create-minion "Alarm-o-Bot" :id "a1")]}]) $
    (end-turn $)
    (end-turn $)
    (do (is= (:name (first (get-hand $ "p1"))) "Fireball")
        (is= (count (get-minions $ "p1")) 1)
        (is= (count (filter #(= (:name %) "Alarm-o-Bot") (get-minions $ "p1")))
             1))))

(clojure.test/deftest blood-imp
  ; having exactly one available friendly minion should buff that minion
  (as-> (create-game [{:minions [(create-minion "Blood Imp" :id "b1")
                                 (create-minion "Imp" :id "i1")]}
                      {:minions [(create-minion "Imp" :id "i2")]}]) $
    ; initially nothing is buffed
    (do (is= (count (get-in (get-minion $ "b1") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "i1") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "i2") [:states :buffs])) 0)
        $)
    ; p1 ends the turn triggering b1 -> i1 should be buffed
    (end-turn $)
    (do (is= (count (get-in (get-minion $ "b1") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "i1") [:states :buffs])) 1)
        (is= (count (get-in (get-minion $ "i2") [:states :buffs])) 0)
        $))
  ; having many available friendly minions should buff exactly one of them
  (as-> (create-game [{:minions [(create-minion "Steward" :id "m1")
                                 (create-minion "Imp" :id "m2")]}
                      {:minions [(create-minion "Blood Imp" :id "m3")
                                 (create-minion "Defender" :id "m4")
                                 (create-minion "Dalaran Mage" :id "m5")]}]) $
    ; initially there are 0 active buffs
    (do (is= (reduce (fn [sum minion]
                       (+ sum (count (get-in minion [:states :buffs]))))
               0
               (get-minions $))
             0)
        $)
    ; p1 ends the turn but has no blood imp, so there are still no buffs
    (end-turn $)
    (do (is= (reduce (fn [sum minion]
                       (+ sum (count (get-in minion [:states :buffs]))))
               0
               (get-minions $))
             0)
        $)
    ; p2 ends the turn triggering m3 -> exactly one of {m4, m5} should be buffed
    (end-turn $)
    (do
      (is= (reduce (fn [sum minion]
                     (+ sum (count (get-in minion [:states :buffs]))))
             0
             (get-minions $))
           1)
      (is (or (= (count (get-in (get-minion $ "m4") [:states :buffs])) 1)
              (= (count (get-in (get-minion $ "m5") [:states :buffs])) 1)))
      (is-not (and (= (count (get-in (get-minion $ "m4") [:states :buffs])) 1)
                   (= (count (get-in (get-minion $ "m5") [:states :buffs])) 1)))
      $))
  ; having no available friendly minions should do nothing
  (as-> (create-game [{:minions [(create-minion "Blood Imp" :id "b1")]}
                      {:minions [(create-minion "Imp" :id "i1")]}]) $
    (do (is= (count (get-in (get-minion $ "b1") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "i1") [:states :buffs])) 0)
        $)
    (end-turn $)
    (do (is= (count (get-in (get-minion $ "b1") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "i1") [:states :buffs])) 0)
        $)))


(clojure.test/deftest doomsayer
  (as->
    (create-game [{:hand [(create-card "Doomsayer" :id "ds1")],
                   :minions [(create-minion "Imp" :id "m1")
                             (create-minion "Silver Hand Recruit" :id "m2")
                             (create-minion "Dalaran Mage" :id "m3")]}
                  {:minions [(create-minion "Blood Imp" :id "m4")
                             (create-minion "Malygos" :id "m5")
                             (create-minion "Steward" :id "m6")]}]) $
    (play-minion-card $ "p1" "ds1" 0)
    (end-turn $)
    ; doomsayer is not triggered yet
    (do (is= (count (get-minions $)) 7) $)
    (end-turn $)
    ; doomsayer is triggered on start of turn destroying all entities
    (do (is= (count (get-minions $)) 0) $)))


(clojure.test/deftest lorewalker-cho
  (as-> (create-game [{:minions [(create-minion "Lorewalker Cho" :id "lc1")],
                       :hand [(create-card "Fireball")]}]) $
    (do (is= (count (get-hand $ "p1")) 1) (is= (count (get-hand $ "p2")) 0) $)
    (play-spell-card $ "p1" (:id (first (get-hand $ "p1"))) "h2")
    (do (is= (count (get-hand $ "p1")) 0)
        (is= (count (get-hand $ "p2")) 1)
        (is= (:name (first (get-hand $ "p2"))) "Fireball")
        $)
    (play-spell-card $ "p2" (:id (first (get-hand $ "p2"))) "h1")
    (do (is= (count (get-hand $ "p1")) 1)
        (is= (count (get-hand $ "p2")) 0)
        (is= (:name (first (get-hand $ "p1"))) "Fireball")
        $)))


(clojure.test/deftest competitive-spirit
  (as-> (create-game [{:minions [(create-minion "Imp" :id "m1")
                                 (create-minion "Steward" :id "m2")],
                       :hand [(create-card "Competitive Spirit" :id "cs1")]}
                      {:minions [(create-minion "Imp" :id "m3")
                                 (create-minion "Steward" :id "m4")]}]) $
    (do (is= (count (get-secrets $)) 0) $)
    (play-spell-card $ "p1" "cs1")
    (do (is= (count (get-secrets $)) 1)
        (is= (count (get-in (get-minion $ "m1") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m2") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m3") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m4") [:states :buffs])) 0)
        $)
    (end-turn $) ; p1 ends turn
    (do (is= (count (get-secrets $)) 1)
        (is= (count (get-in (get-minion $ "m1") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m2") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m3") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m4") [:states :buffs])) 0)
        $)
    (end-turn $) ; p2 ends turn, should trigger comp. spirit
    (do (is= (count (get-secrets $)) 0)
        (is= (count (get-in (get-minion $ "m1") [:states :buffs])) 1)
        (is= (get-buff (get-minion $ "m1") :max-health-buff) 1)
        (is= (get-buff (get-minion $ "m1") :attack-buff) 1)
        (is= (count (get-in (get-minion $ "m2") [:states :buffs])) 1)
        (is= (get-buff (get-minion $ "m2") :max-health-buff) 1)
        (is= (get-buff (get-minion $ "m2") :attack-buff) 1)
        (is= (count (get-in (get-minion $ "m3") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m4") [:states :buffs])) 0)
        $)
    (end-turn $) ; p1 ends turn
    (do (is= (count (get-in (get-minion $ "m1") [:states :buffs])) 1)
        (is= (count (get-in (get-minion $ "m2") [:states :buffs])) 1)
        (is= (count (get-in (get-minion $ "m3") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m4") [:states :buffs])) 0)
        $)
    (end-turn $) ; p2 ends turn
    (do (is= (count (get-in (get-minion $ "m1") [:states :buffs])) 1)
        (is= (count (get-in (get-minion $ "m2") [:states :buffs])) 1)
        (is= (count (get-in (get-minion $ "m3") [:states :buffs])) 0)
        (is= (count (get-in (get-minion $ "m4") [:states :buffs])) 0)
        $)))
