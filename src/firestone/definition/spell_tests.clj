(ns firestone.definition.spell-tests
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [clojure.pprint :refer [pprint]]
            [firestone.spec :as fspec]
            [clojure.spec.alpha :as spec]
            [firestone.api :refer [end-turn attack-entity play-spell-card]]
            [firestone.damage-entity :refer [damage-entity destroy-entity]]
            [firestone.info :refer :all]
            [firestone.construct :refer
             [create-game create-minion create-card create-hero
              create-secret]]))

(clojure.test/deftest rampage
  ; Should not be able to target a minion that is not damaged
  (as-> (create-game [{:hand [(create-card "Rampage" :id "r1")],
                       :minions [(create-minion "War Golem" :id "wg1")]}
                      {:minions [(create-minion "Imp" :id "i1")]}]) state
    (do (error? (play-spell-card state "p1" "r1" "wg1")) state)
    (attack-entity state "wg1" "i1")
    ; Now it should be possible to buff the card
    (play-spell-card state "p1" "r1" "wg1")
    (do ; 7 + 3 = 10
        (is= (get-attack state "wg1") 10)
        ; 7 + 3 - 1 = 9
        (is= (get-health state "wg1") 9))))

(clojure.test/deftest coin
  ; Should give +1 mana
  (is= (-> (create-game [{:mana 1, :hand [(create-card "The Coin" :id "c1")]}])
           (play-spell-card "p1" "c1")
           (get-mana "p1"))
       2)
  ; Having 10 mana and playing the coin should have no effect on the mana
  (is= (-> (create-game [{:mana 10, :hand [(create-card "The Coin" :id "c2")]}])
           (play-spell-card "p1" "c2")
           (get-mana "p1"))
       10)
  ; Mana should disappear on the next turn
  (as-> (create-game
          [{:mana 1, :max-mana 1, :hand [(create-card "The Coin" :id "c1")]}]) $
    (play-spell-card $ "p1" "c1")
    (do (is= (get-mana $ "p1") 2)
        (as-> (end-turn $) $
          (end-turn $)
          (do (is= (get-max-mana $ "p1") 2) (is= (get-mana $ "p1") 2))))))

(clojure.test/deftest mind-control
  (as-> (create-game [{:hand [(create-card "Mind Control" :id "mc1")]}
                      {:minions [(create-minion "Imp" :id "i1" :attack 4)
                                 (create-minion "Imp" :id "i2")]}]) $
    (play-spell-card $ "p1" "mc1" "i1")
    (do (is= (:id (first (get-minions $ "p1"))) "i1")
        (is= (:attack (first (get-minions $ "p1"))) 4)
        (is= (count (get-minions $ "p1")) 1)
        (is= (:id (first (get-minions $ "p2"))) "i2")
        (is= (count (get-minions $ "p2")) 1))))


(clojure.test/deftest bananas
  (as-> (create-game
          [{:hand [(create-card "Bananas" :id "b1")
                   (create-card "Bananas" :id "b2")]}
           {:minions [(create-minion "War Golem" :id "wg1" :health 6)]}]) $
    (play-spell-card $ "p1" "b1" "wg1")
    (do (is= (get-health $ "wg1") 7) (is= (get-attack $ "wg1") 8))))


(clojure.test/deftest frostbolt
  ; Test if heroes get frozen
  (is (-> (create-game [{:hand [(create-card "Frostbolt" :id "f1")]}])
          (play-spell-card "p1" "f1" "h2")
          (frozen? "h2")))
  ; Test if a minion gets frozen
  (as-> (create-game
          [{:hand [(create-card "Frostbolt" :id "f1")
                   (create-card "Frostbolt" :id "f2")]}
           {:minions [(create-minion "War Golem" :id "wg1" :health 6)]}]) $
    (play-spell-card $ "p1" "f1" "wg1")
    (do (is= (get-health $ "wg1") 3)
        (is= (frozen? $ "wg1") true)
        (as-> (play-spell-card $ "p1" "f2" "wg1") $
          (is= (count (get-minions $ "p2")) 0)))))

(clojure.test/deftest fireball
  (as-> (create-game
          [{:hand [(create-card "Fireball" :id "f1")
                   (create-card "Fireball" :id "f2")]}
           {:minions [(create-minion "War Golem" :id "wg1" :health 7)]}]) $
    (play-spell-card $ "p1" "f1" "wg1")
    (do (is= (get-health $ "wg1") 1)
        (as-> (play-spell-card $ "p1" "f2" "wg1") $
          (is= (count (get-minions $ "p2")) 0)))))

(clojure.test/deftest snake-trap
  (as-> (create-game [{:hand [(create-card "Snake Trap" :id "st1")],
                       :minions [(create-minion "Imp" :id "i1")]}
                      {:minions [(create-minion "Imp" :id "i2")]}]) $
    (play-spell-card $ "p1" "st1")
    (do (is= (count (get-secrets $)) 1) $)
    (end-turn $)
    (attack-entity $ "i2" "i1")
    (do (is= (count (get-minions $)) 3)
        (is= (:name (first (get-minions $))) "Snake")
        (is= [] (get-secrets $))))
  (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")],
                       :secrets [(create-secret "Snake Trap" :id "st1")]}
                      {:minions [(create-minion "Imp" :id "i2")]}]
                     :player-in-turn
                     "p2") $
    (end-turn $)
    (attack-entity $ "i2" "i1")
    (do (is= (count (get-minions $)) 3)
        (is= (:name (first (get-minions $))) "Snake")
        (is= [] (get-secrets $)))))


(clojure.test/deftest flare
  (as->
    (create-game [{:minions [(create-minion "Blood Imp" :id "i1")],
                   :secrets [(create-secret "Snake Trap" :id "st1")],
                   :deck [(create-card "Imp")],
                   :hand [(create-card "Flare" :id "f1")]}
                  {:minions [(create-minion "Blood Imp" :id "i2")],
                   :secrets [(create-secret "Snake Trap" :id "st2")]}]) $
    (do (is= (stealth? $ "i1") true)
        (is= (stealth? $ "i2") true)
        (is= (count (get-secrets $)) 2)
        (is= (count (get-secrets $ "p1")) 1)
        (is= (count (get-secrets $ "p2")) 1)
        (is= (count (get-hand $ "p1")) 1)
        (is= (:name (first (get-hand $ "p1"))) "Flare")
        $)
    (play-spell-card $ "p1" "f1")
    (do (is= (stealth? $ "i1") false)
        (is= (stealth? $ "i2") false)
        (is= (count (get-secrets $)) 1)
        (is= (count (get-secrets $ "p1")) 1)
        (is= (count (get-secrets $ "p2")) 0)
        (is= (count (get-hand $ "p1")) 1)
        (is= (:name (first (get-hand $ "p1"))) "Imp")
        $)))
