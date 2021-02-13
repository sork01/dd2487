(ns firestone.integration-tests
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [clojure.pprint :refer [pprint]]
            [firestone.api :refer
             [attack-entity play-minion-card play-spell-card end-turn]]
            [firestone.info :refer [get-minions get-entity]]
            [firestone.damage-entity :refer [destroy-entity]]
            [firestone.construct :refer [create-game create-card]]))


(clojure.test/deftest positioning
  (as-> (create-game [{:hand [(create-card "Imp" :id "i0")
                              (create-card "Imp" :id "i1")
                              (create-card "Imp" :id "i2")]}]) state
    (play-minion-card state "p1" "i0" 0)
    (play-minion-card state "p1" "i1" 1)
    (play-minion-card state "p1" "i2" 2)
    (do ; Their positions should be as they were played.
        (is= (:position (get-entity state "i0")) 0)
        (is= (:position (get-entity state "i1")) 1)
        (is= (:position (get-entity state "i2")) 2)
        ; Their positions should shift as expected
        (as-> (destroy-entity state "i0") state
          (do (is= (:position (get-entity state "i1")) 0)
              (is= (:position (get-entity state "i2")) 1)))
        (as-> (destroy-entity state "i1") state
          (do (is= (:position (get-entity state "i0")) 0)
              (is= (:position (get-entity state "i2")) 1)))
        (as-> (destroy-entity state "i2") state
          (do (is= (:position (get-entity state "i0")) 0)
              (is= (:position (get-entity state "i1")) 1))))))


(clojure.test/deftest play-order
  ; sylvanas played before sneed, double knockout does not result in sylvanas
  ; stealing the minion
  ; spawned by sneed since sylvanas deathrattle takes place before sneeds
  ; deathrattle
  (as-> (create-game
          [{:hand [(create-card "Sylvanas Windrunner" :id "sylvanas") ; "Deathrattle:
                                                                      ; Take
                                                                      ; control
                                                                      ; of a
                                                                      ; random
                                                                      ; enemy
                                                                      ; minion.",
                   (create-card "Fireball" :id "fireball")]}
           {:hand [(create-card "Sneed's Old Shredder" :id "sneed")]}]) $ ; "Deathrattle:
                                                                          ; Summon
                                                                          ; a
                                                                          ; random
                                                                          ; Legendary
                                                                          ; minion.",
    (play-minion-card $ "p1" "sylvanas" 0)
    ; p1 ends turn
    (end-turn $)
    (play-minion-card $ "p2" "sneed" 0)
    ; p2 ends turn
    (end-turn $)
    (play-spell-card $ "p1" "fireball" "sneed")
    ; p1 ends turn
    (end-turn $)
    (attack-entity $ "sneed" "sylvanas")
    (do (is= (count (get-minions $ "p1")) 0)
        (is= (count (get-minions $ "p2")) 1)
        $))
  ; sneed played before sylvanas, double knockout results in sylvanas stealing
  ; the minion
  ; spawned by sneed since sneeds deathrattle takes place before sylvanas
  ; deathrattle
  (as-> (create-game
          [{:hand [(create-card "Sylvanas Windrunner" :id "sylvanas") ; "Deathrattle:
                                                                      ; Take
                                                                      ; control
                                                                      ; of a
                                                                      ; random
                                                                      ; enemy
                                                                      ; minion.",
                   (create-card "Fireball" :id "fireball")]}
           {:hand [(create-card "Sneed's Old Shredder" :id "sneed")]}]) $ ; "Deathrattle:
                                                                          ; Summon
                                                                          ; a
                                                                          ; random
                                                                          ; Legendary
                                                                          ; minion.",
    ; p1 ends turn
    (end-turn $)
    (play-minion-card $ "p2" "sneed" 0)
    ; p2 ends turn
    (end-turn $)
    (play-minion-card $ "p1" "sylvanas" 0)
    (play-spell-card $ "p1" "fireball" "sneed")
    ; p1 ends turn
    (end-turn $)
    (attack-entity $ "sneed" "sylvanas")
    (do (is= (count (get-minions $ "p1")) 1)
        (is= (count (get-minions $ "p2")) 0)
        $)))
