(ns firestone.utils
  (:require [ysera.test :refer [is is-not is= error?]]
            [clojure.test :refer [function?]]
            [clojure.pprint :refer [pprint]]))

(defn remove-first
  "Remove first occurence of element or when function is true. The function must will take one element and return non-false/nil when the element is found."
  {:test (fn []
           (is= (remove-first [1 2 3 4 5 6 5] 5) [1 2 3 4 6 5])
           (is= (remove-first '(1 2 3 4 5 6 5) 5) '(1 2 3 4 6 5))
           (is= (remove-first '(1 2 3 4 5 6 5) 7 false) nil)
           (is= (remove-first '(1 2 3 4 5 6 5) 7 true) '(1 2 3 4 5 6 5)))}
  ([l element-or-function return-list-if-nothing-found]
   (let [func (if (function? element-or-function)
                element-or-function
                (fn [element] (= element-or-function element)))]
     (loop [seen (empty l)
            left l]
       (cond (empty? left)
               ; If no one is found, return the list
               (if return-list-if-nothing-found l nil)
             (func (first left)) (concat (if (list? l) (reverse seen) seen)
                                         (rest left))
             :else (recur (conj seen (first left)) (rest left))))))
  ([l element-or-function] (remove-first l element-or-function true)))
