(ns market-simulation.core
  (:gen-class)
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clj-time.core :as t]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn make-order 
  "Make an order of QUANTITY at PRICE."
  ([price quantity]
   (make-order price quantity (t/now)))
  ([price quantity time]
   {:price price :quantity quantity :time time}))

(defn make-order-book 
  "Make an order book. If 0 arguments are passed the order book is
  empty. Buy and sell orders can be passed via BUYS and SELLS
  respectively"
  ([] (make-order-book () ()))
  ([buys sells]
   {:buy-orders buys :sell-orders sells}))

(defn make-order-comparator
  "Return a function that compares two orders using the
  PRICE-COMPARATOR for price and `t/before?' for order time"
  [price-comparator]
  (fn [x y]    
    (or (price-comparator (:price x) (:price y))
        (and (= (:price x) (:price y)) (t/before? (:time x) (:time y))))))

(defn sort-order-list 
  "Add ORDER to ORDER-LIST and sort order using PRICE-COMPARATOR"
  [order order-list price-comparator]
  (sort (make-order-comparator price-comparator) (cons order order-list)))

(defn add-buy-order 
  "Add ORDER to ORDER-BOOK buy orders sorted list returning the new
  order book. The order list is sorted in descending price order and
  most recent"
  [order-book order]
  (make-order-book (sort-order-list order (:buy-orders order-book) >)
                   (:sell-orders order-book)))

(defn add-sell-order 
  "Add ORDER to ORDER-BOOK sell orders sorted list returning the new
  order book. The order list is sorted in ascending price order and
  most recent"
  [order-book order]
  (make-order-book (:buy-orders order-book)
                   (sort-order-list order (:sell-orders order-book) <)))

(defn get-order-list-top-price 
  "Get the price of the top order in ORDER-LIST"
  [order-list]
  (:price (first order-list)))

(defn get-top-sell-price 
  "Get top sell price in ORDER-BOOK"
  [order-book]
  (get-order-list-top-price (:sell-orders order-book)))

(defn get-top-buy-price
  "Get top buy price in ORDER-BOOK"
  [order-book]
  (get-order-list-top-price (:buy-orders order-book)))

(defn get-order-list-quantity
  "Get the quantity sum of all the orders in ORDER-LIST"
  [order-list]
  (reduce (fn [count order] (+ count (:quantity order))) 0 order-list))

(defn get-sell-list-quantity
  "Get the quantity sum of all the sell orders in ORDER-BOOK"
  [order-book]
  (get-order-list-quantity (:sell-orders order-book)))

(defn get-buy-list-quantity 
  "Get the quantity sum of all the buy orders in ORDER-BOOK"
  [order-book]
  (get-order-list-quantity (:buy-orders order-book)))

(defn fill-order [order order-list matching-price-comparator]
  "Recurse through ORDER-LIST searching for matching prices against
  the ORDER using MATCHING-PRICE-COMPARATOR. Returning remaining ORDER
  and ORDER-LIST"
  (let [top-of-book (first order-list)]
    (cond
      ;; Order has zero quantity return remaining ORDER-LIST
      (zero? (:quantity order))
      [nil order-list]
      ;; Empty ORDER-LIST return ORDER and ORDER-LIST
      (empty? order-list)
      [order order-list]
      ;; Price can not close on any existing orders in ORDER-LIST
      ;; return ORDER-LIST and ORDER
      (not (matching-price-comparator (:price order) (:price top-of-book)))
      [order order-list]
      ;; Price matches. If quantity of ORDER is greater or equal to
      ;; top-of-book then remove top-of-book quantity from ORDER and
      ;; recursively call `FILL-ORDER with the remaining ORDER
      ;; quantity and remaining ORDER-LIST'
      (>= (:quantity order) (:quantity top-of-book))
      (fill-order (make-order (:price order)
                              (- (:quantity order)
                                 (:quantity top-of-book))
                              (:time order))
                  (rest order-list)
                  matching-price-comparator)
      ;; Price matches and ORDER quantity less than top-of-book,
      ;; remove ORDER quantity from top-of-book quantity returning new
      ;; ORDER-LIST
      :else
      [nil (cons (make-order (:price top-of-book)
                             (- (:quantity top-of-book)
                                (:quantity order))
                             (:time top-of-book))
                 (rest order-list))])))

(defn match-sell-order [order order-book]
  "Tries to match sell ORDER against any buy orders in ORDER-BOOK when
  the sell price is less than or equal to the buy price. Any
  unfulfilled sell ORDER is added to the ORDER-BOOK"
  (let [[order order-list] (fill-order order (:buy-orders order-book) <=)]
    (if (= nil order)
      (make-order-book order-list (:sell-orders order-book))
      (add-sell-order (make-order-book order-list (:sell-orders order-book)) order))))

(defn match-buy-order [order order-book]
  "Tries to match buy ORDER against any sell orders in ORDER-BOOK when
  the buy price is greater than or equal to the sell price. Any
  unfulfilled buy ORDER is added to the ORDER-BOOK"
  (let [[order order-list] (fill-order order (:sell-orders order-book) >=)]
    (if (= nil order)
      (make-order-book (:buy-orders order-book) order-list)
      (add-buy-order (make-order-book (:buy-orders order-book) order-list) order))))
