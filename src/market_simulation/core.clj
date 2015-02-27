(ns market-simulation.core
  (:gen-class)
  (:require [clj-time.core :as t]))

(defn make-order
  "Make an order of QUANTITY at PRICE."
  ([price quantity]
   (make-order price quantity (t/now) (System/nanoTime)))
  ([price quantity time timestamp]
   {:price price :quantity quantity :time time :timestamp timestamp}))

(defn make-order-book
  "Make an order book. If 0 arguments are passed the order book is
  empty. Buy and sell orders can be passed via BUYS and SELLS
  respectively"
  ([] (make-order-book () ()))
  ([buys sells]
   {:buy-orders buys :sell-orders sells}))

(defn order-before?
  "Test if ORDER-A timestamp comes before ORDER-B"
  [order-a order-b]
  (< (- (:timestamp order-a) (:timestamp order-b)) 0))

(defn make-order-comparator
  "Return a function that compares two orders using the
  PRICE-COMPARATOR for price and order timestamp to determine eldest
  order"
  [price-comparator]
  (fn [order-a order-b]
    (or (price-comparator (:price order-a) (:price order-b))
        (and (= (:price order-a) (:price order-b))
             (order-before? order-a order-b)))))

(defn sort-order-list
  "Add ORDER to ORDER-LIST maintain list order based on PRICE-COMPARATOR"
  [order order-list price-comparator]
  (let [comparator (make-order-comparator price-comparator)
        insert-order (fn insert-order [order order-list]
                       (cond
                         ;; Empty order list create new one
                         (empty? order-list) (list order)
                         ;; Order should appear before the head of
                         ;; order list
                         (comparator order (first order-list)) (cons order order-list)
                         ;; Recurse through the rest of the list
                         :else (cons (first order-list)
                                     (insert-order order (rest order-list)))))]
    (insert-order order order-list)))

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
  (reduce + (map :quantity order-list)))

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
  (loop [rec-order order 
         rec-order-list order-list]
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
        (recur (make-order (:price order)
                           (- (:quantity order)
                              (:quantity top-of-book))
                           (:time order)
                           (:timestamp order))
               (rest order-list))
        ;; Price matches and ORDER quantity less than top-of-book,
        ;; remove ORDER quantity from top-of-book quantity returning new
        ;; ORDER-LIST
        :else
        [nil (cons (make-order (:price top-of-book)
                               (- (:quantity top-of-book)
                                  (:quantity order))
                               (:time top-of-book)
                               (:timestamp order))
                   (rest order-list))]))))

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

(defn -main
  "Print out sample order book"
  [& args]
  (println "Simulation goes here!"))
