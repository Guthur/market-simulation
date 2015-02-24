(ns my-app.core
  (:gen-class)
  (:require [clojure.core.reducers :as r]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clj-time.core :as t]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn make-order 
  "Make an order of QUANTITY at PRICE."
  [price quantity]
  (list price quantity (t/now)))

(defn order-price 
  "Get ORDER price"
  [order]
  (first order))

(defn order-quantity 
  "Get ORDER quantity"
  [order]
  (second order))

(defn order-time [order]
  "Get ORDER time"
  (nth order 2))

(defn make-order-book 
  "Make an order book. If 0 arguments are passed the order book is
  empty. Buy and sell orders can be passed via BUYS and SELLS
  respectively"
  ([] (list nil nil))
  ([buys sells]
   (list buys sells)))

(defn sell-orders 
  "Get sell orders for ORDER-BOOK"
  [order-book]
  (second order-book))

(defn buy-orders 
  "Get buy orders for ORDER-BOOK"
  [order-book]
  (first order-book))

(defn make-order-comparator
  "Return a function that compares two orders using the
  PRICE-COMPARATOR for price and `t/before?' for order time"
  [price-comparator]
  (fn [x y]
    (or (price-comparator (order-price x) (order-price y))
        (and (= (order-price x) (order-price y)) (t/before? (order-time x) (order-time y))))))

(defn sort-order-list 
  "Add ORDER to ORDER-LIST and sort order using PRICE-COMPARATOR"
  [order order-list price-comparator]
  (sort (make-order-comparator price-comparator) (cons order order-list)))

(defn add-buy-order 
  "Add ORDER to ORDER-BOOK buy orders sorted list returning the new
  order book. The order list is sorted in descending price order and
  most recent"
  [order-book order]
  (make-order-book (sort-order-list order (buy-orders order-book) >)
                   (sell-orders order-book)))

(defn add-sell-order 
  "Add ORDER to ORDER-BOOK sell orders sorted list returning the new
  order book. The order list is sorted in ascending price order and
  most recent"
  [order-book order]
  (make-order-book (buy-orders order-book)
                   (sort-order-list order (sell-orders order-book) <)))
