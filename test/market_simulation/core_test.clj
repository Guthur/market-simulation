(ns market-simulation.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]            
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as tcct]
            [market-simulation.core :refer :all]))

(def number-of-check-runs 1000)

(comment "Check the creation of an order. Checking that price and
quantity are correct and that the time is a DataTime object")
(tcct/defspec generate-order
  number-of-check-runs
  (prop/for-all [generated-price gen/int
                 generated-quantity gen/int]
                (let [order (make-order generated-price generated-quantity)]
                  (and (= generated-price (:price order))
                       (= generated-quantity (:quantity order))
                       (= org.joda.time.DateTime (type (:time order)))))))

(defn make-price-comparator-prop
  "Create check property to check the price ordering for
  `make-order-comparator' using PRICE-COMPARATOR. Property ignores
  when prices are equal due to non-determinism of order time which is
  in the secondary comparision when prices are equal."
  [price-comparator]
  (prop/for-all [price-x gen/int
                 quantity-x gen/int
                 price-y gen/int
                 quantity-y gen/int]
                (let [order-x (make-order price-x quantity-x)
                      order-y (make-order price-y quantity-y)]
                  (or (= ((make-order-comparator price-comparator) order-x order-y)
                         (price-comparator price-x price-y))
                      (= price-x price-y)))))

(comment "Check price is greater order comparision")
(tcct/defspec price-greater-order-comparator
  number-of-check-runs
  (make-price-comparator-prop >))

(comment "Check price is less than order comparision")
(tcct/defspec price-less-than-order-comparator
  number-of-check-runs
  (make-price-comparator-prop <))

(defn make-add-orders-check-prop 
  "Create a check property for adding orders. The function to add the
  particular order type is passed via ADD-ORDER-FN. The order accessor
  function is passed via ORDER-LIST-ACCESSOR, the price comparator is
  passed via PRICE-COMPARISON."
  [add-order-fn order-list-accessor price-comparison]
  (prop/for-all [prices-and-quantities (gen/vector (gen/vector gen/pos-int 2))]
                (let [order-book (reduce (fn [book order-values]
                                           (add-order-fn book (apply make-order order-values)))
                                         (make-order-book) 
                                         prices-and-quantities)]
                  (and (= (count prices-and-quantities) (count (order-list-accessor order-book)))
                       (= (sort price-comparison (map first prices-and-quantities))
                          (map :price (order-list-accessor order-book)))))))

(comment "Check the addition of buy orders to an order-book")
(tcct/defspec add-buy-orders
  number-of-check-runs
  (make-add-orders-check-prop add-buy-order :buy-orders >))

(comment "Check the addition of sell orders to an order-book")
(tcct/defspec add-sell-orders
  number-of-check-runs
  (make-add-orders-check-prop add-sell-order :sell-orders <))

(defn make-orders-generator
  "Return a generator that produces and non-empty list of orders"
  []
  (gen/such-that not-empty (gen/vector (gen/fmap (partial apply make-order) 
                                                 (gen/tuple gen/pos-int gen/pos-int)))))

(defn make-order-book-generator
  "Return an order book that has orders added using ADD-ORDER-FN"
  [add-order-fn]
  (gen/fmap (partial reduce add-order-fn (make-order-book)) (make-orders-generator)))

(comment "Check getting the top sell price of an order book")
(tcct/defspec check-get-top-sell-price
  number-of-check-runs
  (prop/for-all [order-book (make-order-book-generator add-sell-order)]
                (let [top-sell-price (reduce min (map :price (:sell-orders order-book)))]
                  (= (get-top-sell-price order-book)
                     top-sell-price))))

(comment "Check getting the top buy price of an order book")
(tcct/defspec check-get-top-buy-price
  number-of-check-runs
  (prop/for-all [order-book (make-order-book-generator add-buy-order)]
                (let [top-buy-price (reduce max (map :price (:buy-orders order-book)))]
                  (= (get-top-buy-price order-book)
                     top-buy-price))))

(comment "Check getting the total quantity of all buy orders within an order book")
(tcct/defspec check-get-buy-list-quantity
  number-of-check-runs
  (prop/for-all [order-book (make-order-book-generator add-buy-order)]
                (let [total-quantity (reduce + (map :quantity (:buy-orders order-book)))]
                  (= (get-buy-list-quantity order-book)
                     total-quantity))))

(comment "Check getting the total quantity of all sell orders within an order book")
(tcct/defspec check-get-sell-list-quantity
  number-of-check-runs
  (prop/for-all [order-book (make-order-book-generator add-sell-order)]
                (let [total-quantity (reduce + 0 (map :quantity (:sell-orders order-book)))]
                  (= (get-sell-list-quantity order-book)
                     total-quantity))))
