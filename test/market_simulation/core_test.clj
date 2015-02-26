(ns market-simulation.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as tcct]
            [market-simulation.core :refer :all]))

(def number-of-check-runs 1000)

(defn make-order-generator
  "Return a generator that produces an order using
  ORDER-VALUE-GENERATOR or if no ORDER-VALUE-GENERATOR is passed then
  return a generate for an order with random price and quantity"
  ([]
   (make-order-generator (gen/tuple gen/pos-int gen/pos-int)))
  ([order-value-generator]
   (gen/fmap (partial apply make-order)
             order-value-generator)))

(defn make-list-of-orders-generator
  "Return a generator that produces a non-empty list of orders"
  [order-generator]
  (gen/such-that not-empty (gen/vector order-generator)))

(defn make-list-of-random-orders-generator
  "Return a generator that produces a list of orders with random price and quantity"
  []
  (make-list-of-orders-generator (make-order-generator)))

(defn make-order-book-generator
  "Return an order book that has orders added using ADD-ORDER-FN"
  [add-order-fn]
  (gen/fmap (partial reduce add-order-fn (make-order-book)) (make-list-of-random-orders-generator)))

(comment "Check the creation of an order. Checking that price and
quantity are correct and that the time is a DataTime object and
timestamp is a Long.
Also check that explicit order creation produces a valid copy of order")
(tcct/defspec generate-order
  number-of-check-runs
  (prop/for-all [generated-price gen/int
                 generated-quantity gen/int]
                (let [order (make-order generated-price generated-quantity)
                      copy-of-order (make-order (:price order) (:quantity order)
                                                (:time order) (:timestamp order))]
                  (and (= generated-price (:price order))
                       (= generated-quantity (:quantity order))
                       (= org.joda.time.DateTime (type (:time order)))
                       (= java.lang.Long (type (:timestamp order)))
                       (= order copy-of-order)))))

(comment "Check that `ORDER-BEFORE?' is a valid comparison function
ie. order-a should be before order-b or order-b should be before
order-a")
(tcct/defspec check-order-before?
  number-of-check-runs
  (prop/for-all [order-a (make-order-generator)
                 order-b (make-order-generator)]
                (= (order-before? order-a order-b)
                   (not (order-before? order-b order-a)))))

(defn make-price-comparator-prop
  "Create check property to check the price ordering for
  `make-order-comparator' using PRICE-COMPARATOR. Ensuring consistency
  of the comparator function ie. when the inputs are reversed the
  result is negated"
  [price-comparator]
  (prop/for-all [order-x (make-order-generator)
                 order-y (make-order-generator)]
                (= ((make-order-comparator price-comparator) order-x order-y)
                   (not ((make-order-comparator price-comparator) order-y order-x)))))

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
  (prop/for-all [orders (make-list-of-random-orders-generator)]
                (let [order-book (reduce add-order-fn (make-order-book) orders)]
                  (and (= (count orders) (count (order-list-accessor order-book)))
                       (= (sort price-comparison (map :price orders))
                          (map :price (order-list-accessor order-book)))))))

(comment "Check the addition of buy orders to an order-book")
(tcct/defspec add-buy-orders
  number-of-check-runs
  (make-add-orders-check-prop add-buy-order :buy-orders >))

(comment "Check the addition of sell orders to an order-book")
(tcct/defspec add-sell-orders
  number-of-check-runs
  (make-add-orders-check-prop add-sell-order :sell-orders <))

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
