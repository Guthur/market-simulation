(ns market-simulation.core-test
  (:require [clojure.core.reducers :as r]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]            
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as tcct]
            [market-simulation.core :refer :all]))

(def number-of-test-runs 1000)

(comment "Test the creation of an order. Checking that price and
quantity are correct and that the time is a DataTime object")
(tcct/defspec generate-order
  number-of-test-runs
  (prop/for-all [generated-price gen/int
                 generated-quantity gen/int]
                (let [order (make-order generated-price generated-quantity)]
                  (and (= generated-price (order-price order))
                       (= generated-quantity (order-quantity order))
                       (= org.joda.time.DateTime (type (order-time order)))))))

(defn make-price-comparator-prop
  "Create test property to test the price ordering for
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

(comment "Test price is greater order comparision")
(tcct/defspec price-greater-order-comparator
  number-of-test-runs
  (make-price-comparator-prop >))

(comment "Test price is less than order comparision")
(tcct/defspec price-less-than-order-comparator
  number-of-test-runs
  (make-price-comparator-prop <))

(defn make-add-orders-test-prop 
  "Create a test property for adding orders. The function to add the
  particular order type is passed via ADD-ORDER-FN. The order accessor
  function is passed via ORDER-ACCESSOR, the price comparator is
  passed via PRICE-COMPARISON."
  [add-order-fn order-accessor price-comparison]
  (prop/for-all [prices-and-quantities (gen/vector (gen/vector gen/pos-int 2))]
                (let [order-book (r/reduce (fn [book order-values]
                                             (add-order-fn book (apply make-order order-values)))
                                           (make-order-book) 
                                           prices-and-quantities)]
                  (and (= (count prices-and-quantities) (count (order-accessor order-book)))
                       (= (sort price-comparison (map first prices-and-quantities))
                          (map order-price (order-accessor order-book)))))))

(comment "Test the addition of buy orders to an order-book")
(tcct/defspec add-buy-orders
  number-of-test-runs
  (make-add-orders-test-prop add-buy-order buy-orders >))

(comment "Test the addition of sell orders to an order-book")
(tcct/defspec add-sell-orders
  number-of-test-runs
  (make-add-orders-test-prop add-sell-order sell-orders <))
