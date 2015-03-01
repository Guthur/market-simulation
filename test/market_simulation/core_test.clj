(ns market-simulation.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as tcct]
            [market-simulation.core :refer :all :import #'make-order-generator]))

(def number-of-check-runs 1000)

(defn make-order-generator
  "Return a generator that produces an order using
  ORDER-VALUE-GENERATOR or if no ORDER-VALUE-GENERATOR is passed then
  return a generate for an order with random price and quantity"
  ([]
   (make-order-generator gen/pos-int gen/pos-int))
  ([price-generator quantity-generator]
   (gen/fmap (partial apply make-order)
             (gen/tuple price-generator quantity-generator))))

(defn make-order-list-generator
  "Return a generator that produces a non-empty list of orders"
  ([]
   (make-order-list-generator (make-order-generator)))
  ([order-generator]
   (gen/such-that not-empty (gen/vector order-generator))))

(defn make-order-book-generator
  "Return an order book that has aorders added using ADD-ORDER-FN"
  ([add-order-fn]
   (make-order-book-generator add-order-fn (make-order-list-generator)))
  ([add-order-fn order-generator]
   (gen/fmap (partial reduce add-order-fn (make-order-book)) order-generator)))

(comment "Check the creation of an order.  Checking that price and
quantity are correct and that the time is a DataTime object and
timestamp is a Long.  Also check that explicit order creation produces
a valid copy of order")
(tcct/defspec check-generate-order
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
  (prop/for-all [[order-a order-b] (gen/such-that (comp not
                                                        (partial apply =)
                                                        (partial map :timestamp))
                                                  (gen/tuple (make-order-generator)
                                                             (make-order-generator))
                                                  30)]
                (and (= (order-before? order-a order-b)
                        (not (order-before? order-b order-a)))
                     (< (- (:timestamp order-a) (:timestamp order-b))
                        0))))

(defn make-price-comparator-prop
  "Create check property to check the price ordering for
  `make-order-comparator' using PRICE-COMPARATOR.  Ensuring
  consistency of the comparator function ie. when the inputs are
  reversed the result is negated.  The check that PRICE-COMPARATOR
  produces the same result.  Not checking time ordering on price equal
  orders"
  [price-comparator]
  (prop/for-all [orders (gen/such-that (comp not
                                             (partial apply =)
                                             (partial map :price))
                                       (gen/tuple (make-order-generator)
                                                  (make-order-generator)))]
                (let [[order-a order-b] orders
                      order-comparator (#'market-simulation.core/make-order-comparator price-comparator)]
                  (and (= (order-comparator order-a order-b)
                          (not (order-comparator order-b order-a)))
                       (= (price-comparator (:price order-a) (:price order-b))
                          (order-comparator order-a order-b))))))

(comment "Check price is greater order comparision")
(tcct/defspec check-price-greater-order-comparator
  number-of-check-runs
  (make-price-comparator-prop >))

(comment "Check price is less than order comparision")
(tcct/defspec check-price-less-than-order-comparator
  number-of-check-runs
  (make-price-comparator-prop <))

(defn make-add-orders-check-prop
  "Create a check property for adding orders ensuring that 0 quantity
  orders are not added.  The function to add the particular order type
  is passed via ADD-ORDER-FN.  The order accessor function is passed
  via ORDER-LIST-ACCESSOR, the price comparator is passed via
  PRICE-COMPARISON."
  [add-order-fn order-list-accessor price-comparison]
  (prop/for-all [orders (make-order-list-generator)]
                (let [order-book (reduce add-order-fn (make-order-book) orders)]
                  (and (= (reduce #(if (zero? (:quantity %2)) %1 (inc %1))
                                  0
                                  orders)
                          (count (order-list-accessor order-book)))
                       (= (sort price-comparison (map :price (remove (comp zero? :quantity) orders)))
                          (map :price (order-list-accessor order-book)))))))

(comment "Check the addition of buy orders to an order-book")
(tcct/defspec check-add-buy-orders
  number-of-check-runs
  (make-add-orders-check-prop add-buy-order :buy-orders >))

(comment "Check the addition of sell orders to an order-book")
(tcct/defspec check-add-sell-orders
  number-of-check-runs
  (make-add-orders-check-prop add-sell-order :sell-orders <))

(defn make-check-top-price-property
  "Return check property for get top price functions.
  GET-TOP-PRICE-FN is the function to be tested.  ADD-ORDER-FN is the
  function for adding the orders to the order book.
  ORDER-LIST-ACCESSOR is the accessor function for the order list to
  checked.  PRICE-AGGREGATOR is the extremum function to check against"
  [get-top-price-fn add-order-fn order-list-accessor price-extremum]
  (prop/for-all [order-book (make-order-book-generator add-order-fn
                                                       (make-order-list-generator
                                                        (make-order-generator gen/pos-int
                                                                              gen/s-pos-int)))]
                (let [top-price (reduce price-extremum (map :price (order-list-accessor order-book)))]
                  (= (get-top-price-fn order-book)
                     top-price))))

(comment "Check getting the top sell price of an order book")
(tcct/defspec check-get-top-sell-price
  number-of-check-runs
  (make-check-top-price-property get-top-sell-price add-sell-order :sell-orders min))

(comment "Check getting the top buy price of an order book")
(tcct/defspec check-get-top-buy-price
  number-of-check-runs
  (make-check-top-price-property get-top-buy-price add-buy-order :buy-orders max))

(comment "Check getting the total quantity of all buy orders within an order book")
(tcct/defspec check-get-buy-list-quantity
  number-of-check-runs
  (prop/for-all [order-book (make-order-book-generator add-buy-order
                                                       (make-order-list-generator
                                                        (make-order-generator gen/pos-int
                                                                              gen/s-pos-int)))]
                (let [total-quantity (reduce + (map :quantity (:buy-orders order-book)))]
                  (= (get-buy-list-quantity order-book)
                     total-quantity))))

(comment "Check getting the total quantity of all sell orders within an order book")
(tcct/defspec check-get-sell-list-quantity
  number-of-check-runs
  (prop/for-all [order-book (make-order-book-generator add-sell-order
                                                       (make-order-list-generator
                                                        (make-order-generator gen/pos-int
                                                                              gen/s-pos-int)))]
                (let [total-quantity (reduce + (map :quantity (:sell-orders order-book)))]
                  (= (get-sell-list-quantity order-book)
                     total-quantity))))

(defn make-order-matching-prop
  "Return a check property to confirm that given a list orders with
  prices executable against an oppose order list in an order-book that
  either all the orders to match will be filled or all the opposing
  orders in the book will be filled.

  ADD-ORDER-FN provides the function for adding orders to the order
  book.  ORDER-MATCH-FN provides the order matching function.
  ORDERS-TO-MATCH-PRICES provides the generator to generate prices
  that will allow order execution against the prices generated using
  ORDER-BOOK-PRICES."
  [add-order-fn order-match-fn
   incoming-orders-accessor existing-orders-accessor
   orders-to-match-prices order-book-prices]
  (prop/for-all [order-book (make-order-book-generator
                             add-order-fn
                             (make-order-list-generator
                              (make-order-generator order-book-prices gen/s-pos-int)))
                 orders-to-match (make-order-list-generator
                                  (make-order-generator orders-to-match-prices gen/s-pos-int))]
                (let [order-book-quantity (reduce + (map :quantity (existing-orders-accessor order-book)))
                      orders-to-match-quantity (reduce + (map :quantity orders-to-match))]
                  (let [order-book (reduce order-match-fn order-book orders-to-match)]
                    (cond
                      ;; Starting order-book-quantity was less than
                      ;; orders-to-match-quantity. Check that
                      ;; existing-orders-accessor returns an empty
                      ;; list as they have all been filled and that
                      ;; remaining incoming-orders have a aggregate
                      ;; quantity of orders-to-match-quantity minus
                      ;; order-book-quantity
                      (< order-book-quantity orders-to-match-quantity)
                      (and (empty? (existing-orders-accessor order-book))
                           (= (reduce + (map :quantity (incoming-orders-accessor order-book)))
                              (- orders-to-match-quantity order-book-quantity)))
                      (> order-book-quantity orders-to-match-quantity)
                      (and (empty? (incoming-orders-accessor order-book))
                           (= (reduce + (map :quantity (existing-orders-accessor order-book)))
                              (- order-book-quantity orders-to-match-quantity)))
                      ;; The orders-to-match-quantity and
                      ;; order-book-quantity were equal, both order
                      ;; lists should be empty
                      :else
                      (and (empty? (:sell-orders order-book))
                           (empty? (:buy-orders order-book))))))))

(comment "Check sell order matching against order book with buy orders
that are priced to allow execution")
(tcct/defspec check-sell-order-matching
  number-of-check-runs
  (make-order-matching-prop add-buy-order match-sell-order
                            :sell-orders :buy-orders
                            (gen/choose 0 5) (gen/choose 5 10)))

(comment "Check buy order matching against order book with sell orders
that are priced to allow execution")
(tcct/defspec check-buy-order-matching
  number-of-check-runs
  (make-order-matching-prop add-sell-order match-buy-order
                            :buy-orders :sell-orders
                            (gen/choose 5 10) (gen/choose 0 5)))

(comment "Check that order execution is obeying the price matching
rules and removing the correct quantity from orders in the order book")
(tcct/defspec check-order-execution
  number-of-check-runs
  (prop/for-all [buy-order (make-order-generator gen/s-pos-int gen/s-pos-int)
                 sell-order (make-order-generator gen/s-pos-int gen/s-pos-int)]
                (let [order-book (match-sell-order (match-buy-order (make-order-book)
                                                                    buy-order)
                                                   sell-order)
                      sell-price (:price sell-order)
                      buy-price (:price buy-order)
                      sell-quantity (:quantity sell-order)
                      buy-quantity (:quantity buy-order)
                      remaining-sell-quantity (get-sell-list-quantity order-book)
                      remaining-buy-quantity (get-buy-list-quantity order-book)]
                  (or
                   ;; If buy price is equal to or greater an order
                   ;; execution should occur.
                   (and (>= buy-price sell-price)
                        (or
                         ;; Buy quantity is greater than sell.  There
                         ;; should be a remain buy order minus the
                         ;; sell quantity.
                         (and (> buy-quantity sell-quantity)
                              (= remaining-buy-quantity
                                 (- buy-quantity sell-quantity)))
                         ;; Sell quantity is greater than buy.  There
                         ;; should be a remain sell order minus the
                         ;; buy quantity.
                         (and (> sell-quantity buy-quantity)
                              (= remaining-sell-quantity
                                 (- sell-quantity buy-quantity)))
                         ;; Buy and sell quantities were equal, there
                         ;; should be no orders in the book.
                         (and (empty? (:sell-orders order-book))
                              (empty? (:buy-orders order-book)))))
                   ;; Buy price is less than sell, no execution should
                   ;; occur.
                   (and (= remaining-sell-quantity sell-quantity)
                        (= remaining-buy-quantity buy-quantity))))))
