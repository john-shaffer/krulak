(ns test-krulak
  (:use clojure.test
        krulak))

(deftest test-deep-merge
  (is (= {:a 1 :b {:x 1 :y 3} :c 2}
         (deep-merge {:a 1 :b {:x 0 :y 3}} {:b {:x 1} :c 2})))
  (is (= {:a {:a 1} :b {:b 2}}
         (deep-merge {:a nil :b {:b 2}} {:a {:a 1} :b nil}))))


(deftest test-base64url-uuid
  (is (= 21 (count (base64url-uuid))))
  (is (= 32 (count (base64url-uuid 32)))))
