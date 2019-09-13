(ns krulak.test
  (:require [clojure.test :as test]))

(defmacro status-is [status test]
  `(test/is (= ~status (:status ~test))))

(defmacro body-is [body test]
  `(test/is (= ~body (:body ~test))))
