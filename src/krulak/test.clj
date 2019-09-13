(ns krulak.test)

(defmacro status-is [status test]
  `(is (= ~status (:status ~test))))

(defmacro body-is [body test]
  `(is (= ~body (:body ~test))))
