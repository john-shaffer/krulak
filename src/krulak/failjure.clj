(ns krulak.failjure
  (:require [failjure.core :as f]))

(defn parse-double [s]
  (try
    (Double/parseDouble s)
    (catch Exception e
      (assoc
        (f/fail (str e))
        :error e))))

(defn parse-long [s]
  (try
    (Long/parseLong s)
    (catch Exception e
      (assoc
        (f/fail (str e))
        :error e))))
