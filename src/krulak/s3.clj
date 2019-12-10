(ns krulak.s3
  "Functions to create policies that allow users to upload files to an
  Amazon S3 bucket."
  (:require [cheshire.core :as json]
            [clj-time.core :as tm]
            [clj-time.format :as tf]
            [clojure.string :as str]
            [krulak :as kr]
            [medley.core :as me]
            [ring.util.codec :as ruc]))

(def field-aliases
  {:cache-control :Cache-Control
   :content-disposition :Content-Disposition
   :content-encoding :Content-Encoding
   :content-type :Content-Type
   :expires :Expires
   :success-action-redirect :success_action_redirect
   :success-action-status :success_action_status})

(def range-condition? (comp boolean #{"content-length-range"}))

(defn policy-conditions [m]
  (for [[k v] m]
    (let [k (name (field-aliases k k))]
      (cond
       (range-condition? k) (apply vector k v)
       (or (string? v) (number? v)) {k v}
       true [(name (first v)) (str "$" k) (second v)]))))

(def format-policy-date (partial tf/unparse (tf/formatters :date-time)))

(defn to-date [f x]
  (if (number? x)
    (-> x f tm/from-now)
    x))

(def to-date-seconds (partial to-date tm/seconds))

(defn policy-map [bucket key expires & [conditions]]
  {:expiration (format-policy-date (to-date-seconds expires))
   :conditions (policy-conditions (assoc conditions :bucket bucket :key key))})

(defn policy
  "Return [base64-policy signature]."
  [secret-key & args]
  (let [p (-> (apply policy-map args)
              json/generate-string
              (.getBytes "utf-8")
              ruc/base64-encode)]
    [p (kr/base64-hmac-sha1 secret-key p)]))

(def no-policy-fields
  #{:AWSAccessKeyId :file :Policy :Signature})

(defn policy-needed? [kw]
  (not (or (no-policy-fields kw)
           (.startsWith (name kw) "x-ignore-"))))

(defn upload-form-data
  "Return [upload-url form-data]."
  [access-key secret-key bucket key expires & [fields conditions]]
  (let [fields (me/map-keys #(field-aliases % %) fields)
        conditions (me/map-keys #(field-aliases % %) conditions)
        conditions (-> (assoc fields :bucket bucket :key key)
                       (->> (me/filter-keys policy-needed?))
                       (merge conditions))
        [base64-policy signature]
		  (policy secret-key (:bucket conditions)
                          (:key conditions) expires conditions)]
  [(str "http://" bucket ".s3.amazonaws.com")
   (merge {:AWSAccessKeyId access-key
           :key key
           :Policy base64-policy
           :Signature signature}
          fields)]))
