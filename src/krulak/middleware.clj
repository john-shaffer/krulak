(ns krulak.middleware
  "An assortment of useful middleware for Ring handlers."
  (:require [krulak :as kr]
            [ring.middleware.format :as rmf]
            [ring.middleware.session :as rms]
            [ring.middleware.session.cookie :as rmsc]
            [ring.middleware.stacktrace :as rmst]
            [ring.util.codec :as ruc]))

(defn maybe-long-lived-cookie
  "Sets a cookie's max-age to 20 years if it does not have one yet."
  [cookie]
  (update-in cookie [:max-age] #(or % (* 60 60 24 365 20))))

(defn wrap-session-id
  "Automatically generates an :id and adds it to the session cookie."
  [f]
  (fn [request]
    (let [session-id (or (-> request :session :id) (kr/base64url-uuid))
          request (assoc-in request [:session :id] session-id)
          response (f request)]
      (if (contains? response :session)
        response
        (assoc response :session (:session request))))))

(defn wrap-encrypted-session-cookie
  "Creates an encrypted cookie store for :session data. The default
  session length is 20 years, but it can be configured by passing
  {:max-age length-in-seconds} as cookie-attrs.

  secret-key must be a length 16 byte-array or a base64-encoded string
  representing 16 bytes."
  [f secret-key & [cookie-attrs]]
  {:pre [(seq secret-key)]}
  (let [secret-key (if (string? secret-key)
                     (ruc/base64-decode secret-key)
                     secret-key)]
    (rms/wrap-session
     f
     {:store (rmsc/cookie-store {:key secret-key})
      :cookie-attrs (maybe-long-lived-cookie cookie-attrs)})))

(defn wrap-form-in-body-params
  "ring-middleware-format places decoded data in :body-params. This
  merges :form-params into :body-params. This is used when you want a
  function to accept both form data and AJAX-type data, but not query
  parameters, to avoid CSRF security holes."
  [f]
  (fn [request]
    (->> (merge (:form-params request) (:body-params request))
         (assoc request :body-params)
         f)))

(defn wrap-restful-format [f]
  (-> f
      wrap-form-in-body-params
      (rmf/wrap-restful-format
       :formats [:json-kw :edn :yaml-kw :yaml-in-html])))

(defmacro defrestful [sym & body]
  `(kr/defn-wrap ~sym wrap-restful-format
     ~@body))
