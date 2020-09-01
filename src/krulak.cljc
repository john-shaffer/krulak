(ns krulak
  "Utility functions and macros."
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as memo]
            [clojure.string :as str]
            [medley.core :as me]
            [ring.util.codec :as ruc])
  (:import clojure.core.memoize.PluggableMemoization
           java.net.URLEncoder
           [java.time LocalDate ZoneId]
           java.time.format.DateTimeFormatter
           java.util.Locale
           javax.crypto.Mac
           javax.crypto.spec.SecretKeySpec))

(set! *warn-on-reflection* true)

(defmacro defn-wrap
  "Like defn, but applies wrap-fn."
  [name-sym wrap-fn & body]
  `(do
     (defn ~name-sym ~@body)
     (alter-var-root #'~name-sym ~wrap-fn)))

(defn take-map [n m]
  (if (> 0 n)
    (empty m)
    (if (< n (count m))
      (->> m first key (dissoc m) (recur n))
      m)))

(defn merge-meta [x m]
  (vary-meta x merge m))

(defn lru-ttl-cache [base threshold ttl]
  (-> (or (take-map threshold base) {})
      (cache/lru-cache-factory :threshold threshold)
      (cache/ttl-cache-factory :ttl ttl)))

(def soft-lru-ttl-cache
  (comp cache/soft-cache-factory lru-ttl-cache))

(defn ^{:deprecated "0.8.0"} memo-build-helper
  "DEPRECATED: Use clojure.core.memoize/memoizer."
  [cache-factory f base & args]
  (memo/memoizer f (apply cache-factory base args)))

(defn lru-ttl-memo [f base threshold ttl]
  (merge-meta
   (memo/memoizer f (lru-ttl-cache base threshold ttl))
   {::cache-threshold threshold
    ::cache-ttl ttl}))

(defn soft-lru-ttl-memo [f base threshold ttl]
  (merge-meta
   (memo/memoizer f (soft-lru-ttl-cache base threshold ttl))
   {::cache-threshold threshold
    ::cache-ttl ttl}))

(defn memo-cache-swap!
  "Replaces the entire cache of a function backed by
  clojure.core.memoize, where clojure.core.memoize/cache-swap! only
  replaces the data inside the cache. Used when you want to adjust
  parameters of the cache (like size) on the fly."
  [f new-cache]
  (when-let [old-cache (#'memo/cache-id f)]
    (swap! old-cache (constantly new-cache))))

(defn ^{:deprecated "0.8.0"} memo-evict!
  "DEPRECATED: Use clojure.core.memoize/memo-clear!"
  [f & args]
  (-> f (#'memo/cache-id) (swap! cache/evict args))
  f)

(defn memo-update!
  "Update the cached return value for a seq of args for a function backed
  by clojure.core.memoize."
  [f args v]
  (-> f (#'memo/cache-id) (swap! cache/miss args (delay v))))

(defn memo-fresh
  "Takes a function backed by clojure.core.memoize and args, gets a
  fresh result, then swaps the new result into the cache."
  [f & args]
  (let [g (memo/memo-unwrap f)
        result (apply g args)]
    (memo-update! f args result)
    result))

(defn deep-merge [& args]
  (if (every? #(or (map? %) (nil? %)) args)
    (apply merge-with deep-merge args)
    (last args)))

(defn rmerge [& ms]
  "Like clojure.core/merge, but in reverse order."
  (apply merge (reverse ms)))

(defn maybe-deref [id]
  (when id @id))

(defn rpartial [f & args]
  #(apply f (concat %& args)))

(defn update-alias-meta [old-name old-meta]
  (fn [new-meta]
    (merge new-meta
      (update old-meta :doc #(str "Alias for " old-name "." (if % "\n\n") %)))))

(defmacro defalias [new-name old-name]
  `(alter-meta!
    (def ~new-name)
      (update-alias-meta '~old-name (meta (var ~old-name)))))

(defmacro defaliases [& syms]
  `(do
    ~@(map
       (fn [[new-name old-name]]
         `(defalias ~new-name ~old-name))
       (partition 2 syms))))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn rand-bytes [n]
  {:pre [(number? n)]}
  (let [arr (byte-array n)]
    (.nextBytes
      (java.security.SecureRandom/getInstance "SHA1PRNG")
      arr)
    arr))

(defn secret-key [& [num-bytes]]
  (-> (or num-bytes 16) rand-bytes ruc/base64-encode))

(defn base64url-encode
  "Return a base64url encoded string as defined by RFC 4648 §5 at
  https://tools.ietf.org/html/rfc4648#section-5"
  [byte-arr]
  (.encodeToString (java.util.Base64/getUrlEncoder) byte-arr))

(defn base64url-uuid
  "Return a random URL-safe base64 UUID. The default length is 21 characters,
  which represent 126 bits, 2 less bits than a normal UUID."
  ([] (base64url-uuid 21))
  ([length]
     (->> (rand-bytes length)
          base64url-encode
          (take length)
          (apply str))))

(def url-slug-replacements {\space \-, \( \_, \) \_})

(def url-slug-allowed-chars
  (set "0123456789abcdefghijklmnopqrstuvwxyz-_~"))

(defn to-url-slug [& xs]
  (->> (apply str xs)
       str/lower-case
       (map #(url-slug-replacements % %))
       (filter url-slug-allowed-chars)
       (apply str)))

(defn json-get [url & [req & r]]
  (update
   (client/get
    url
    (assoc req :accept :json)
    r)
   :body
   #(json/parse-string % true)))

(defn json-post [url & [req & r]]
  (update
   (client/post
    url
    (assoc req
           :accept :json
           :content-type :json)
    r)
   :body
   #(json/parse-string % true)))

(defn json-put [url & [req & r]]
  (update
   (client/put
    url
    (assoc req
           :accept :json
           :content-type :json)
    r)
   :body
   #(json/parse-string % true)))

(defn json-delete [url & [req & r]]
  (update
   (client/delete
    url
    (assoc req
           :accept :json
           :content-type :json)
    r)
   :body
   #(json/parse-string % true)))

(defn vectorize-params
  "Ring turns a duplicated parameter like ?c=a&c=b into a vector value,
  so you get {\"c\" [\"a\" \"b\"] in :params. A non-duplicated parameter like
  ?c=a is turned into {\"c\" \"a\"}. This function always puts parameter values
  inside vectors, so that callers only have to understand one format."
  [m]
  (me/map-vals #(if (vector? %) % [%]) m))

(defn full-name [x]
  (when x
    (if (string? x)
      x
      (if (simple-ident? x)
        (name x)
        (str (namespace x) "/" (name x))))))

(defn query-encode [k v]
  (str (URLEncoder/encode (full-name k)) "="
       (URLEncoder/encode (str v))))

(defn query-param-pairs [m]
  (mapcat
   (fn [[k v]]
     (if (coll? v)
       (map #(query-encode k %) v)
       (list (query-encode k v))))
   m))

(defn query-param-string
  "Turn a ring-style map of params into a querystring, including the ?. Values
  that are collections are turned into multiple key-value pairs:
  (query-param-string {:a [1 2]}) => \"?a=1&a=2\". This is the inverse of how
  ring processes the querystring."
  [m]
  (if (empty? m)
    ""
    (let [[f & more] (query-param-pairs m)]
      (str/join "&" (cons (str "?" f) more)))))

#?(:clj
   (def date-formatter
     (DateTimeFormatter/ofPattern "MMMM d, yyyy" Locale/ENGLISH)))

#?(:clj
   (def utc-zone
     (ZoneId/of "UTC")))

(def month-names
  ["January" "February" "March" "April" "May" "June"
   "July" "August" "September" "October" "November" "December"])

(defn pretty-date
  "Return date formatted like \"February 2, 2014\"."
  [dt]
  #?(:clj (-> dt .toInstant (LocalDate/ofInstant utc-zone)
            (.format date-formatter))
     :cljs
     (str
       (nth month-names (.getUTCMonth dt)) " "
       (.getUTCDate dt) ", " (.getUTCFullYear dt))))

(defn hmac-fn [algorithm]
  (fn ^bytes [^String key ^String data]
    (let [key (.getBytes key "UTF-8")
          data (.getBytes data "UTF-8")
          mac (Mac/getInstance algorithm)]
      (.init mac (SecretKeySpec. key algorithm))
      (.doFinal mac data))))

(def hmac-sha1 (hmac-fn "HmacSHA1"))
(def hmac-sha256 (hmac-fn "HmacSHA256"))
(def base64-hmac-sha1 (comp ruc/base64-encode hmac-sha1))
(def base64-hmac-sha256 (comp ruc/base64-encode hmac-sha256))
