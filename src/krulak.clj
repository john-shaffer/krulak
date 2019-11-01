(ns krulak
  "Utility functions and macros."
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as memo]
            [clojure.string :as str]
            [ring.util.codec :as codec])
  (:import clojure.core.memoize.PluggableMemoization))

(defn take-map [n m]
  (if (> 0 n)
    (empty m)
    (if (< n (count m))
      (->> m first key (dissoc m) (recur n))
      m)))

(defn merge-meta [x m]
  (vary-meta x merge m))

(defn lru-ttl-cache [base threshold ttl]
  (-> (take-map threshold base)
      (cache/lru-cache-factory :threshold threshold)
      (cache/ttl-cache-factory :ttl ttl)))

(def soft-lru-ttl-cache (comp cache/soft-cache-factory lru-ttl-cache))

(defn memo-build-helper [cache-factory f base & args]
  (memo/build-memoizer
   #(PluggableMemoization. %1 (apply cache-factory %2 %3))
   f
   (#'memo/derefable-seed base)
   args))

(defn lru-ttl-memo [f base threshold ttl]
  (merge-meta
    (memo-build-helper lru-ttl-cache f base threshold ttl)
    {::cache-threshold threshold
     ::cache-ttl ttl}))

(defn soft-lru-ttl-memo [f base threshold ttl]
  (merge-meta
    (memo-build-helper soft-lru-ttl-cache f base threshold ttl)
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

(defn memo-evict!
  "Evicts a set of arguments from the cache of a function backed by
  clojure.core.memoize. Returns the function.";
  [f & args]
  (-> f (#'clojure.core.memoize/cache-id) (swap! cache/evict args))
  f)

(defn memo-fresh
  "Takes a function backed by clojure.core.memoize and args and gets a
  fresh result, swapping the new result into the cache. This swaps a
  delay into the cache before dereferencing it, so it could cause latency
  for other users of the cache."
  [f & args]
  (let [g (memo/memo-unwrap f)
        id (delay (apply g args))]
    (-> f (#'clojure.core.memoize/cache-id)
        (swap! cache/miss args id))
    @id))

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
  (-> (or num-bytes 16) rand-bytes codec/base64-encode))

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
