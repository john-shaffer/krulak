(ns krulak
  "Utility functions and macros."
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.string :as str]
            [ring.util.codec :as codec]))

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
   json/parse-string))

(defn json-post [url & [req & r]]
  (update
   (client/post
    url
    (assoc req
           :accept :json
           :content-type :json)
    r)
   :body
   json/parse-string))

(defn json-put [url & [req & r]]
  (update
   (client/put
    url
    (assoc req
           :accept :json
           :content-type :json)
    r)
   :body
   json/parse-string))

(defn json-delete [url & [req & r]]
  (update
   (client/delete
    url
    (assoc req
           :accept :json
           :content-type :json)
    r)
   :body
   json/parse-string))
