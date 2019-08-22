(ns krulak
  "Utility functions and macros.")

(defn keywordize
  "Return a keyword whether given a string, symbol, or keyword."
  [x]
  (if (keyword? x) x (keyword (str x))))

(defmacro defalias [new-name old-name]
  `(let [doc-str# (:doc (meta #'~old-name))]
     (def ^{:doc doc-str#} ~new-name @#'~old-name)))

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
