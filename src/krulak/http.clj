(ns krulak.http
  (:require [cheshire.core :as json]
            [clj-http-failjure.client :as http]
            [failjure.core :as f]
            [krulak :as kr])
  (:refer-clojure :exclude [get]))

(kr/defaliases
  get http/get
  head http/head
  post http/post
  put http/put
  delete http/delete
  options http/options
  copy http/copy
  move http/move
  patch http/patch)

(defn json-get [url & [req]]
  (let [req (assoc req :accept :json)]
    (f/when-let-ok? [resp (get url req)]
      (if (empty? (:body resp))
        (assoc resp :body nil)
        (f/try-all [json-resp (json/parse-string (:body resp) true)]
          (assoc resp :body json-resp))))))

(defn json-request [http-method-f url req body]
  (let [req (assoc req :accept :json :content-type :json)]
    (f/try-all [json (json/generate-string body)]
      (f/when-let-ok? [resp (http-method-f url (assoc req :body json))]
        (if (empty? (:body resp))
          (assoc resp :body nil)
          (f/try-all [json-resp (json/parse-string (:body resp) true)]
            (assoc resp :body json-resp)))))))

(defn json-post [url req body]
  (json-request post url req body))

(defn json-put [url req body]
  (json-request put url req body))

(defn json-delete [url req body]
  (json-request delete url req body))
