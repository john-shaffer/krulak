(ns krulak
  "krulak is a web utility library for Clojure.")

(defmacro defalias [new-name old-name]
  `(let [doc-str# (:doc (meta #'~old-name))]
     (def ^{:doc doc-str#} ~new-name @#'~old-name)))

(defmacro defaliases [& syms]
  `(do
    ~@(map
       (fn [[new-name old-name]]
         `(defalias ~new-name ~old-name))
       (partition 2 syms))))
