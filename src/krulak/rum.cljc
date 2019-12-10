(ns krulak.rum
  (:require [rum.core :as rum :refer (defc)]))

(defc include-css
  "Returns link tags referring to the given CSS files."
  ([url] (include-css nil url))
  ([attrs & urls]
   (for [url urls]
     [:link
      (assoc attrs :type "text/css", :href url, :rel "stylesheet")])))
