(ns data.html
  (:require (data.html [tokenize-impl :as tokenize-impl]))
  (:import (java.io InputStream InputStreamReader Reader)))

(defn ^:private reader-to-lazy-seq
  [reader]
  (lazy-seq (let [read (.read reader)]
              (if (neg? read) '() (cons read (reader-to-lazy-seq reader))))))

(defprotocol Tokenizable
  (tokenize [obj] "Tokenizes an HTML input source."))

(extend-protocol Tokenizable
  InputStream
    (tokenize [stream] (tokenize (InputStreamReader. stream)))
  Reader
    (tokenize [reader] (tokenize (map char (reader-to-lazy-seq reader))))
  Object
    (tokenize [coll] (tokenize-impl/tokenize coll {:state :data})))
