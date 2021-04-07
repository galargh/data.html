(ns data.html
  (:require (data.html [tokenizer :refer [tokenize]]))
  (:import (java.io InputStream InputStreamReader StringReader Reader)))

(defn ^:private reader-to-lazy-seq
  [reader]
  (lazy-seq (let [read (.read reader)]
              (if (neg? read) '() (cons read (reader-to-lazy-seq reader))))))

(defprotocol Parsable
 (parse [obj] "Parses an HTML input source."))

(extend-protocol Parsable
 InputStream
   (parse [stream] (parse (InputStreamReader. stream)))
 Reader
   (parse [reader] (parse (map char (reader-to-lazy-seq reader))))
 Object
   (parse [coll] (tokenize coll {:state :data})))
