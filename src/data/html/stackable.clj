(ns data.html.stackable
  (:import (java.io Reader)
           (clojure.lang StringSeq)))

(defprotocol Stackable
 (safe-peek [coll] [coll n] "Look at the n top characters.")
 (safe-pop [coll]
           [coll n]
           "Return the collection without the n top characters."))

(extend-protocol Stackable
 Reader
   (safe-peek
     ([coll] (first (safe-peek coll 1)))
     ([coll n]
      (let [top-n (do (.mark coll n)
                      (map char (remove #{-1} (repeatedly n #(.read coll)))))]
        (.reset coll)
        (empty top-n))))
   (safe-pop ([coll] (safe-pop coll 1))
             ([coll n] (nth (iterate #(do (.read %) %) coll) n)))
 String
   (safe-peek ([coll] (first (safe-peek coll 1)))
              ([coll n]
               (apply str (safe-peek (apply list (char-array coll)) n))))
   (safe-pop ([coll] (safe-pop coll 1))
             ([coll n]
              (apply str (safe-pop (apply list (char-array coll)) n))))
 StringSeq
   (safe-peek ([coll] (first (safe-peek coll 1)))
              ([coll n] (apply str (safe-peek (apply list coll) n))))
   (safe-pop ([coll] (safe-pop coll 1))
             ([coll n] (apply str (safe-pop (apply list coll) n))))
 Object
   (safe-peek
     ([coll] (first (safe-peek coll 1)))
     ([coll n]
      (if (= (min (count coll) n) 0)
        nil
        (cons (peek coll)
              (safe-peek (safe-pop coll) (dec (min (count coll) n)))))))
   (safe-pop ([coll] (safe-pop coll 1))
             ([coll n] (nth (iterate #(pop %) coll) (min (count coll) n)))))
