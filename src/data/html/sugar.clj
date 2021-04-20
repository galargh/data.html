(ns data.html.sugar)

(defn type? [type token] (= (:type token) type))
(defn data? [data token] (= (:data token) data))
(defn public-identifier? [identifier token] (= (:public-identifier token) identifier))
(defn system-identifier? [identifier token] (= (:system-identifier token) identifier))
(defn attribute [name token] (some #(when (= (:name %) name) %) (:attributes token)))
(defn attribute?
  ([name value token] (= (:value (attribute name token)) value))
  ([name value re token] (contains? (set (clojure.string/split (:value (attribute name token) "") re)) value)))
(defn errors? [element] (some? (:errors element)))
(defn error? [error element] (contains? (set (:errors element)) error))
