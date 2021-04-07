(ns data.html.tokenizer
  (:require
   (data.html
    [numeric :refer
     [null-character-reference? character-reference-outside-unicode-range?
      surrogate-character-reference? noncharacter-character-reference?
      control-character-reference? numeric-character-references]]
    [named :refer [named-character-references named-character-reference-find]]
    [stackable :refer [safe-peek safe-pop]])
   [clojure.string :refer [upper-case]])
  (:import (java.util.regex Pattern)))

(defn ^:private pattern? [pattern] (isa? (class pattern) Pattern))

(defn ^:private consume
  [matching-characher next-input-character]
  (cond (coll? matching-characher) (contains? matching-characher
                                              next-input-character)
        (fn? matching-characher) (matching-characher next-input-character)
        (pattern? matching-characher) (re-matches matching-characher
                                                  (str next-input-character))
        :else (= matching-characher next-input-character)))

(defn tokenize
  [[next-input-character & remaining-input-characters :as all-input-characters]
   {:keys [state return-state temporary-buffer stack-of-open-elements
           character-reference-code],
    {:keys [type data name public-identifier system-identifier errors],
     [attribute & remaining-attributes :as attributes] :attributes,
     :as token}
    :token,
    :as tokenizer-state}]
  (case state
    :data (condp consume next-input-character
            \& (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :character-reference
                                :return-state :data))
            \< (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :tag-open))
            \u0000 (cons {:type :character,
                          :data \u0000,
                          :errors (list :unexpected-null-character)}
                         (tokenize remaining-input-characters tokenizer-state))
            nil (list {:type :EOF, :tokenizer-state tokenizer-state})
            (cons {:type :character, :data next-input-character}
                  (tokenize remaining-input-characters tokenizer-state)))
    :RCDATA
    (condp consume next-input-character
      \& (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :character-reference
                          :return-state :RCDATA))
      \< (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :RCDATA-less-than-sign))
      \u0000 (cons {:type :character,
                    :data \uFFFD,
                    :errors (list :unexpected-null-character)}
                   (tokenize remaining-input-characters tokenizer-state))
      nil (list {:type :EOF, :tokenizer-state tokenizer-state})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters tokenizer-state)))
    :RAWTEXT
    (condp consume next-input-character
      \< (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :RAWTEXT-less-than-sign))
      \u0000 (cons {:type :character,
                    :data \uFFFD,
                    :errors (list :unexpected-null-character)}
                   (tokenize remaining-input-characters tokenizer-state))
      nil (list {:type :EOF, :tokenizer-state tokenizer-state})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters tokenizer-state)))
    :script-data
    (condp consume next-input-character
      \< (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :script-data-less-than-sign))
      \u0000 (cons {:type :character,
                    :data \uFFFD,
                    :errors (list :unexpected-null-character)}
                   (tokenize remaining-input-characters tokenizer-state))
      nil (list {:type :EOF, :tokenizer-state tokenizer-state})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters tokenizer-state)))
    :PLAINTEXT (condp consume next-input-character
                 \u0000 (cons {:type :character,
                               :data \uFFFD,
                               :errors (list :unexpected-null-character)}
                              (tokenize remaining-input-characters
                                        tokenizer-state))
                 nil (list {:type :EOF, :tokenizer-state tokenizer-state})
                 (cons {:type :character, :data next-input-character}
                       (tokenize remaining-input-characters tokenizer-state)))
    :tag-open
    (condp consume next-input-character
      \! (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :markup-declaration-open))
      \/ (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :end-tag-open))
      #"[A-Za-z]" (tokenize all-input-characters
                            (assoc tokenizer-state
                                   :state :tag-name
                                   :token {:type :start-tag, :data ""}))
      \? (tokenize
          all-input-characters
          (assoc tokenizer-state
                 :state :bogus-comment
                 :token {:type :comment,
                         :data "",
                         :errors
                         (list :unexpected-question-mark-instead-of-tag-name)}))
      nil (list
           {:type :character, :data \<, :errors (list :eof-before-tag-name)}
           {:type :EOF, :tokenizer-state tokenizer-state})
      (cons {:type :character,
             :data \<,
             :errors (list :invalid-first-character-of-tag-name)}
            (tokenize all-input-characters
                      (assoc tokenizer-state :state :data))))
    :end-tag-open
    (condp consume next-input-character
      #"[A-Za-z]" (tokenize all-input-characters
                            (assoc tokenizer-state
                                   :state :tag-name
                                   :token {:type :end-tag, :data ""}))
      \> (cons {:type :error, :data :missing-end-tag-name}
               (tokenize all-input-characters
                         (assoc tokenizer-state :state :data)))
      nil (list
           {:type :character, :data \<, :errors (list :eof-before-tag-name)}
           {:type :character, :data \/}
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :bogus-comment
              :token {:type :comment,
                      :data "",
                      :errors (list :invalid-first-character-of-tag-name)})))
    :tag-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (tokenize remaining-input-characters
                (assoc tokenizer-state :state :before-attribute-name))
      \/ (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :self-closing-start-tag))
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :data
                                :token nil
                                :stack-of-open-elements
                                (if (= type :start-tag)
                                  (conj stack-of-open-elements token)
                                  (safe-pop stack-of-open-elements)))))
      #"[A-Z]"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :data
                     (str data (Character/toLowerCase next-input-character)))))
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :token
                     (assoc token
                            :data (str data \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-tag)})
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token :data (str data next-input-character)))))
    :RCDATA-less-than-sign (condp consume next-input-character
                             \/ (tokenize remaining-input-characters
                                          (assoc tokenizer-state
                                                 :state :RCDATA-end-tag-open
                                                 :temporary-buffer ""))
                             (cons {:type :character, :data \<}
                                   (tokenize
                                    all-input-characters
                                    (assoc tokenizer-state :state :RCDATA))))
    :RCDATA-end-tag-open
    (condp consume next-input-character
      #"[A-Za-z]" (tokenize all-input-characters
                            (assoc tokenizer-state
                                   :state :RCDATA-end-tag-name
                                   :token {:type :end-tag, :data ""}))
      (cons {:type :character, :data \<}
            (cons {:type :character, :data \/}
                  (tokenize all-input-characters
                            (assoc tokenizer-state :state :RCDATA)))))
    :RCDATA-end-tag-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (if (= data (:data (safe-peek stack-of-open-elements)))
        (tokenize remaining-input-characters
                  (assoc tokenizer-state :state :before-attribute-name))
        (concat (map #(-> {:type :character, :data %})
                     (str \< \/ temporary-buffer))
                (tokenize
                 all-input-characters
                 (assoc tokenizer-state :state :RCDATA :temporary-buffer nil))))
      \/ (if (= data (:data (safe-peek stack-of-open-elements)))
           (tokenize remaining-input-characters
                     (assoc tokenizer-state :state :self-closing-start-tag))
           (concat
            (map #(-> {:type :character, :data %})
                 (str \< \/ temporary-buffer))
            (tokenize
             all-input-characters
             (assoc tokenizer-state :state :RCDATA :temporary-buffer nil))))
      \>
      (if (= data (:data (safe-peek stack-of-open-elements)))
        (cons token
              (tokenize remaining-input-characters
                        (assoc tokenizer-state
                               :state :data
                               :token nil
                               :stack-of-open-elements
                               (safe-pop stack-of-open-elements))))
        (concat (map #(-> {:type :character, :data %})
                     (str \< \/ temporary-buffer))
                (tokenize
                 all-input-characters
                 (assoc tokenizer-state :state :RCDATA :temporary-buffer nil))))
      #"[A-Z]" (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :token (assoc token
                                              :data
                                              (str data
                                                   (Character/toLowerCase
                                                    next-input-character)))
                                :temporary-buffer (str temporary-buffer
                                                       (Character/toLowerCase
                                                        next-input-character))))
      #"[a-z]"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token (assoc token :data (str data next-input-character))
              :temporary-buffer (str temporary-buffer next-input-character)))
      (concat (map #(-> {:type :character, :data %})
                   (str \< \/ temporary-buffer))
              (tokenize
               all-input-characters
               (assoc tokenizer-state :state :RCDATA :temporary-buffer nil))))
    :RAWTEXT-less-than-sign (condp consume next-input-character
                              \/ (tokenize remaining-input-characters
                                           (assoc tokenizer-state
                                                  :state :RAWTEXT-end-tag-open
                                                  :temporary-buffer ""))
                              (cons {:type :character, :data \<}
                                    (tokenize
                                     all-input-characters
                                     (assoc tokenizer-state :state :RAWTEXT))))
    :RAWTEXT-end-tag-open
    (condp consume next-input-character
      #"[A-Za-z]" (tokenize all-input-characters
                            (assoc tokenizer-state
                                   :state :RAWTEXT-end-tag-name
                                   :token {:type :end-tag, :data ""}))
      (cons {:type :character, :data \<}
            (cons {:type :character, :data \/}
                  (tokenize all-input-characters
                            (assoc tokenizer-state :state :RAWTEXT)))))
    :RAWTEXT-end-tag-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (if (= data (:data (safe-peek stack-of-open-elements)))
        (tokenize remaining-input-characters
                  (assoc tokenizer-state :state :before-attribute-name))
        (concat
         (map #(-> {:type :character, :data %})
              (str \< \/ temporary-buffer))
         (tokenize
          all-input-characters
          (assoc tokenizer-state :state :RAWTEXT :temporary-buffer nil))))
      \/ (if (= data (:data (safe-peek stack-of-open-elements)))
           (tokenize remaining-input-characters
                     (assoc tokenizer-state :state :self-closing-start-tag))
           (concat
            (map #(-> {:type :character, :data %})
                 (str \< \/ temporary-buffer))
            (tokenize
             all-input-characters
             (assoc tokenizer-state :state :RAWTEXT :temporary-buffer nil))))
      \> (if (= data (:data (safe-peek stack-of-open-elements)))
           (cons token
                 (tokenize remaining-input-characters
                           (assoc tokenizer-state
                                  :state :data
                                  :token nil
                                  :stack-of-open-elements
                                  (safe-pop stack-of-open-elements))))
           (concat
            (map #(-> {:type :character, :data %})
                 (str \< \/ temporary-buffer))
            (tokenize
             all-input-characters
             (assoc tokenizer-state :state :RAWTEXT :temporary-buffer nil))))
      #"[A-Z]" (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :token (assoc token
                                              :data
                                              (str data
                                                   (Character/toLowerCase
                                                    next-input-character)))
                                :temporary-buffer (str temporary-buffer
                                                       (Character/toLowerCase
                                                        next-input-character))))
      #"[a-z]"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token (assoc token :data (str data next-input-character))
              :temporary-buffer (str temporary-buffer next-input-character)))
      (concat (map #(-> {:type :character, :data %})
                   (str \< \/ temporary-buffer))
              (tokenize
               all-input-characters
               (assoc tokenizer-state :state :RAWTEXT :temporary-buffer nil))))
    :script-data-less-than-sign
    (condp consume next-input-character
      \/ (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :script-data-end-tag-open
                          :temporary-buffer ""))
      \! (cons
          {:type :character, :data \<}
          (cons {:type :character, :data \!}
                (tokenize
                 remaining-input-characters
                 (assoc tokenizer-state :state :script-data-escape-start))))
      (cons {:type :character, :data \<}
            (tokenize all-input-characters
                      (assoc tokenizer-state :state :script-data))))
    :script-data-end-tag-open
    (condp consume next-input-character
      #"[A-Za-z]" (tokenize all-input-characters
                            (assoc tokenizer-state
                                   :state :script-data-end-tag-name
                                   :token {:type :end-tag, :data ""}))
      (cons {:type :character, :data \<}
            (cons {:type :character, :data \/}
                  (tokenize all-input-characters
                            (assoc tokenizer-state :state :script-data)))))
    :script-data-end-tag-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (if (= data (:data (safe-peek stack-of-open-elements)))
        (tokenize remaining-input-characters
                  (assoc tokenizer-state :state :before-attribute-name))
        (concat
         (map #(-> {:type :character, :data %})
              (str \< \/ temporary-buffer))
         (tokenize
          all-input-characters
          (assoc tokenizer-state :state :script-data :temporary-buffer nil))))
      \/ (if (= data (:data (safe-peek stack-of-open-elements)))
           (tokenize remaining-input-characters
                     (assoc tokenizer-state :state :self-closing-start-tag))
           (concat (map #(-> {:type :character, :data %})
                        (str \< \/ temporary-buffer))
                   (tokenize all-input-characters
                             (assoc tokenizer-state
                                    :state :script-data
                                    :temporary-buffer nil))))
      \> (if (= data (:data (safe-peek stack-of-open-elements)))
           (cons token
                 (tokenize remaining-input-characters
                           (assoc tokenizer-state
                                  :state :data
                                  :token nil
                                  :stack-of-open-elements
                                  (safe-pop stack-of-open-elements))))
           (concat (map #(-> {:type :character, :data %})
                        (str \< \/ temporary-buffer))
                   (tokenize all-input-characters
                             (assoc tokenizer-state
                                    :state :script-data
                                    :temporary-buffer nil))))
      #"[A-Z]" (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :token (assoc token
                                              :data
                                              (str data
                                                   (Character/toLowerCase
                                                    next-input-character)))
                                :temporary-buffer (str temporary-buffer
                                                       (Character/toLowerCase
                                                        next-input-character))))
      #"[a-z]"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token (assoc token :data (str data next-input-character))
              :temporary-buffer (str temporary-buffer next-input-character)))
      (concat
       (map #(-> {:type :character, :data %})
            (str \< \/ temporary-buffer))
       (tokenize
        all-input-characters
        (assoc tokenizer-state :state :script-data :temporary-buffer nil))))
    :script-data-escape-start
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize
                remaining-input-characters
                (assoc tokenizer-state :state :script-data-escape-start-dash)))
      (tokenize all-input-characters
                (assoc tokenizer-state :state :script-data)))
    :script-data-escape-start-dash
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state
                                :script-data-escaped-start-dash-dash)))
      (tokenize all-input-characters
                (assoc tokenizer-state :state :script-data)))
    :script-data-escaped
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize
                remaining-input-characters
                (assoc tokenizer-state :state :script-data-escaped-dash)))
      \< (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :script-data-escaped-less-than-sign))
      \u0000 (cons {:type :character,
                    :data \uFFFD,
                    :errors (list :unexpected-null-character)}
                   (tokenize remaining-input-characters tokenizer-state))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-script-html-comment-like-text)})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters tokenizer-state)))
    :script-data-escaped-dash
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize
                remaining-input-characters
                (assoc tokenizer-state :state :script-data-escaped-dash-dash)))
      \< (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :script-data-escaped-less-than-sign))
      \u0000 (cons
              {:type :character,
               :data \uFFFD,
               :errors (list :unexpected-null-character)}
              (tokenize remaining-input-characters
                        (assoc tokenizer-state :state :script-data-escaped)))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-script-html-comment-like-text)})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters
                      (assoc tokenizer-state :state :script-data-escaped))))
    :script-data-escaped-dash-dash
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize remaining-input-characters tokenizer-state))
      \< (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :script-data-escaped-less-than-sign))
      \> (cons {:type :character, :data \>}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :script-data)))
      \u0000 (cons
              {:type :character,
               :data \uFFFD,
               :errors (list :unexpected-null-character)}
              (tokenize remaining-input-characters
                        (assoc tokenizer-state :state :script-data-escaped)))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-script-html-comment-like-text)})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters
                      (assoc tokenizer-state :state :script-data-escaped))))
    :script-data-escaped-less-than-sign
    (condp consume next-input-character
      \/ (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :script-data-escaped-end-tag-open
                          :temporary-buffer ""))
      #"[A-Za-z]" (cons {:type :character, :data \<}
                        (tokenize all-input-characters
                                  (assoc tokenizer-state
                                         :state :script-data-double-escape-start
                                         :temporary-buffer "")))
      (cons {:type :character, :data \<}
            (tokenize all-input-characters
                      (assoc tokenizer-state :state :script-data-escaped))))
    :script-data-escaped-end-tag-open
    (condp consume next-input-character
      #"[A-Za-z]" (tokenize all-input-characters
                            (assoc tokenizer-state
                                   :state :script-data-escaped-end-tag-name
                                   :token {:type :end-tag, :data ""}))
      (cons {:type :character, :data \<}
            (cons {:type :character, :data \/}
                  (tokenize
                   all-input-characters
                   (assoc tokenizer-state :state :script-data-escaped)))))
    :script-data-escaped-end-tag-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (if (= data (:data (safe-peek stack-of-open-elements)))
        (tokenize remaining-input-characters
                  (assoc tokenizer-state :state :before-attribute-name))
        (concat (map #(-> {:type :character, :data %})
                     (str \< \/ temporary-buffer))
                (tokenize all-input-characters
                          (assoc tokenizer-state
                                 :state :script-data-escaped
                                 :temporary-buffer nil))))
      \/ (if (= data (:data (safe-peek stack-of-open-elements)))
           (tokenize remaining-input-characters
                     (assoc tokenizer-state :state :self-closing-start-tag))
           (concat (map #(-> {:type :character, :data %})
                        (str \< \/ temporary-buffer))
                   (tokenize all-input-characters
                             (assoc tokenizer-state
                                    :state :script-data-escaped
                                    :temporary-buffer nil))))
      \> (if (= data (:data (safe-peek stack-of-open-elements)))
           (cons token
                 (tokenize remaining-input-characters
                           (assoc tokenizer-state
                                  :state :data
                                  :token nil
                                  :stack-of-open-elements
                                  (safe-pop stack-of-open-elements))))
           (concat (map #(-> {:type :character, :data %})
                        (str \< \/ temporary-buffer))
                   (tokenize all-input-characters
                             (assoc tokenizer-state
                                    :state :script-data-escaped
                                    :temporary-buffer nil))))
      #"[A-Z]" (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :token (assoc token
                                              :data
                                              (str data
                                                   (Character/toLowerCase
                                                    next-input-character)))
                                :temporary-buffer (str temporary-buffer
                                                       (Character/toLowerCase
                                                        next-input-character))))
      #"[a-z]"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token (assoc token :data (str data next-input-character))
              :temporary-buffer (str temporary-buffer next-input-character)))
      (concat (map #(-> {:type :character, :data %})
                   (str \< \/ temporary-buffer))
              (tokenize all-input-characters
                        (assoc tokenizer-state
                               :state :script-data-escaped
                               :temporary-buffer nil))))
    :script-data-double-escape-start
    (condp consume next-input-character
      #{\tab \u000A \u000C \space \/ \>}
      (if (= temporary-buffer "script")
        (cons {:type :character, :data next-input-character}
              (tokenize
               remaining-input-characters
               (assoc tokenizer-state :state :script-data-double-escaped)))
        (cons {:type :character, :data next-input-character}
              (tokenize remaining-input-characters
                        (assoc tokenizer-state :state :script-data-escaped))))
      #"[A-Z]" (cons {:type :character, :data next-input-character}
                     (tokenize remaining-input-characters
                               (assoc tokenizer-state
                                      :temporary-buffer
                                      (str temporary-buffer
                                           (Character/toLowerCase
                                            next-input-character)))))
      #"[a-z]" (cons
                {:type :character, :data next-input-character}
                (tokenize remaining-input-characters
                          (assoc tokenizer-state
                                 :temporary-buffer
                                 (str temporary-buffer next-input-character))))
      (tokenize all-input-characters
                (assoc tokenizer-state :state :script-data-escaped)))
    :script-data-double-escaped
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state
                                :script-data-double-escaped-dash)))
      \< (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state
                          :script-data-double-escaped-less-than-sign))
      \u0000 (cons {:type :character,
                    :data \uFFFD,
                    :errors (list :unexpected-null-character)}
                   (tokenize remaining-input-characters tokenizer-state))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-script-html-comment-like-text)})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters tokenizer-state)))
    :script-data-double-escaped-dash
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state
                                :script-data-double-escaped-dash-dash)))
      \< (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state
                          :script-data-double-escaped-less-than-sign))
      \u0000 (cons {:type :character,
                    :data \uFFFD,
                    :errors (list :unexpected-null-character)}
                   (tokenize
                    remaining-input-characters
                    (assoc tokenizer-state :state :script-data-double-escaped)))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-script-html-comment-like-text)})
      (cons {:type :character, :data next-input-character}
            (tokenize
             remaining-input-characters
             (assoc tokenizer-state :state :script-data-double-escaped))))
    :script-data-double-escaped-dash-dash
    (condp consume next-input-character
      \- (cons {:type :character, :data \-}
               (tokenize remaining-input-characters tokenizer-state))
      \< (cons {:type :character, :data \<}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state
                                :script-data-double-escaped-less-than-sign)))
      \> (cons {:type :character, :data \>}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :script-data)))
      \u0000 (cons {:type :character,
                    :data \uFFFD,
                    :errors (list :unexpected-null-character)}
                   (tokenize
                    remaining-input-characters
                    (assoc tokenizer-state :state :script-data-double-escaped)))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-script-html-comment-like-text)})
      (cons {:type :character, :data next-input-character}
            (tokenize
             remaining-input-characters
             (assoc tokenizer-state :state :script-data-double-escaped))))
    :script-data-double-escaped-less-than-sign
    (condp consume next-input-character
      \/ (cons {:type :character, :data \/}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :script-data-double-escape-end
                                :temporary-buffer "")))
      (tokenize all-input-characters
                (assoc tokenizer-state :state :script-data-double-escaped)))
    :script-data-double-escape-end
    (condp consume next-input-character
      #{\tab \u000A \u000C \space \/ \>}
      (if (= temporary-buffer "script")
        (cons {:type :character, :data next-input-character}
              (tokenize remaining-input-characters
                        (assoc tokenizer-state :state :script-data-escaped)))
        (cons {:type :character, :data next-input-character}
              (tokenize
               remaining-input-characters
               (assoc tokenizer-state :state :script-data-double-escaped))))
      #"[A-Z]" (cons {:type :character, :data next-input-character}
                     (tokenize remaining-input-characters
                               (assoc tokenizer-state
                                      :temporary-buffer
                                      (str temporary-buffer
                                           (Character/toLowerCase
                                            next-input-character)))))
      #"[a-z]" (cons
                {:type :character, :data next-input-character}
                (tokenize remaining-input-characters
                          (assoc tokenizer-state
                                 :temporary-buffer
                                 (str temporary-buffer next-input-character))))
      (tokenize all-input-characters
                (assoc tokenizer-state :state :script-data-double-escaped)))
    :before-attribute-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      #{\/ \> nil} (tokenize
                    all-input-characters
                    (assoc tokenizer-state :state :after-attribute-name))
      \= (tokenize
          remaining-input-characters
          (assoc tokenizer-state
                 :state :attribute-name
                 :token (assoc
                         token
                         :attributes
                         (conj
                          {:name (str next-input-character),
                           :value "",
                           :errors
                           (list :unexpected-equals-sign-before-attribute-name)}
                          attributes))))
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :attribute-name
                       :token (assoc token
                                     :attributes
                                     (conj attributes {:name "", :value ""})))))
    :attribute-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space \/ \> nil}
      (if (contains? (set (map :name remaining-attributes)) (:name attribute))
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state :after-attribute-name
                :token (assoc token
                              :attributes
                              (conj remaining-attributes
                                    (assoc attribute
                                           :errors
                                           (conj (:errors attribute)
                                                 :duplicate-attribute))))))
        (tokenize all-input-characters
                  (assoc tokenizer-state :state :after-attribute-name)))
      \=
      (if (contains? (set (map :name remaining-attributes)) (:name attribute))
        (tokenize
         remaining-input-characters
         (assoc tokenizer-state
                :state :before-attribute-value
                :token (assoc token
                              :attributes
                              (conj remaining-attributes
                                    (assoc attribute
                                           :errors
                                           (conj (:errors attribute)
                                                 :duplicate-attribute))))))
        (tokenize remaining-input-characters
                  (assoc tokenizer-state :state :before-attribute-value)))
      #"[A-Z]"
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token
                              :attributes
                              (conj remaining-attributes
                                    (assoc attribute
                                           :name
                                           (str (:name attribute)
                                                (Character/toLowerCase
                                                 next-input-character)))))))
      \u0000
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :attributes
                     (conj remaining-attributes
                           (assoc attribute
                                  :name (str (:name attribute) \uFFFD)
                                  :errors (conj (:errors attribute)
                                                :unexpected-null-character))))))
      #{\" \' \<}
      (tokenize
       remaining-input-characters
       (assoc
        tokenizer-state
        :token
        (assoc token
               :attributes
               (conj remaining-attributes
                     (assoc attribute
                            :name (str (:name attribute) next-input-character)
                            :errors
                            (conj (:errors attribute)
                                  :unexpected-character-in-attribute-name))))))
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token
                              :attributes
                              (conj remaining-attributes
                                    (assoc attribute
                                           :name
                                           (str (:name attribute)
                                                next-input-character)))))))
    :after-attribute-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      \/ (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :self-closing-start-tag))
      \= (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :before-attribute-value))
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :data
                                :token nil
                                :stack-of-open-elements
                                (if (= type :start-tag)
                                  (conj stack-of-open-elements token)
                                  (safe-pop stack-of-open-elements)))))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-tag)})
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :attribute-name
                       :token (assoc token
                                     :attributes
                                     (conj attributes {:name "", :value ""})))))
    :before-attribute-value
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      \" (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :attribute-value-double-quoted))
      \' (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :attribute-value-single-quoted))
      \> (let [token (assoc token
                            :attributes
                            (conj remaining-attributes
                                  (assoc attribute
                                         :errors
                                         (conj (:errors attribute)
                                               :missing-attribute-value))))]
           (cons token
                 (tokenize remaining-input-characters
                           (assoc tokenizer-state
                                  :state :data
                                  :token nil
                                  :stack-of-open-elements
                                  (conj stack-of-open-elements token)))))
      (tokenize all-input-characters
                (assoc tokenizer-state :state :attribute-value-unquoted)))
    :attribute-value-double-quoted
    (condp consume next-input-character
      \" (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :after-attribute-value-quoted))
      \& (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :character-reference
                          :return-state :attribute-value-double-quoted))
      \u0000
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :attributes
                     (conj remaining-attributes
                           (assoc attribute
                                  :value (str (:value attribute) \uFFFD)
                                  :errors (conj (:errors attribute)
                                                :unexpected-null-character))))))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-tag)})
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token
                              :attributes
                              (conj remaining-attributes
                                    (assoc attribute
                                           :value
                                           (str (:value attribute)
                                                next-input-character)))))))
    :attribute-value-single-quoted
    (condp consume next-input-character
      \' (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :after-attribute-value-quoted))
      \& (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :character-reference
                          :return-state :attribute-value-single-quoted))
      \u0000
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :attributes
                     (conj remaining-attributes
                           (assoc attribute
                                  :value (str (:value attribute) \uFFFD)
                                  :errors (conj (:errors attribute)
                                                :unexpected-null-character))))))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-tag)})
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token
                              :attributes
                              (conj remaining-attributes
                                    (assoc attribute
                                           :value
                                           (str (:value attribute)
                                                next-input-character)))))))
    :attribute-value-unquoted
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (tokenize remaining-input-characters
                (assoc tokenizer-state :state :before-attribute-name))
      \& (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :character-reference
                          :return-state :attribute-value-unquoted))
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :data
                                :token nil
                                :stack-of-open-elements
                                (conj stack-of-open-elements token))))
      \u0000
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :attributes
                     (conj remaining-attributes
                           (assoc attribute
                                  :value (str (:value attribute) \uFFFD)
                                  :errors (conj (:errors attribute)
                                                :unexpected-null-character))))))
      #{\" \' \< \= \`}
      (tokenize
       remaining-input-characters
       (assoc
        tokenizer-state
        :token
        (assoc
         token
         :attributes
         (conj
          remaining-attributes
          (assoc attribute
                 :value (str (:value attribute) next-input-character)
                 :errors
                 (conj (:errors attribute)
                       :unexpected-character-in-unquoted-attribute-value))))))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-tag)})
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token
                              :attributes
                              (conj remaining-attributes
                                    (assoc attribute
                                           :value
                                           (str (:value attribute)
                                                next-input-character)))))))
    :after-attribute-value-quoted
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (tokenize remaining-input-characters
                (assoc tokenizer-state :state :before-attribute-name))
      \/ (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :self-closing-start-tag))
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :data
                                :token nil
                                :stack-of-open-elements
                                (conj stack-of-open-elements token))))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-tag)})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :before-attribute-name
              :token (assoc
                      token
                      :attributes
                      (conj remaining-attributes
                            (assoc
                             attribute
                             :errors
                             (conj (:errors attribute)
                                   :missing-whitespace-between-attributes)))))))
    :self-closing-start-tag
    (condp consume next-input-character
      \> (cons (assoc token :self-closing :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-tag)})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :before-attribute-name
              :token
              (assoc token :errors (conj errors :unexpected-solidus-in-tag)))))
    :bogus-comment
    (condp consume next-input-character
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list token {:type :EOF, :tokenizer-state tokenizer-state})
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :token
                     (assoc token
                            :data (str data \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token :data (str data next-input-character)))))
    :markup-declaration-open
    (cond
      (= (apply str (safe-peek all-input-characters 2)) "--")
      (tokenize (safe-pop all-input-characters 2)
                (assoc tokenizer-state
                       :state :comment-start
                       :token {:type :comment, :data ""}))
      (= (upper-case (apply str (safe-peek all-input-characters 7))) "DOCTYPE")
      (tokenize (safe-pop all-input-characters 7)
                (assoc tokenizer-state :state :DOCTYPE))
      (= (upper-case (apply str (safe-peek all-input-characters 7))) "[CDATA[")
      (tokenize (safe-pop all-input-characters 7)
                (assoc tokenizer-state
                       :state :bogus-comment
                       :token {:type :comment,
                               :data "[CDATA[",
                               :errors (list :cdata-in-html-content)}))
      :else (tokenize
             all-input-characters
             (assoc tokenizer-state
                    :state :bogus-comment
                    :token {:type :comment,
                            :data "",
                            :errors (list :incorrectly-opened-comment)})))
    :comment-start
    (condp consume next-input-character
      \- (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :comment-start-dash))
      \> (cons
          (assoc token :errors (conj errors :abrupt-closing-of-empty-comment))
          (tokenize remaining-input-characters
                    (assoc tokenizer-state :state :data :token nil)))
      (tokenize all-input-characters (assoc tokenizer-state :state :comment)))
    :comment-start-dash
    (condp consume next-input-character
      \- (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :comment-end))
      \> (cons
          (assoc token :errors (conj errors :abrupt-closing-of-empty-comment))
          (tokenize remaining-input-characters
                    (assoc tokenizer-state :state :data :token nil)))
      nil (list (assoc token :errors (conj errors :eof-in-comment))
                {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :comment
                       :token (assoc token :data (str data \-)))))
    :comment
    (condp consume next-input-character
      \< (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :comment-less-than-sign
                          :token
                          (assoc token :data (str data next-input-character))))
      \- (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :comment-end-dash))
      \u0000
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :state :comment-less-than-sign
              :token (assoc token
                            :data (str data \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      nil (list (assoc token :errors (conj errors :eof-in-comment))
                {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token :data (str data next-input-character)))))
    :comment-less-than-sign
    (condp consume next-input-character
      \! (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :comment-less-than-sign-bang
                          :token
                          (assoc token :data (str data next-input-character))))
      \< (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :token
                          (assoc token :data (str data next-input-character))))
      (tokenize all-input-characters (assoc tokenizer-state :state :comment)))
    :comment-less-than-sign-bang
    (condp consume next-input-character
      \- (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :comment-less-than-sign-bang-dash))
      (tokenize all-input-characters (assoc tokenizer-state :state :comment)))
    :comment-less-than-sign-bang-dash
    (condp consume next-input-character
      \- (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :comment-less-than-sign-bang-dash-dash))
      (tokenize all-input-characters
                (assoc tokenizer-state :state :comment-end-dash)))
    :comment-less-than-sign-bang-dash-dash
    (condp consume next-input-character
      #{\> nil} (tokenize all-input-characters
                          (assoc tokenizer-state :state :comment-end))
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :comment-end
                       :token
                       (assoc token :errors (conj errors :nested-comment)))))
    :comment-end-dash
    (condp consume next-input-character
      \- (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :comment-end))
      nil (list (assoc token :errors (conj errors :eof-in-comment))
                {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :comment
                       :token (assoc token :data (str data \-)))))
    :comment-end
    (condp consume next-input-character
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      \! (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :comment-end-bang))
      \- (tokenize
          remaining-input-characters
          (assoc tokenizer-state :token (assoc token :data (str data \-))))
      nil (list (assoc token :errors (conj errors :eof-in-comment))
                {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :comment
                       :token (assoc token :data (str data \- \-)))))
    :comment-end-bang
    (condp consume next-input-character
      \- (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :comment-end-dash
                          :token (assoc token :data (str data \- \- \!))))
      \> (cons (assoc token :errors (conj errors :incorrectly-closed-comment))
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data)))
      nil (list (assoc token :errors (conj errors :eof-in-comment))
                {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :comment
                       :token (assoc token :data (str data \- \- \!)))))
    :DOCTYPE
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      #"[A-Z]" (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :DOCTYPE-name
                                :token {:type :DOCTYPE,
                                        :name (str (Character/toLowerCase
                                                    next-input-character))}))
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :state :DOCTYPE-name
                     :token {:type :DOCTYPE,
                             :name (str \uFFFD),
                             :errors (list :unexpected-null-character)}))
      \> (cons {:type :DOCTYPE,
                :errors (list :missing-doctype-name),
                :force-quirks :on}
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data)))
      nil (list
           {:type :DOCTYPE, :errors (list :eof-in-doctype), :force-quirks :on}
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :state :DOCTYPE-name
                       :token {:type :DOCTYPE,
                               :name (str next-input-character)})))
    :DOCTYPE-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (tokenize remaining-input-characters
                (assoc tokenizer-state :state :after-DOCTYPE-name))
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      #"[A-Z]"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :name
                     (str name (Character/toLowerCase next-input-character)))))
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :token
                     (assoc token
                            :name (str name \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :token
                       (assoc token :name (str name next-input-character)))))
    :after-DOCTYPE-name
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :eof, :tokenizer-state tokenizer-state})
      (cond
        (= (upper-case (apply str (safe-peek all-input-characters 6))) "PUBLIC")
        (tokenize (safe-pop all-input-characters 6)
                  (assoc tokenizer-state :state :after-DOCTYPE-public-keyword))
        (= (upper-case (apply str (safe-peek all-input-characters 6))) "SYSTEM")
        (tokenize (safe-pop all-input-characters 6)
                  (assoc tokenizer-state :state :after-DOCTYPE-system-keyword))
        :else
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state :bogus-comment
                :token (assoc token
                              :errors
                              (conj
                               errors
                               :invalid-character-sequence-after-doctype-name)
                              :force-quirks :on)))))
    :after-DOCTYPE-public-keyword
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state :state :before-DOCTYPE-public-identifier))
      \"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :state :DOCTYPE-public-identifier-double-quoted
              :token (assoc
                      token
                      :public-identifier ""
                      :errors
                      (conj errors
                            :missing-whitespace-after-doctype-public-keyword))))
      \'
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :state :DOCTYPE-public-identifier-single-quoted
              :token (assoc
                      token
                      :public-identifier ""
                      :errors
                      (conj errors
                            :missing-whitespace-after-doctype-public-keyword))))
      \> (cons (assoc token
                      :errors (conj errors :missing-doctype-public-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :bogus-DOCTYPE
              :token (assoc token
                            :errors
                            (conj
                             errors
                             :missing-quote-before-doctype-public-identifier)
                            :force-quirks :on))))
    :before-DOCTYPE-public-identifier
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      \" (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :DOCTYPE-public-identifier-double-quoted
                          :token (assoc token :public-identifier "")))
      \' (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :DOCTYPE-public-identifier-single-quoted
                          :token (assoc token :public-identifier "")))
      \> (cons (assoc token
                      :errors (conj errors :missing-doctype-public-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :bogus-DOCTYPE
              :token (assoc token
                            :errors
                            (conj
                             errors
                             :missing-quote-before-doctype-public-identifier)
                            :force-quirks :on))))
    :DOCTYPE-public-identifier-double-quoted
    (condp consume next-input-character
      \" (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :after-DOCTYPE-public-identifier))
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :token
                     (assoc token
                            :public-identifier (str public-identifier \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      \> (cons (assoc token
                      :errors (conj errors :abrupt-doctype-public-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :public-identifier
                     (str (:public-identifier token) next-input-character)))))
    :DOCTYPE-public-identifier-single-quoted
    (condp consume next-input-character
      \' (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :after-DOCTYPE-public-identifier))
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :token
                     (assoc token
                            :public-identifier (str public-identifier \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      \> (cons (assoc token
                      :errors (conj errors :abrupt-doctype-public-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :public-identifier
                     (str (:public-identifier token) next-input-character)))))
    :after-DOCTYPE-public-identifier
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :state
                       :between-DOCTYPE-public-and-system-identifiers))
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      \"
      (tokenize
       remaining-input-characters
       (assoc
        tokenizer-state
        :state :DOCTYPE-system-identifier-double-quoted
        :token
        (assoc
         token
         :system-identifier ""
         :errors
         (conj
          errors
          :missing-whitespace-between-doctype-public-and-system-identifiers))))
      \'
      (tokenize
       remaining-input-characters
       (assoc
        tokenizer-state
        :state :DOCTYPE-system-identifier-single-quoted
        :token
        (assoc
         token
         :system-identifier ""
         :errors
         (conj
          errors
          :missing-whitespace-between-doctype-public-and-system-identifiers))))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc
        tokenizer-state
        :state :bogus-DOCTYPE
        :token
        (assoc
         token
         :errors
         (conj
          errors
          :missing-whitespace-between-doctype-public-and-system-identifiers)
         :force-quirks :on))))
    :between-DOCTYPE-public-and-system-identifiers
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      \" (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :DOCTYPE-system-identifier-double-quoted
                          :token (assoc token :system-identifier "")))
      \' (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :DOCTYPE-system-identifier-single-quoted
                          :token (assoc token :system-identifier "")))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :bogus-DOCTYPE
              :token (assoc token
                            :errors
                            (conj
                             errors
                             :missing-quote-before-doctype-system-identifier)
                            :force-quirks :on))))
    :after-DOCTYPE-system-keyword
    (condp consume next-input-character
      #{\tab \u000A \u000C \space}
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state :state :before-DOCTYPE-system-identifier))
      \"
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :state :DOCTYPE-system-identifier-double-quoted
              :token (assoc
                      token
                      :system-identifier ""
                      :errors
                      (conj errors
                            :missing-whitespace-after-doctype-system-keyword))))
      \'
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :state :DOCTYPE-system-identifier-single-quoted
              :token (assoc
                      token
                      :system-identifier ""
                      :errors
                      (conj errors
                            :missing-whitespace-after-doctype-system-keyword))))
      \> (cons (assoc token
                      :errors (conj errors :missing-doctype-system-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :bogus-DOCTYPE
              :token (assoc token
                            :errors
                            (conj
                             errors
                             :missing-quote-before-doctype-system-identifier)
                            :force-quirks :on))))
    :before-DOCTYPE-system-identifier
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      \" (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :DOCTYPE-system-identifier-double-quoted
                          :token (assoc token :system-identifier "")))
      \' (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :DOCTYPE-system-identifier-single-quoted
                          :token (assoc token :system-identifier "")))
      \> (cons (assoc token
                      :errors (conj errors :missing-doctype-system-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :bogus-DOCTYPE
              :token (assoc token
                            :errors
                            (conj
                             errors
                             :missing-quote-before-doctype-system-identifier)
                            :force-quirks :on))))
    :DOCTYPE-system-identifier-double-quoted
    (condp consume next-input-character
      \" (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :after-DOCTYPE-system-identifier))
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :token
                     (assoc token
                            :system-identifier (str system-identifier \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      \> (cons (assoc token
                      :errors (conj errors :abrupt-doctype-system-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :system-identifier
                     (str (:system-identifier token) next-input-character)))))
    :DOCTYPE-system-identifier-single-quoted
    (condp consume next-input-character
      \' (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :after-DOCTYPE-system-identifier))
      \u0000 (tokenize
              remaining-input-characters
              (assoc tokenizer-state
                     :token
                     (assoc token
                            :system-identifier (str system-identifier \uFFFD)
                            :errors (conj errors :unexpected-null-character))))
      \> (cons (assoc token
                      :errors (conj errors :abrupt-doctype-system-identifier)
                      :force-quirks :on)
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       remaining-input-characters
       (assoc tokenizer-state
              :token
              (assoc token
                     :system-identifier
                     (str (:system-identifier token) next-input-character)))))
    :after-DOCTYPE-system-identifier
    (condp consume next-input-character
      #{\tab \u000A \u000C \space} (tokenize remaining-input-characters
                                             tokenizer-state)
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data :token nil)))
      nil (list
           (assoc token :errors (conj errors :eof-in-doctype) :force-quirks :on)
           {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize
       all-input-characters
       (assoc tokenizer-state
              :state :bogus-DOCTYPE
              :token
              (assoc token
                     :errors
                     (conj
                      errors
                      :unexpected-character-after-doctype-system-identifier)))))
    :bogus-DOCTYPE
    (condp consume next-input-character
      \> (cons token
               (tokenize remaining-input-characters
                         (assoc tokenizer-state :state :data)))
      \u0000 (tokenize
              remaining-input-characters
              (assoc
               tokenizer-state
               :token
               (assoc token :errors (conj errors :unexpected-null-character))))
      nil (list token {:type :EOF, :tokenizer-state tokenizer-state})
      (tokenize remaining-input-characters tokenizer-state))
    :CDATA-section
    (condp consume next-input-character
      \] (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :CDATA-section-bracket))
      nil (list {:type :EOF,
                 :tokenizer-state tokenizer-state,
                 :errors (list :eof-in-cdata)})
      (cons {:type :character, :data next-input-character}
            (tokenize remaining-input-characters tokenizer-state)))
    :CDATA-section-bracket
    (condp consume next-input-character
      \] (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :CDATA-section-end))
      (cons {:type :character, :data \]}
            (tokenize all-input-characters
                      (assoc tokenizer-state :state :CDATA-section))))
    :CDATA-section-end
    (condp consume next-input-character
      \] (cons {:type :character, :data \]}
               (tokenize remaining-input-characters tokenizer-state))
      \> (tokenize remaining-input-characters
                   (assoc tokenizer-state :state :data))
      (cons {:type :character, :data \]}
            (cons {:type :character, :data \]}
                  (tokenize all-input-characters
                            (assoc tokenizer-state :state :CDATA-section)))))
    :character-reference
    (condp consume next-input-character
      #"[A-Za-z0-9]" (tokenize all-input-characters
                               (assoc tokenizer-state
                                      :state :named-character-reference
                                      :temporary-buffer "&"))
      \# (tokenize remaining-input-characters
                   (assoc tokenizer-state
                          :state :numeric-character-reference
                          :temporary-buffer "&#"))
      (if attribute
        (tokenize all-input-characters
                  (assoc tokenizer-state
                         :state return-state
                         :token (assoc token
                                       :attributes
                                       (conj remaining-attributes
                                             (assoc attribute
                                                    :value
                                                    (str (:value attribute)
                                                         temporary-buffer))))
                         :temporary-buffer nil
                         :return-state nil))
        (concat (map #(-> {:type :character, :data %})
                     (str \< \/ temporary-buffer))
                (tokenize all-input-characters
                          (assoc tokenizer-state
                                 :state return-state
                                 :temporary-buffer nil
                                 :return-state nil)))))
    :named-character-reference
    (let [named-character-reference (named-character-reference-find
                                     all-input-characters)]
      (if named-character-reference
        (let [next-input-character
              (last (safe-peek all-input-characters
                               (+ (count named-character-reference) 1)))]
          (if (and attribute
                   (not= (last named-character-reference) \;)
                   (or (= next-input-character \=)
                       (re-matches #"[A-Za-z0-9]" (str next-input-character))))
            (tokenize
             (safe-pop all-input-characters (count named-character-reference))
             (assoc tokenizer-state
                    :state return-state
                    :token (assoc
                            token
                            :attributes
                            (conj remaining-attributes
                                  (assoc attribute
                                         :value
                                         (str (:value attribute)
                                              named-character-reference))))
                    :temporary-buffer nil
                    :return-state nil))
            (if attribute
              (tokenize
               (safe-pop all-input-characters (count named-character-reference))
               (assoc tokenizer-state
                      :state return-state
                      :token
                      (assoc
                       token
                       :attributes
                       (conj
                        remaining-attributes
                        (assoc
                         attribute
                         :value (apply str
                                       (:value attribute)
                                       (map char
                                            (get named-character-references
                                                 named-character-reference)))
                         :errors
                         (if (= (last named-character-reference) \;)
                           nil
                           (list
                            :missing-semicolon-after-character-reference)))))
                      :temporary-buffer nil
                      :return-state nil))
              (concat
               (map #(-> {:type :character, :data %})
                    (apply str
                           (map char
                                (get named-character-references
                                     named-character-reference))))
               (if (= (last named-character-reference) \;)
                 nil
                 (list {:type :error,
                        :data :missing-semicolon-after-character-reference}))
               (tokenize (safe-pop all-input-characters
                                   (count named-character-reference))
                         (assoc tokenizer-state
                                :state return-state
                                :temporary-buffer nil
                                :return-state nil))))))
        (if attribute
          (tokenize
           all-input-characters
           (assoc tokenizer-state
                  :state :ambiguous-ampersand
                  :token (assoc token
                                :attributes
                                (conj remaining-attributes
                                      (assoc attribute
                                             :value
                                             (apply str
                                                    (:value attribute)
                                                    temporary-buffer))))
                  :temporary-buffer nil))
          (concat (map #(-> {:type :character, :data %})
                       temporary-buffer)
                  (tokenize all-input-characters
                            (assoc tokenizer-state
                                   :state :ambiguous-ampersand
                                   :temporary-buffer nil))))))
    :ambiguous-ampersand
    (condp consume next-input-character
      #"[A-Za-z0-9]"
      (if attribute
        (tokenize remaining-input-characters
                  (assoc tokenizer-state
                         :token
                         (assoc token
                                :attributes
                                (conj remaining-attributes
                                      (assoc attribute
                                             :value
                                             (str (:value attribute)
                                                  next-input-character))))))
        (cons {:type :character, :data next-input-character}
              (tokenize remaining-input-characters tokenizer-state)))
      \;
      (if attribute
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state return-state
                :token
                (assoc token
                       :attributes
                       (conj remaining-attributes
                             (assoc attribute
                                    :errors
                                    (conj (:errors attribute)
                                          :unknown-named-character-reference))))
                :return-state nil))
        (cons {:type :error, :data :unknown-named-character-reference}
              (tokenize
               all-input-characters
               (assoc tokenizer-state :state return-state :return-state nil))))
      (tokenize all-input-characters
                (assoc tokenizer-state :state return-state :return-state nil)))
    :numeric-character-reference
    (condp consume next-input-character
      #{\x \X} (tokenize remaining-input-characters
                         (assoc tokenizer-state
                                :state :hexadecimal-character-reference-start
                                :character-reference-code 0
                                :temporary-buffer (str temporary-buffer
                                                       next-input-character)))
      (tokenize all-input-characters
                (assoc tokenizer-state
                       :state :decimal-character-reference-start
                       :character-reference-code 0)))
    :hexadecimal-character-reference-start
    (condp consume next-input-character
      #"[A-Fa-f0-9]"
      (tokenize all-input-characters
                (assoc tokenizer-state :state :hexadecimal-character-reference))
      (if attribute
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state return-state
                :token
                (assoc
                 token
                 :attributes
                 (conj
                  remaining-attributes
                  (assoc attribute
                         :value (str (:value attribute) temporary-buffer)
                         :errors
                         (conj
                          (:errors attribute)
                          :absence-of-digits-in-numeric-character-reference))))
                :temporary-buffer nil
                :return-state nil))
        (concat (map #(-> {:type :character, :data %})
                     (str \< \/ temporary-buffer))
                (list {:type :error,
                       :data :absence-of-digits-in-numeric-character-reference})
                (tokenize all-input-characters
                          (assoc tokenizer-state
                                 :state return-state
                                 :temporary-buffer nil
                                 :return-state nil)))))
    :decimal-character-reference-start
    (condp consume next-input-character
      #"[0-9]" (tokenize
                all-input-characters
                (assoc tokenizer-state :state :decimal-character-reference))
      (if attribute
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state return-state
                :token
                (assoc
                 token
                 :attributes
                 (conj
                  remaining-attributes
                  (assoc attribute
                         :value (str (:value attribute) temporary-buffer)
                         :errors
                         (conj
                          (:errors attribute)
                          :absence-of-digits-in-numeric-character-reference))))
                :temporary-buffer nil
                :return-state nil))
        (concat (map #(-> {:type :character, :data %})
                     (str \< \/ temporary-buffer))
                (list {:type :error,
                       :data :absence-of-digits-in-numeric-character-reference})
                (tokenize all-input-characters
                          (assoc tokenizer-state
                                 :state return-state
                                 :temporary-buffer nil
                                 :return-state nil)))))
    :hexadecimal-character-reference
    (condp consume next-input-character
      #"[A-Fa-f0-9]"
      (tokenize remaining-input-characters
                (assoc tokenizer-state
                       :character-reference-code
                       (+ (* 16 character-reference-code)
                          (Integer/parseInt (str next-input-character) 16))))
      \; (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :numeric-character-reference-end))
      (if attribute
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state :numeric-character-reference-end
                :token
                (assoc
                 token
                 :attributes
                 (conj remaining-attributes
                       (assoc
                        attribute
                        :errors
                        (conj (:errors attribute)
                              :missing-semicolon-after-character-reference))))))
        (cons {:type :error, :data :missing-semicolon-after-character-reference}
              (tokenize all-input-characters
                        (assoc tokenizer-state
                               :state
                               :numeric-character-reference-end)))))
    :decimal-character-reference
    (condp consume next-input-character
      #"[0-9]" (tokenize
                remaining-input-characters
                (assoc tokenizer-state
                       :character-reference-code
                       (+ (* 10 character-reference-code)
                          (Integer/parseInt (str next-input-character) 10))))
      \; (tokenize
          remaining-input-characters
          (assoc tokenizer-state :state :numeric-character-reference-end))
      (if attribute
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state :numeric-character-reference-end
                :token
                (assoc
                 token
                 :attributes
                 (conj remaining-attributes
                       (assoc
                        attribute
                        :errors
                        (conj (:errors attribute)
                              :missing-semicolon-after-character-reference))))))
        (cons {:type :error, :data :missing-semicolon-after-character-reference}
              (tokenize all-input-characters
                        (assoc tokenizer-state
                               :state
                               :numeric-character-reference-end)))))
    :numeric-character-reference-end
    (let [[error code-point]
          (condp consume character-reference-code
            null-character-reference? [:null-character-reference 0xFFFD]
            character-reference-outside-unicode-range?
            [:character-reference-outside-unicode-range 0xFFFD]
            surrogate-character-reference? [:surrogate-character-reference
                                            0xFFFD]
            noncharacter-character-reference? [:noncharacter-character-reference
                                               character-reference-code]
            control-character-reference? [:control-character-reference
                                          character-reference-code]
            [nil
             (get numeric-character-references
                  character-reference-code
                  character-reference-code)])]
      (if attribute
        (tokenize
         all-input-characters
         (assoc tokenizer-state
                :state return-state
                :token (assoc
                        token
                        :attributes
                        (conj remaining-attributes
                              (assoc attribute
                                     :value (str (:value attribute)
                                                 (char code-point))
                                     :errors (conj (:errors attribute) error))))
                :temporary-buffer nil
                :return-state nil))
        (cons {:type :character, :data (char code-point), :errors (list error)}
              (tokenize all-input-characters
                        (assoc tokenizer-state
                               :state return-state
                               :temporary-buffer nil
                               :return-state nil)))))))
