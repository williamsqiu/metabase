(ns metabase.spec
  "Shared specs used across Metabase."
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(s/def :metabase/positive-int
  (s/and int? pos?))

(s/def :metabase/positive-number
  (s/and number? pos?))

(s/def :metabase/non-negative-int
  (s/and int? (complement neg?)))

(s/def :metabase/non-blank-string
  (s/and string? (complement str/blank?)))
