(ns metabase.mbql.spec
  "Spec for a valid *normalized* MBQL query. This is also the definitive grammar for MBQL, wow!"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [metabase.models.query :as query]
            [metabase.util.date :as du]))

(defn- clause-name [x]
  (when (sequential? x)
    (first x)))

(defn- is-clause? [clause]
  (fn [x]
    (= (clause-name x) clause)))

(defmacro ^:private def-one-of {:style/indent 1} [spec-name & clause-name-spec-pairs]
  (let [multimethod-name (symbol (str "one-of-" (-> (str (namespace spec-name) \_ (name spec-name))
                                                    munge
                                                    (str/replace #"\." "_"))))]
    `(do
       (defmulti ~(vary-meta multimethod-name assoc :private true) clause-name)

       ~@(for [[clause spec] (partition 2 clause-name-spec-pairs)]
           `(defmethod ~multimethod-name ~clause [~'_] ~spec))

       (s/def ~spec-name
         (s/multi-spec ~multimethod-name :clause-name)))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                   Primitives                                                   |
;;; +----------------------------------------------------------------------------------------------------------------+

(s/def ::positive-int
  (s/and int? pos?))

(s/def ::positive-number
  (s/and number? pos?))

(s/def ::non-negative-int
  (s/and int? (complement neg?)))

(s/def ::non-blank-string
  (s/and string? (complement str/blank?)))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                   DateTimes                                                    |
;;; +----------------------------------------------------------------------------------------------------------------+

(s/def ::datetime-unit
  #{:default :minute :minute-of-hour :hour :hour-of-day :day :day-of-week :day-of-month :day-of-year :week :week-of-year
    :month :month-of-year :quarter :quarter-of-year :year})

(s/def ::relative-datetime-unit
  #{:minute :hour :day :week :month :quarter :year})

(s/def ::datetime-literal-string
  du/date-string?)

(defmulti ^:private datetime-literal class)

(defmethod datetime-literal java.sql.Date  [_] (constantly true))
(defmethod datetime-literal java.util.Date [_] (constantly true))
(defmethod datetime-literal String         [_] ::datetime-literal-string)

(s/def ::datetime-literal
  (s/multi-spec datetime-literal :class))


(s/def :mbql/relative-datetime
  (s/or
   :relative-datetime-with-2-args (partial = [:relative-datetime :current])
   :relative-datetime-with-3-args (s/tuple (partial = :relative-datetime), int?, ::relative-datetime-unit)))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                  MBQL Fields                                                   |
;;; +----------------------------------------------------------------------------------------------------------------+

(s/def :mbql.field/type
  #(isa? % :type/*))

;; Normal lowest-level Field clauses refer to a Field either by ID or by name

;; [:field-id <id>]
(s/def :mbql.field/field-id
  (s/tuple (partial = :field-id), ::positive-int))

;; [:field-literal <field-name> <field-type>]
(s/def :mbql.field/field-literal
  (s/tuple (partial = :field-literal), ::non-blank-string, :mbql.field/type))

(def-one-of :mbql.field/field-id-or-literal
  :field-id :mbql.field/field-id, :field-literal :mbql.field/field-literal)

;; [:fk-> <source-field> <dest-field>]
;;
;; Both args in `[:fk-> <source-field> <dest-field>]` are implict `:field-ids`. E.g.
;;
;;   [:fk-> 10 20] --[NORMALIZE]--> [:fk-> [:field-id 10] [:field-id 20]]
(s/def :mbql.field/fk->
  (s/tuple (partial = :fk->), :mbql.field/field-id-or-literal, :mbql.field/field-id-or-literal))

;; [:expression <expression-name>]
;;
;; Expression *references* refer to a something in the `:expressions` clause, e.g. something like `[:+ [:field-id 1]
;; [:field-id 2]]`
(s/def :mbql.field/expression-ref
  (s/tuple (partial = :expression), ::non-blank-string))

;; [:datetime-field <field> <unit>]
;;
;; datetime-field wraps a Field to give it an explict datetime bucketing unit, e.g.
;; [:datetime-field [:field-id 10] :day]
(def-one-of :mbql.field/datetime-wrappable-field
  :field-id      :mbql.field/field-id
  :field-literal :mbql.field/:field-literal
  :fk->          :mbql.field/fk->
  :expression    :mbql.field/expression-ref)

(s/def :mbql.field/datetime-field
  (s/tuple (partial = :datetime-field), :mbql.field/datetime-wrappable-field, ::datetime-unit))

;; [:binning-strategy <field> <strategy> <strategy-arg?>]
;;
;; Depending on the strategy, strategy-arg is either disallowed or must conform to different specs, which is why we
;; use the multimethod below
;;
;; binning strategy can wrap any of the above clauses, but again, not another binning strategy clause
(def-one-of :mbql.field/binnable-field
  :field-id       :mbql.field/field-id
  :field-literal  :mbql.field/field-literal
  :fk->           :mbql.field/fk->
  :expression     :mbql.field/expression-ref
  :datetime-field :mbql.field/datetime-field)

(defmulti ^:private binning-strategy #(name (nth % 2))) ; wrap in name so `:default` isn't the default case

(defmethod binning-strategy "default"   [_]
  (s/tuple (partial = :binning-strategy), :mbql.field/binnable-field (partial = :default)))

(defmethod binning-strategy "num-bins"  [_]
  (s/tuple (partial = :binning-strategy), :mbql.field/binnable-field (partial = :num-bins), ::positive-int))

(defmethod binning-strategy "bin-width" [_]
  (s/tuple (partial = :binning-strategy), :mbql.field/binnable-field (partial = :bin-width), ::positive-number))

(s/def :mbql.field/binning-strategy
  (s/and (is-clause? :binning-strategy)
         (s/multi-spec binning-strategy :strategy)))

(def-one-of :mbql/field
  :field-id         :mbql.field/field-id
  :field-literal    :mbql.field/field-literal
  :expression       :mbql.field/expression-ref
  :datetime-field   :mbql.field/datetime-field
  :binning-strategy :mbql.field/binning-strategy)


;; aggregate field reference refers to an aggregation, e.g.
;;
;;    {:aggregation [[:count]]
;;     :order-by    [[:asc [:aggregation 0]]]} ;; refers to the 0th aggregation, `:count`
;;
;; Currently aggregate Field references can only be used inside order-by clauses. In the future once we support SQL
;; `HAVING` we can allow them in filter clauses too
;;
;; TODO - shouldn't we allow composing aggregations in expressions? e.g.
;;
;;    {:order-by [[:asc [:+ [:aggregation 0] [:aggregation 1]]]]}
;;
;; TODO - it would be nice if we could check that there's actually an aggregation with the corresponding index,
;; wouldn't it
(s/def :mbql.field/aggregation-ref
  (s/tuple (partial = :aggregation), ::non-negative-int)) ; aggregation clause index

(s/def :mbql.field/field-or-aggregation-ref
  (s/or
   :field           :mbql/field
   :aggregation-ref :mbql.field/aggregation-ref))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                  MBQL Clauses                                                  |
;;; +----------------------------------------------------------------------------------------------------------------+

;;; -------------------------------------------------- Expressions ---------------------------------------------------

(def-one-of :mbql.expression/def
  :+ :mbql.expression/+, :- :mbql.expression/-, :* :mbql.expression/*, :* :mbql.expression/*)

(s/def :mbql.expression/arg
  (s/or
   :number     number?
   :expression :mbql.expression/def
   :field      :mbql/field))

(s/def :mbql.expression/+ (s/cat :name (partial = :+), :x :mbql.expression/arg, :y (s/+ :mbql.expression/arg)))
(s/def :mbql.expression/- (s/cat :name (partial = :-), :x :mbql.expression/arg, :y (s/+ :mbql.expression/arg)))
(s/def :mbql.expression// (s/cat :name (partial = :/), :x :mbql.expression/arg, :y (s/+ :mbql.expression/arg)))
(s/def :mbql.expression/* (s/cat :name (partial = :*), :x :mbql.expression/arg, :y (s/+ :mbql.expression/arg)))


;;; -------------------------------------------------- Aggregations --------------------------------------------------

(s/def :mbql.aggregation/field-or-expression-def
  (s/or
   :field          :mbql/field
   :expression-def :mbql.expression/def))

;; For all of the 'normal' Aggregations below (excluding Metrics) fields are implicit Field IDs

(s/def :mbql.aggregation/count     (s/cat :name (partial =  :count),     :field (s/? :mbql/field)))
(s/def :mbql.aggregation/cum-count (s/cat :name (partial =  :cum-count), :field (s/? :mbql/field)))

;; technically aggregations besides count can also accept expressions as args, e.g.
;;
;;    [[:sum [:+ [:field-id 1] [:field-id 2]]]]
;;
;; Which is equivalent to SQL:
;;
;;    SUM(field_1 + field_2)

(s/def :mbql.aggregation/avg,      (s/tuple (partial = :avg),      :mbql.aggregation/field-or-expression-def))
(s/def :mbql.aggregation/cum-sum,  (s/tuple (partial = :cum-sum),  :mbql.aggregation/field-or-expression-def))
(s/def :mbql.aggregation/distinct, (s/tuple (partial = :distinct), :mbql.aggregation/field-or-expression-def))
(s/def :mbql.aggregation/stddev,   (s/tuple (partial = :stddev),   :mbql.aggregation/field-or-expression-def))
(s/def :mbql.aggregation/sum,      (s/tuple (partial = :sum),      :mbql.aggregation/field-or-expression-def))
(s/def :mbql.aggregation/min,      (s/tuple (partial = :min),      :mbql.aggregation/field-or-expression-def))
(s/def :mbql.aggregation/max,      (s/tuple (partial = :max),      :mbql.aggregation/field-or-expression-def))

;; Metrics are just 'macros' (placeholders for other aggregations with optional filter and breakout clauses) that get
;; expanded to other aggregations/etc. in the expand-macros middleware
;;
;; METRICS WITH STRING IDS, e.g. `[:metric "ga:sessions"]`, are Google Analytics metrics, not Metabase metrics! They
;; pass straight thru to the GA query processor.
(s/def :mbql.aggregation/metric
  (s/tuple
   (partial = :metric)
   (s/or :metric-id      ::positive-int
         :ga-metric-name ::non-blank-string)))

;; the following are definitions for expression aggregations, e.g. [:+ [:sum [:field-id 10]] [:sum [:field-id 20]]]

(s/def :mbql.aggregation/expression-arg
  (s/or
   :number      number?
   :aggregation :mbql.aggregation/unnamed-aggregation))

(s/def :mbql.aggregation/+
  (s/cat
   :name (partial = :+)
   :x    :mbql.aggregation/expression-arg
   :y    (s/+ :mbql.aggregation/expression-arg)))

(s/def :mbql.aggregation/-
  (s/cat
   :name (partial = :-)
   :x    :mbql.aggregation/expression-arg
   :y    (s/+ :mbql.aggregation/expression-arg)))

(s/def :mbql.aggregation/*
  (s/cat
   :name (partial = :*)
   :x    :mbql.aggregation/expression-arg
   :y    (s/+ :mbql.aggregation/expression-arg)))

(s/def :mbql.aggregation//
  (s/cat
   :name (partial = :/)
   :x    :mbql.aggregation/expression-arg
   :y    (s/+ :mbql.aggregation/expression-arg)))


(def-one-of :mbql.aggregation/unnamed-aggregation
  :count     :mbql.aggregation/count
  :avg       :mbql.aggregation/avg
  :cum-count :mbql.aggregation/cum-count
  :cum-sum   :mbql.aggregation/cum-sum
  :distinct  :mbql.aggregation/distinct
  :stddev    :mbql.aggregation/stddev
  :sum       :mbql.aggregation/sum
  :min       :mbql.aggregation/min
  :max       :mbql.aggregation/max
  :metric    :mbql.aggregation/metric
  :+         :mbql.aggregation/+
  :-         :mbql.aggregation/-
  :*         :mbql.aggregation/*
  :/         :mbql.aggregation//)

;; any sort of aggregation can be wrapped in a `[:named <ag> <custom-name>]` clause, but you cannot wrap a `:named` in
;; a `:named`
(s/def :mbql.aggregation/named-aggregation
  (s/tuple (partial = :named), :mbql.aggregation/unnamed-aggregation, ::non-blank-string))


(s/def :mbql.aggregation/aggregation
  (s/or :unnamed :mbql.aggregation/unnamed-aggregation
        :named :mbql.aggregation/named-aggregation))

(s/def :mbql/aggregation
  (s/+ :mbql.aggregation/aggregation))

;;; ---------------------------------------------------- Order-By ----------------------------------------------------

;; order-by is just a series of `[<direction> <field>]` clauses like
;;
;;    {:order-by [[:asc [:field-id 1]], [:desc [:field-id 2]]]}
;;
;; Field ID is implicit in these clauses

(s/def :mbql.order-by/asc  (s/tuple (partial = :asc)  :mbql.field/field-or-aggregation-ref))
(s/def :mbql.order-by/desc (s/tuple (partial = :desc) :mbql.field/field-or-aggregation-ref))

(def-one-of :mbql.order-by/order-by
  :asc  :mbql.order-by/asc
  :desc :mbql.order-by/desc)

(s/def :mbql/order-by (s/+ :mbql.order-by/order-by))

;;; ----------------------------------------------------- Filter -----------------------------------------------------

;; [:and <filter> <filter+>]
(s/def :mbql.filter/and (s/cat :name (partial = :and), :x :mbql.filter/filter, :y (s/+ :mbql.filter/filter)))
(s/def :mbql.filter/or  (s/cat :name (partial = :or),  :x :mbql.filter/filter, :y (s/+ :mbql.filter/filter)))

;; [:not <filter>]
(s/def :mbql.filter/not (s/tuple (partial = :not) :mbql.filter/filter))


(s/def :mbql.filter/field-or-relative-datetime
  (s/or
   :field             :mbql/field
   :relative-datetime :mbql/relative-datetime))

;; Things things that make sense in a `=` or `!=` filter, i.e. things that can be compared for equality.
(s/def :mbql.filter/equality-comparible
  (s/or
   :nil               nil?
   :boolean           boolean?
   :number            number?
   :string            string?
   :datetime-literal  ::datetime-literal
   :field             :mbql/field
   :relative-datetime :mbql/relative-datetime))

;; Things that make sense in a filter like `>` or `<`, i.e. things that can be sorted.
(s/def :mbql.filter/order-comaparible
  (s/or
   :number            number?
   :string            string?
   :datetime-literal  ::datetime-literal
   :field             :mbql/field
   :relative-datetime :mbql/relative-datetime))

;; For all of the non-compound Filter clauses below the first arg is an implicit Field ID
;; [:= <field> <field-or-value+>]
(s/def :mbql.filter/=
  (s/cat
   :name             (partial = :=)
   :field            :mbql/field
   :values-or-fields (s/+ :mbql.filter/equality-comparible)))

;; [:!= <field> <field-or-value+>]
(s/def :mbql.filter/!=
  (s/cat
   :name             (partial = :!=)
   :field            :mbql/field
   :values-or-fields (s/+ :mbql.filter/equality-comparible)))

;; [:< <field> <field-or-value>]
(s/def :mbql.filter/<  (s/tuple (partial = :<)  :mbql/field :mbql.filter/order-comparible))
(s/def :mbql.filter/>  (s/tuple (partial = :>)  :mbql/field :mbql.filter/order-comparible))
(s/def :mbql.filter/<= (s/tuple (partial = :<=) :mbql/field :mbql.filter/order-comparible))
(s/def :mbql.filter/>= (s/tuple (partial = :>=) :mbql/field :mbql.filter/order-comparible))

;; [:between <field> <min> <max>]
(s/def :mbql.filter/between
  (s/tuple
   (partial = :between)
   :mbql/field
   :mbql.filter/order-comparible
   :mbql.filter/order-comparible))

;; [:inside <lat-field> <lon-field> <lat-max> <lon-min> <lat-min> <lat-max>]
(s/def :mbql.filter/inside
  (s/tuple
   (partial = :inside)
   :mbql/field
   :mbql/field
   :mbql.filter/order-comparible
   :mbql.filter/order-comparible
   :mbql.filter/order-comparible
   :mbql.filter/order-comparible))

;; [:is-null <field>]
(s/def :mbql.filter/is-null (s/tuple (partial = :is-null)  :mbql/field))
(s/def :mbql.filter/is-null (s/tuple (partial = :not-null) :mbql/field))

(s/def :mbql.filter.string-filter-options/case-sensitive boolean?) ; default true
(s/def :mbql.filter/string-filter-options
  (s/keys :opt-un [:mbql.filter.string-filter-options/case-sensitive]))

(s/def :mbql.filter/string-or-field
  (s/or
   :string string?
   :field  :mbql/field))

(s/def :mbql.filter/starts-with
  (s/cat
   :name  (partial = :starts-with)
   :field :mbql/field
   :arg   :mbql.filter/string-or-field
   :opts  (s/? :mbql.filter/string-filter-options)))

(s/def :mbql.filter/ends-with
  (s/cat
   :name  (partial = :ends-with)
   :field :mbql/field
   :arg   :mbql.filter/string-or-field
   :opts  (s/? :mbql.filter/string-filter-options)))

(s/def :mbql.filter/contains
  (s/cat
   :name  (partial = :contains)
   :field :mbql/field
   :arg   :mbql.filter/string-or-field
   :opts  (s/? :mbql.filter/string-filter-options)))

(s/def :mbql.filter/does-not-contain
  (s/cat
   :name  (partial = :does-not-contain)
   :field :mbql/field
   :arg   :mbql.filter/string-or-field
   :opts  (s/? :mbql.filter/string-filter-options)))

(s/def :mbql.filter.time-interval-options/include-current boolean?) ; default false

(s/def :mbql.filter/time-interval-options
  (s/keys :opt-un [:mbql.filter.time-interval-options/include-current]))

;; [:time-interval <n> <unit> <options?>]
(s/def :mbql.filter/time-interval
  (s/cat
   :name    (partial = :time-interval)
   :n       (s/or :int int?, :n #{:current :last :next})
   :unit    ::relative-datetime-unit
   :options (s/? :mbql.filter/time-interval-options)))

;; A segment is a special `macro` that saves some pre-definied filter clause, e.g. [:segment 1]
;; this gets replaced by a normal Filter clause in MBQL macroexpansion
;;
;; It can also be used for GA, which looks something like `[:segment "gaid::-11"]`. GA segments aren't actually MBQL
;; segments and pass-thru to GA.
(s/def :mbql.filter/segment
  (s/tuple
   (partial = :segment)
   (s/or
    :segment-id      ::positive-int
    :ga-segment-name ::non-blank-string)))

(def-one-of :mbql/filter
  :and              :mbql.filter/and
  :or               :mbql.filter/or
  :not              :mbql.filter/not
  :=                :mbql.filter/=
  :!=               :mbql.filter/!=
  :<                :mbql.filter/<
  :>                :mbql.filter/>
  :<=               :mbql.filter/<=
  :>=               :mbql.filter/>=
  :between          :mbql.filter/between
  :inside           :mbql.filter/inside
  :is-null          :mbql.filter/is-null
  :not-null         :mbql.filter/not-null
  :starts-with      :mbql.filter/starts-with
  :ends-with        :mbql.filter/ends-with
  :contains         :mbql.filter/contains
  :does-not-contain :mbql.filter/does-not-contain
  :time-interval    :mbql.filter/time-interval
  :segment          :mbql.filter/segment)


;;; ------------------------------------------------------ Page ------------------------------------------------------

(s/def :mbql.page/page ::non-negative-int)

(s/def :mbql.page/items ::positive-int)

(s/def :mbql/page
  (s/keys :req-un [:mbql.page/page :mbql.page/items]))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                    Queries                                                     |
;;; +----------------------------------------------------------------------------------------------------------------+

;;; -------------------------------------------------- Native Query --------------------------------------------------

;; TODO - schemas for template tags and dimensions live in `metabase.query-processor.middleware.parameters.sql`. Move
;; them here when we get the chance.

(s/def :native/query
  any?)

;; any? for now until we move over the stuff from the parameters middleware
(s/def :native/template-tag-definition
  any?)

(s/def :native/template-tags
  (s/map-of ::non-blank-string :native/template-tag-definition))

;; collection (table) this query should run against. Needed for MongoDB
(s/def :native/collection
  (s/or
   :nil             nil?
   :collection-name ::non-blank-string))

(s/def :query/native
  (s/keys
   :req-un [:native/query]
   :opt-un [:native/template-tags
            :native/collection]))


;;; --------------------------------------------------- MBQL Query ---------------------------------------------------

(s/def :mbql.source-query.native/native
  :native/query)

;; spec for a native source query is the same as a native inner query except the key `:native` gets used in place of
;; `:query` (ick)
(s/def :mbql.source-query/native
  (s/keys
   :req-un [:mbql.source-query.native/native]
   :opt-un [:native/template-tags
            :native/collection]))

;; spec for an MBQL source query is the same as MBQL inner query
(s/def :mbql.source-query/mbql
  :query/query)

(defmulti ^:private source-query #(contains? % :native))

(defmethod source-query true  [_] :mbql.source-query/native)
(defmethod source-query false [_] :mbql.source-query/mbql)

(s/def :mbql/source-query
  (s/multi-spec source-query :native?))


(s/def :mbql/source-table
  (s/or
   :id          ::positive-int
   :source-card (partial re-matches #"^card__[1-9]\d*$")))


(s/def :mbql/breakout    (s/+ :mbql/field))
(s/def :mbql/expressions (s/map-of keyword? :mbql.expression/def)) ; TODO - I think expressions keys should be strings
(s/def :mbql/fields      (s/+ :mbql/field))
(s/def :mbql/limit       ::non-negative-int)

(defn- xor [x y]
  (and (or x y)
       (not (and x y))))

(defn- query-has-one-source? [query]
  (xor (contains? query :source-table) (contains? query :source-query)))

(s/def :query/query
  (s/and
   (s/keys
    :opt-un [:mbql/source-query
             :mbql/source-table
             :mbql/aggregation
             :mbql/breakout
             :mbql/expressions
             :mbql/fields
             :mbql/filter
             :mbql/limit
             :mbql/order-by
             :mbql/page])
   query-has-one-source?))


;;; ----------------------------------------------------- Params -----------------------------------------------------

;; any? for now until we move over the stuff from the parameters middleware
(s/def :query.parameters/parameter
  any?)

(s/def :query/parameters
  (s/* :query.parameters/parameter))


;;; ---------------------------------------------------- Options -----------------------------------------------------

;; The timezone the query should be ran in, overriding the default report timezone for the instance.
(s/def :query.settings/report-timezone ::non-blank-string)

;; Options that tweak the behavior of the query processor.
(s/def :query/settings
  (s/keys :opt-un [:query.settings/report-timezone]))


;; maximum number of results to allow for a query with aggregations
(s/def :query.constraints/max-results ::non-negative-int)

;; maximum number of results to allow for a query with no aggregations
(s/def :query.constraints/max-results-bare-rows ::non-negative-int)

;; Additional constraints added to a query limiting the maximum number of rows that can be returned. Mostly useful
;; because native queries don't support the MBQL `:limit` clause. For MBQL queries, if `:limit` is set, it will
;; override these values.
(s/def :query/constraints
  (s/keys :opt-un [:query.constraints/max-results :query.constraints/max-results-bare-rows]))

;; should we skip adding results_metadata to query results after running the query? Used by
;; `metabase.query-processor.middleware.results-metadata`; default `false`
(s/def :query.middleware-options/skip-results-metadata? boolean?)

;; should we skip converting datetime types to ISO-8601 strings with appropriate timezone when post-processing
;; results? Used by `metabase.query-processor.middleware.format-rows`; default `false`
(s/def :query.middleware-options/format-rows? boolean?)

(s/def :query/middleware-options
  (s/keys :opt-un [:query.middleware-options/skip-results-metadata? :query.middleware-options/format-rows?]))


;;; ------------------------------------------------------ Info ------------------------------------------------------

;; This stuff is used for informational purposes, primarily to record QueryExecution entries when a query is ran. Pass
;; them along if applicable when writing code that creates queries, but when working on middleware and the like you
;; can most likely ignore this stuff entirely.

;; Spec for `info.context`; used for informational purposes to record how a query was executed.
(s/def :query.info/context
  (s/or
   :nil     nil?
   :context #{:ad-hoc
              :csv-download
              :dashboard
              :embedded-dashboard
              :embedded-question
              :json-download
              :map-tiles
              :metabot
              :public-dashboard
              :public-question
              :pulse
              :question
              :xlsx-download}))

(s/def :query.info/executed-by  (s/or :nil nil?, :id ::positive-int))
(s/def :query.info/card-id      (s/or :nil nil?, :id ::positive-int))
(s/def :query.info/dashboard-id (s/or :nil nil?, :id ::positive-int))
(s/def :query.info/pulse-id     (s/or :nil nil?, :id ::positive-int))
(s/def :query.info/nested?      (some-fn nil? boolean?))

;; `:hash` and `:query-type` get added automatically by `process-query-and-save-execution!`, so don't try passing
;; these in yourself. In fact, I would like this a lot better if we could take these keys out of `:info` entirely
;; and have the code that saves QueryExceutions figure out their values when it goes to save them
(s/def :query.info/query-hash (some-fn nil? (partial instance? (Class/forName "[B"))))

;; TODO - this key is pointless since we can just look at `:type`; let's normalize it out and remove it entirely
;; when we get a chance
(s/def :query.info/query-type #{"MBQL" "native"})

;; Spec for query `:info` dictionary, which is used for informational purposes to record information about how a query
;; was executed in QueryExecution and other places. It is considered bad form for middleware to change its behavior
;; based on this information, don't do it!
(s/def :query/info
  (s/keys :opt-un [:query.info/context
                   :query.info/executed-by
                   :query.info/card-id
                   :query.info/dashboard-id
                   :query.info/pulse-id
                   :query.info/nested?
                   :query.info/query-hash
                   :query.info/query-type]))


;;; -------------------------------------------------- Outer Query ---------------------------------------------------

(s/def :query/type #{:query :native})

;; TODO - move database/virtual-id into this namespace so we don't have to use the magic number here
(s/def :query/database (s/or
                        :id         ::positive-int
                        :virtual-id (partial = -1337)))

(s/def :query/paramamters any?) ; TODO

(s/def :mb/base-query
  (s/keys
   :req-un [:query/database
            :query/type]
   :opt-un [:query/parameters
            :query/settings
            :query/constraints
            :query/middleware
            :query/info]))

(defmulti ^:private query-type :type)

(defmethod query-type :native [_]
  (s/and
   (s/keys :req-un [:query/native])
   (complement #(contains? % :query))))

(defmethod query-type :query [_]
  (s/and
   (s/keys :req-un [:query/query])
   (complement #(contains? % :native))))

(s/def :metabase/query
  (s/and
   :mb/base-query
   (s/multi-spec query-type :type)))


(s/check-asserts true)

(defn- validate-query
  "Compiled schema validator for an [outer] Metabase query. (Pre-compling a validator is more efficient; use this
  instead of calling `(s/validate Query query)` or similar."
  [query]
  (s/assert :metabase/query query))
