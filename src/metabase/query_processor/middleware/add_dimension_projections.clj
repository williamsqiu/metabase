(ns metabase.query-processor.middleware.add-dimension-projections
  "Middleware for adding remapping and other dimension related projections. This remaps Fields that have a corresponding
  Dimension object (which defines a remapping) in two different ways, depending on the `:type` attribute of the
  Dimension:

  `external` type Dimensions mean the Field's values will be replaced with corresponding values from a column on a
  different table, joined via a foreign key. A common use-case would be to replace FK IDs with the name of whatever it
  references, for example replacing a values of `venue.category_id` with values of `category.name`. Actual replacement
  of values happens on the frontend, so this middleware simply adds the column to be used for replacement (e.g.
  `category.name`) to the `:fields` clause in pre-processing, so the Field will be fetched. Recall that Fields
  referenced via with `:fk->` clauses imply that JOINs will take place, which are automatically handled later in the
  Query Processor pipeline. Additionally, this middleware will swap out and `:order-by` clauses referencing the
  original Field with ones referencing the remapped Field (for example, so we would sort by `category.name` instead of
  `category_id`).

  `internal` type Dimensions mean the Field's values are replaced by a user-defined map of values, stored in the
  `human_readable_values` column of a corresponding `FieldValues` object. A common use-case for this scenario would be
  to replace integer enum values with something more descriptive, for example replacing values of an enum `can_type`
  -- `0` becomes `Toucan`, `1` becomes `Pelican`, and so forth. This is handled exclusively in post-processing by
  adding extra columns and values to the results.

  In both cases, to accomplish values replacement on the frontend, the post-processing part of this middleware adds
  appropriate `:remapped_from` and `:remapped_to` attributes in the result `:cols` in post-processing.
  `:remapped_from` and `:remapped_to` are the names of the columns, e.g. `category_id` is `:remapped_to` `name`, and
  `name` is `:remapped_from` `:category_id`."
  (:require [metabase.mbql
             [util :as mbql.u]]
            [metabase.models.dimension :refer [Dimension]]
            [metabase.util :as u]
            [clojure.spec.alpha :as s]
            [toucan.db :as db]
            [clojure.string :as str]))

;; Spec for the info we fetch about `external` type Dimensions that will be used for remappings in this Query. Fetched
;; by the pre-processing portion of the middleware, and passed along to the post-processing portion.
(s/def ::name                    (s/and string? (complement str/blank?)))
(s/def ::field_id                (s/and int? pos?))
(s/def ::human_readable_field_id (s/and int? pos?))

(s/def ::external-remapping-dimension
  (s/keys :req-un [::name ::field_id ::human_readable_field_id]))


;;; ----------------------------------------- add-fk-remaps (pre-processing) -----------------------------------------

(defn- fields->field-id->remapping-dimension
  "Given a sequence of field clauses (from the `:fields` clause), return a map of `:field-id` clause (other clauses
  are ineligable) to a remapping dimension information for any Fields that have an `external` type dimension remapping."
  [fields]
  {:pre  [(s/assert (s/* :mbql/field) fields)]
   :post [(s/assert (s/or :nil nil?, :map (s/map-of (s/and int? pos?) ::external-remapping-dimension)) %)]}
  (when-let [field-ids (seq (map second (filter (partial mbql.u/is-clause? :field-id) fields)))]
    (u/key-by :field_id (db/select [Dimension :field_id :name :human_readable_field_id]
                          :field_id [:in (set field-ids)]
                          :type     "external"))))

(defn- create-remap-col-tuples
  "Return tuples of `:field-id` clauses, the new remapped column `:fk->` clauses that the Field should be remapped to,
  and the Dimension that suggested the remapping, which is used later in this middleware for post-processing. Order is
  important here, because the results are added to the `:fields` column in order. (TODO - why is it important, if they
  get hidden when displayed anyway?)"
  [fields]
  {:pre  [(s/assert (s/* :mbql/field) fields)]
   :post [(s/assert (s/* (s/tuple :mbql.field/field-id :mbql.field/fk-> ::external-remapping-dimension)) %)]}
  (when-let [field-id->remapping-dimension (fields->field-id->remapping-dimension fields)]
    (vec (for [field fields
               :when (mbql.u/is-clause? :field-id field)
               :let  [dimension (field-id->remapping-dimension (second field))]
               :when dimension]
           [field
            [:fk-> field [:field-id (:human_readable_field_id dimension)]]
            dimension]))))

(defn- update-remapped-order-by
  "Order by clauses that include an external remapped column should be replace that original column in the order by with
  the newly remapped column. This should order by the text of the remapped column vs. the id of the source column
  before the remapping"
  [field->remapped-col order-by-clauses]
  {:pre  [(s/assert (s/map-of :mbql.field/field-id :mbql.field/fk->) field->remapped-col)
          (s/assert (s/* :mbql/order-by) order-by-clauses)]
   :post [(s/assert (s/* :mbql/order-by) %)]}
  (vec
   (for [[direction field, :as order-by-clause] order-by-clauses]
     (if-let [remapped-col (get field->remapped-col field)]
       [direction remapped-col]
       order-by-clause))))

(defn- add-fk-remaps
  "Add any Fields needed for `:external` remappings to the `:fields` clause of the query, and update `:order-by`
  clause as needed. Returns a pair like `[external-remapping-dimensions updated-query]`."
  [{{:keys [fields order-by]} :query, :as query}]
  {:pre  [(s/assert :metabase/query query)]
   :post [(s/assert (s/tuple (s/or :nil nil?, :dimension ::external-remapping-dimension) :metabase.query) %)]}
  ;; fetch remapping column pairs if any exist...
  (if-let [remap-col-tuples (seq (create-remap-col-tuples fields))]
    ;; if they do, update `:fields` and `:order-by` clauses accordingly and add to the query
    (let [new-fields   (vec (concat fields (map second remap-col-tuples)))
          ;; make a map of field-id-clause -> fk-clause from the tuples
          new-order-by (update-remapped-order-by (into {} (for [[field-clause fk-clause] remap-col-tuples]
                                                            [field-clause fk-clause]))
                                                 order-by)]
      ;; return the Dimensions we are using and the query
      [(map last remap-col-tuples)
       (cond-> (assoc-in query [:query :fields] new-fields)
         (seq new-order-by) (assoc-in [:query :order-by] new-order-by))])
    ;; otherwise return query as-is
    [nil query]))


;;; ---------------------------------------- remap-results (post-processing) -----------------------------------------

(defn- add-remapping-info
  "Add `:display_name`, `:remapped_to`, and `:remapped_from` keys to columns for the results, needed by the frontend.
  To get this critical information, this uses the `remapping-dimensions` info saved by the pre-processing portion of
  this middleware for external remappings, and the internal-only remapped columns handled by post-processing
  middleware below for internal columns."
  [remapping-dimensions columns internal-remap-columns]
  {:pre  [(s/assert (s/or :nil nil?, :dimensions (s/* ::external-remapping-dimension)) remapping-dimensions)
          (s/assert (s/* map?) columns)
          (s/assert (some-fn nil? map?) internal-remap-columns)]
   :post [(s/assert map? %)]}
  (let [column-id->column              (u/key-by :id columns)
        name->internal-remapped-to-col (u/key-by :remapped_from internal-remap-columns)
        id->remapped-to-dimension      (u/key-by :field_id                remapping-dimensions)
        id->remapped-from-dimension    (u/key-by :human_readable_field_id remapping-dimensions)]
    (for [{:keys [id], column-name :name, :as column} columns]
      (merge
       column
       ;; if one of the internal remapped columns says it's remapped from this column, add a matching `:remapped_to`
       ;; entry
       (when-let [{remapped-to-name :name} (get name->internal-remapped-to-col column-name)]
         {:remapped_to remapped-to-name})
       ;; if the pre-processing remapping Dimension info contains an entry where this Field's ID is `:field_id`, add
       ;; an entry noting the name of the Field it gets remapped to
       (when-let [{remapped-to-id :human_readable_field_id} (get id->remapped-to-dimension id)]
         {:remapped_to (:name (get column-id->column remapped-to-id))})
       ;; if the pre-processing remapping Dimension info contains an entry where this Field's ID is
       ;; `:human_readable_field_id`, add an entry noting the name of the Field it gets remapped from, and use the
       ;; `:display_name` of the Dimension
       (when-let [{dimension-name :name, remapped-from-id :field_id} (get id->remapped-from-dimension id)]
         {:display_name  dimension-name
          :remapped_from (:name (get column-id->column remapped-from-id))})))))

(defn- create-remapped-col [col-name remapped-from]
  {:description     nil
   :id              nil
   :table_id        nil
   :expression-name col-name
   :source          :fields
   :name            col-name
   :display_name    col-name
   :target          nil
   :extra_info      {}
   :remapped_from   remapped-from
   :remapped_to     nil})

(defn- transform-values-for-col
  "Converts `values` to a type compatible with the base_type found for `col`. These values should be directly comparable
  with the values returned from the database for the given `col`."
  [{:keys [base_type] :as col} values]
  (map (condp #(isa? %2 %1) base_type
         :type/Decimal    bigdec
         :type/Float      double
         :type/BigInteger bigint
         :type/Integer    int
         :type/Text       str
         identity)
       values))

(defn- col->dim-map
  [idx {{remap-to :dimension-name, remap-type :dimension-type, field-id :field-id} :dimensions, :as col}]
  (when field-id
    (let [remap-from (:name col)]
      {:col-index      idx
       :from           remap-from
       :to             remap-to
       :xform-fn       (zipmap (transform-values-for-col col (get-in col [:values :values]))
                               (get-in col [:values :human-readable-values]))
       :new-column     (create-remapped-col remap-to remap-from)
       :dimension-type remap-type})))

(defn- row-map-fn [dim-seq]
  (fn [row]
    (concat row (map (fn [{:keys [col-index xform-fn]}]
                          (xform-fn (nth row col-index)))
                        dim-seq))))

(defn- remap-results
  "Munges results for remapping after the query has been executed. For internal remappings, a new column needs to be
  added and each row flowing through needs to include the remapped data for the new column. For external remappings,
  the column information needs to be updated with what it's being remapped from and the user specified name for the
  remapped column."
  [remapping-dimensions results]
  {:pre [(s/assert (s/or :nil nil?, :dimensions (s/* ::external-remapping-dimension)) remapping-dimensions)]}
  (let [indexed-dims       (keep-indexed col->dim-map (:cols results))
        internal-only-dims (filter #(= :internal (:dimension-type %)) indexed-dims)
        remap-fn           (row-map-fn internal-only-dims)
        internal-only-cols (map :new-column internal-only-dims)]
    (-> results
        (update :columns into (map :to internal-only-dims))
        (assoc  :cols    (map #(dissoc % :dimensions :values)
                              (concat (add-remapping-info remapping-dimensions (:cols results) internal-only-cols)
                                      internal-only-cols)))
        (update :rows    #(map remap-fn %)))))


;;; --------------------------------------------------- middleware ---------------------------------------------------

(defn add-remapping
  "Query processor middleware. `qp` is the query processor, returns a function that works on a `query` map. Delgates to
  `add-fk-remaps` for making remapping changes to the query (before executing the query). Then delegates to
  `remap-results` to munge the results after query execution."
  [qp]
  (fn [query]
    (let [[remapping-dimensions query] (add-fk-remaps query)
          results                      (qp query)]
      (remap-results remapping-dimensions results))))
