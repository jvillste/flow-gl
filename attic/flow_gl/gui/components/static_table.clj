(ns flow-gl.gui.components.static-table
  (:require [flow-gl.gui.layouts :as layouts]))

(defn static-table-view [view-context state]
  (layouts/->StaticTable (:rows state)
                         (:column-widths state)
                         (:row-height state)))

(defn static-table [view-context initial-rows]
  (let [[column-widths row-heights] (layouts/table-sizes  (take 1 initial-rows))]
    {:local-state {:column-widths column-widths
                   :row-height (first row-heights)}
     :view #'static-table-view}))
