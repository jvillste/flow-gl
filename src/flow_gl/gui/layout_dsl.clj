(ns flow-gl.gui.layout-dsl
  (:require (flow-gl.gui [layouts :as layouts])
            ))

(defmacro def-dsl [helper-name parameters & body]
  (let [implementation-symbol (symbol (str (name helper-name) "-implementation"))]
    `(do (defn ~implementation-symbol ~parameters ~@body)
         (defmacro ~helper-name [& args#]
           `(when-let [~'value# (~~implementation-symbol ~@args#)]
              (with-meta ~'value#
                ~(assoc (meta ~'&form)
                        :file *file*))))
         (alter-meta! (var ~helper-name) assoc :arglists (:arglists (meta (var ~implementation-symbol)))))))

(defn flatten-contents [values]
  (->> values
       (filter (complement nil?))
       (flatten)
       (filter (complement nil?))))

(def-dsl superimpose [& contents]
  (layouts/->Superimpose (flatten-contents contents)))

(def-dsl vertically [& contents]
  (layouts/->VerticalStack (flatten-contents contents)))

(def-dsl preferred [content]
  (layouts/->Preferred [content]))

(def-dsl absolute [& contents]
  (let [contents (flatten-contents contents)]
    (when (seq contents)
      (layouts/->Absolute contents))))

(def-dsl horizontally [& contents]
  (layouts/->HorizontalStack (flatten-contents contents)))

(def-dsl margin [top right bottom left content]
  (when content
    (layouts/->Margin top right bottom left [content])))

(def-dsl center [content]
  (when content
    (layouts/->Center [content])))

(def-dsl box [margin outer inner]
  (when (and outer inner)
    (layouts/->Box margin [outer inner])))

(def-dsl minimum-size [width height content]
  (when content
    (layouts/->MinimumSize width height [content])))

(def-dsl float-left [left right]
  (layouts/->FloatLeft [left right]))

(def-dsl float-top [top bottom]
  (layouts/->FloatTop [top bottom]))

(def-dsl table [margin-size rows]
  (layouts/->Table (->> rows
                        (apply concat)
                        (map #(margin margin-size
                                      margin-size
                                      margin-size
                                      margin-size
                                      %))) 
                   (count (first rows))))

(def-dsl static-table [margin-size rows]
  
  (layouts/->StaticTable (->> rows
                              (apply concat)
                              (map #(margin margin-size
                                            margin-size
                                            margin-size
                                            margin-size
                                            %))) 
                         (count (first rows))))


(def-dsl fixed-size [width height content]
  (when content
    (layouts/->FixedSize width
                         height
                         [content])))
