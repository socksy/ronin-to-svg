(ns ronin.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn rect [x y w h] 
  [:rect {:x x :y y :width w :height h}])

(defn circle [cx cy r] 
  [:circle {:cx cx :cy cy :r r}])

(defn poly [& points]
  [:polygon {:points (->> points
                         (partition 2)
                         (map #(str/join "," %))
                         (str/join " "))}])

(defn text 
  ([x y content] (text y y content "start" nil))
  ([x y content anchor font] 
   [:text {:x x :y y :text-anchor anchor 
           :font-family font} content]))

(defn fill [shape color]
  (update shape 1 assoc :fill color))

(defn stroke 
  ([shape color] (stroke shape color 1))
  ([shape color width]
   (-> shape
       (update 1 assoc :stroke color)
       (update 1 assoc :stroke-width width))))

(defn make-svg [w h & elements]
  (into [:svg {:width w :height h 
               :xmlns "http://www.w3.org/2000/svg"}] 
        elements))

;; SVG string generation
(defn- attrs->str [attrs]
  (->> attrs
       (remove (comp nil? second))
       (map (fn [[k v]]
              (str (name k) "=\""
                   (if (number? v) v v)
                   "\"")))
       (str/join " ")))

(defn- element->str [[tag attrs & content]]
  (let [attrs-str (attrs->str attrs)]
    (if (seq content)
      (format "<%s %s>%s</%s>" 
              (name tag) 
              attrs-str 
              (str/join content)
              (name tag))
      (format "<%s %s/>" 
              (name tag) 
              attrs-str))))

(defn generate-svg [hiccup-svg]
  (element->str hiccup-svg))

(defn evaluate-ronin [forms]
  (loop [remaining-forms forms
         current-svg [:svg {}]]
    (if (empty? remaining-forms)
      current-svg
      (let [form (first remaining-forms)]
        (case (first form)
          clear (recur (rest remaining-forms)
                      [:svg {}])
          resize (let [[_ w h] form]
                  (recur (rest remaining-forms)
                         (-> current-svg 
                             (assoc-in  [1 :width] w)
                             (assoc-in  [1 :height] h)
                             (assoc-in  [1 :xmlns] "http://www.w3.org/2000/svg"))))
          (recur (rest remaining-forms)
                 (conj current-svg (eval form))))))))

(defn read-ronin-file [filename]
  (with-open [rdr (io/reader filename)]
    (->> (slurp rdr)
         (format "(do %s)")
         edn/read-string
         rest)))

(defn process-ronin-file [filename]
  (try 
    (-> filename
       read-ronin-file
       evaluate-ronin
       generate-svg)
    (catch Exception e
      (println "Error reading file:" (.getMessage e)))))

(defn -main [& args]
  (process-ronin-file (first args)))

(comment
  ;; Direct usage
  (generate-svg
    (make-svg 600 600
      (fill (rect 0 0 300 600) "#495057")
      (fill (circle 300 300 150) "black")
      (stroke (rect 0 0 600 600) "black" 100)))
  
  ;; Process from file
  (process-ronin-file "logo.ronin"))
