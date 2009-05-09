(ns net.philh.cloggle
  (:import [javax.media.opengl GL]))

;; Uncomment this, and the later (comment), to time how long cloggle takes to
;; initialise.
(comment
  (println "cloggle loading")
  (def t1 (. java.lang.System nanoTime)))

(def #^GL opengl-context nil)

(defmacro ctx
  "Evaluates forms in the context of the GL object."
  [#^GL gl-obj & forms]
  `(binding [opengl-context ~gl-obj]
     ~@forms))

(defmacro beg-end
  "Evaluates forms within (begin mode) and (end) expressions."
  [mode & forms]
  `(do (glBegin ~mode)
       ~@forms
       (glEnd)))

(def gl-methods (seq (.getDeclaredMethods GL)))
(def gl-fields
     (map #(hash-map :name (.getName %)
		     :type (.getType %)
		     :value  (.get % GL))
	  (seq (.getDeclaredFields GL))))

(defn def-ev
  "Like def, but evaluates its first argument. And (currently) doesn't add
metadata."
  ([#^Symbol name]
     (intern *ns* name))
  ([#^Symbol name val]
     (intern *ns* name val)))

(defn defn-from-method
  "Takes an instance method of GL and makes a function on opengl-context of it."
  [meth]
    (def-ev (symbol (.getName meth))
      (fn [& args]
	(.invoke meth opengl-context (to-array args)))))

(doall (map #(def-ev (symbol (% :name)) (% :value)) gl-fields))
(doall (map defn-from-method gl-methods))

;; This is the "later (comment)" referred to above.
(comment
  (println "cloggle took"
	   (double (/ (- (. java.lang.System nanoTime) t1) 1000000))
	   "msecs to load."))
