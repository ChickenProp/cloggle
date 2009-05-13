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

(let [#^Class gl GL]
  (def gl-methods (seq (.getDeclaredMethods gl)))
  (def gl-fields
       (map (fn [#^java.lang.reflect.Field m]
	      (hash-map :name  (.getName m)
			:type  (.getType m)
			:value (.get m gl)))
	    (seq (.getDeclaredFields gl)))))

(defn def-ev
  "Like def, but evaluates its first argument. And (currently) doesn't add
metadata."
  ([#^Symbol name]
     (intern *ns* name))
  ([#^Symbol name val]
     (intern *ns* name val)))

;; getParameterTypes returns primitive types (int, float, etc.) and array types
;; ([I, [F, etc.) when possible.
;; The array types are easy to get, but I don't know of any way to get the
;; primitive types without reflecting on a method which takes or returns them.
(let [[iat fat dat] (map #(class (% 1 0)) [int-array float-array double-array])
      [ipt fpt dpt] (map (fn [mname]
			   (let [meth (first (filter #(= (.getName %) mname)
						     (.getDeclaredMethods GL)))]
			     (aget (.getParameterTypes meth) 0)))
			 ["glVertex2i" "glVertex2f" "glVertex2d"])
      tmap {ipt :int  fpt :float  dpt :double
	    iat :ints fat :floats dat :doubles
	    Integer :int Float :float Double :double}]
  (defn ptypes->ktypes [ptypes]
    (vec (map #(or (tmap %) %) (seq ptypes))))
  (defn vals->ktypes [& vals]
    (vec (map #(or (tmap (class %)) (class %)) vals))))

(defn defn-from-method
  "Takes an instance method of GL and makes a function on opengl-context of it."
  [#^java.lang.reflect.Method meth]
    (let [name (.getName meth)
	  multi
	    (var-get (or (ns-resolve *ns* (symbol name))
			 (def-ev (symbol name)
			   (new clojure.lang.MultiFn name vals->ktypes
				:default #'clojure.core/global-hierarchy))))
	  params (.getParameterTypes meth)]
      (defmethod multi (ptypes->ktypes params) [& args]
	(.invoke meth opengl-context (to-array args)))))

(doseq [i gl-fields]
  (def-ev (symbol (i :name)) (i :value)))
(doseq [i gl-methods]
  (defn-from-method i))

;; This is the "later (comment)" referred to above.
(comment
  (println "cloggle took"
	   (double (/ (- (. java.lang.System nanoTime) t1) 1000000))
	   "msecs to load."))
