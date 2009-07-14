(ns net.philh.cloggle
  (:import [javax.media.opengl GL]
	   [java.lang.reflect Field Method]
	   [java.awt.image BufferedImage]
	   [javax.imageio ImageIO]
	   [java.io File]))

(def *cloggle-time-load* (ref false))
;; Uncomment this to time how long cloggle takes to initialise.
;; (dosync (ref-set *cloggle-time-load* (. java.lang.System nanoTime)))

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

  (defn def-ev
    "Like def, but evaluates its first argument. And (currently) doesn't add
metadata."
    ([#^Symbol name]
       (intern *ns* name))
    ([#^Symbol name val]
       (intern *ns* name val)))

(let [#^Class gl GL] ; this is the only way I know to avoid reflection on it.
  (def gl-methods (seq (.getDeclaredMethods gl)))
  (def gl-fields
       (map (fn [#^Field m]
	      (hash-map :name  (.getName m)
			:type  (.getType m)
			:value (.get m gl)))
	    (seq (.getDeclaredFields gl)))))

;; getParameterTypes returns primitive types (int, float, etc.) and array types
;; ([I, [F, etc.) when possible. We also want to accept object types (Integer,
;; Float) and clojure collections. So use keyword types to represent them.
      ;;types of arrays of primitives
(let [[iat fat dat] (map #(class (% 1 0)) [int-array float-array double-array])

      ;;types of primitives (we can't map because (. % TYPE) doesn't work.)
      ipt Integer/TYPE
      fpt Float/TYPE
      dpt Double/TYPE

      ;;map them to keyword types
      tmap {ipt     ::int,  fpt   ::float,  dpt    ::double,
	    Integer ::int,  Float ::float,  Double ::double,
	    iat     ::ints, fat   ::floats, dat    ::doubles}

      ;;map keyword types to their weaker variants
      wmap {::int  ::num,  ::float  ::num,  ::double  ::num,
	    ::ints ::nums, ::floats ::nums, ::doubles ::nums}

      ;;map keyword types to functions coercing to them
      fmap {::int int, ::float float, ::double double,
	    ::ints int-array, ::floats float-array, ::doubles double-array}]

  (defn ptypes->ktypes
    "Takes a seq of primitive types, returns a vector of their keyword types."
    [ptypes]
    (vec (map #(or (tmap %) %) ptypes)))
  (defn weaken-ktypes
    "Takes a seq of keyword types, returns a vector of their weak forms."
    [ktypes]
    (vec (map #(or (wmap %) %) ktypes)))

  (defn vals->ktypes
    "Takes multiple values, and returns a vector of their keyword types."
    [& vals]
    (vec (map #(or (tmap (class %)) (class %)) vals)))

  (defn ktype-coerce
    "Coerces a value to a keyword type."
    [ktype val]
    ((get fmap ktype identity) val)))

(derive ::int ::float)
(derive ::float ::double)
(derive ::double ::num)

;; These can't be coerced upwards natively, so we have to send them all directly
;; to ::nums.
(derive ::ints ::nums)
(derive ::floats ::nums)
(derive ::doubles ::nums)
(derive clojure.lang.Seqable ::nums)

(defn defn-from-method
  "Takes an instance method of GL and makes two multifunctions on opengl-context
of it.

One dispatches on exactly the argument types of the original, so [int int]
becomes [::int ::int], and is called with Integer arguments. They are passed
directly to the method.

The other dispatches on weaker forms of the argument types, so [int int] becomes
 [::num ::num], and can be called with numeric arguments of any sort. They will
be coerced to ints before the method is invoked on them."
  [#^Method meth]
  (let [name (.getName meth)

	#^clojure.lang.MultiFn multi
	(var-get (or (ns-resolve *ns* (symbol name))
		     (def-ev (symbol name)
		       (new clojure.lang.MultiFn name vals->ktypes
			    :default #'clojure.core/global-hierarchy))))

	params (.getParameterTypes meth)
	ktypes (ptypes->ktypes params)
	ktypes-weak (weaken-ktypes ktypes)]
    (defmethod multi ktypes [& args]
      (.invoke meth opengl-context (to-array args)))
    (if (not (= ktypes ktypes-weak))
      (defmethod multi ktypes-weak [& args]
	(.invoke meth opengl-context
		 (to-array (map ktype-coerce ktypes args)))))))

(doseq [i gl-fields]
  (def-ev (symbol (i :name)) (i :value)))
(doseq [i gl-methods]
  (defn-from-method i))

;; I assume all BufferedImages are byte-based, which I'm sure isn't true.
;; But I'm not sure what's the best way to handle images that might be based on
;; some other type, so stick with this until it breaks.
(defn bi-get-pixels
  "Returns a byte array of the pixel data in a BufferedImage."
  [#^BufferedImage bi]
  (let [buffer (.. bi (getRaster) (getDataBuffer))]
    (.getData #^java.awt.image.DataBufferByte buffer)))

(defn texture-from-file
  "Given the file name of an image, returns an opengl texture representing it.

The texture will appear to be upside-down due to opengl and image formats having
different ideas about the location of (0,0). Simply place texture coordinates
upside-down as well."
  [#^String file] 
  (let [texa (int-array 1)
	tex (do (glGenTextures 1 texa 0)
		(nth (seq texa) 0))
	im (. ImageIO read (File. file))
	data (bi-get-pixels im)]

    (glBindTexture GL_TEXTURE_2D tex)
    (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
    (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
    (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
    (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (.getWidth im) (.getHeight im) 0
		  GL_RGBA GL_UNSIGNED_BYTE (. java.nio.ByteBuffer wrap data))

    tex))

(if @*cloggle-time-load*
  (println "cloggle took"
	   (double (/ (- (. java.lang.System nanoTime) @*cloggle-time-load*)
		      1000000))
	   "msecs to load."))

