(ns net.philh.cloggle
  (:import [javax.media.opengl GL]
           [java.lang.reflect Field Method]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File])
  (:refer-clojure :exclude [flush]))

(def *cloggle-time-load* (ref false))
;; Uncomment this to time how long cloggle takes to initialise.
;; (dosync (ref-set *cloggle-time-load* (. java.lang.System nanoTime)))

(def #^GL *opengl-context* nil)

(defmacro with-gl
  "Evaluates forms in the context of the GL object."
  [#^GL gl-obj & body]
  `(io! (binding [*opengl-context* ~gl-obj]
          ~@body)))

(defmacro with-primitive
  "Evaluates body within (begin mode) and (end) expressions."
  [mode & body]
  `(try (begin ~mode)
        ~@body
        (finally (end))))

(defmacro with-pushed-matrix
  "Evaluates body within (push-matrix) and (pop-matrix) expressions."
  [& body]
  `(try (push-matrix)
        ~@body
        (finally (pop-matrix))))

(defn- def-ev
  "Like def, but evaluates its first argument. And (currently) doesn't add
metadata."
  ([#^Symbol name]
     (intern *ns* name))
  ([#^Symbol name val]
     (intern *ns* name val)))

(let [#^Class gl GL] ; this is the only way I know to avoid reflection on it.
  (def #^{:private true} gl-methods (seq (.getDeclaredMethods gl)))
  (def #^{:private true} gl-fields
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

      ;;map normal types to keyword types
      tmap {ipt     ::int,  fpt   ::float,  dpt    ::double,
            Integer ::int,  Float ::float,  Double ::double,
            iat     ::ints, fat   ::floats, dat    ::doubles,
            clojure.lang.Ratio ::num}

      ;;map keyword types to their weaker variants
      wmap {::int  ::num,  ::float  ::num,  ::double  ::num,
            ::ints ::nums, ::floats ::nums, ::doubles ::nums}

      ;;map keyword types to functions coercing to them
      fmap {::int int, ::float float, ::double double,
            ::ints int-array, ::floats float-array, ::doubles double-array}]

  (defn- ptypes->ktypes
    "Takes a seq of primitive types, returns a vector of their keyword types."
    [ptypes]
    (vec (map #(or (tmap %) %) ptypes)))
  (defn- weaken-ktypes
    "Takes a seq of keyword types, returns a vector of their weak forms."
    [ktypes]
    (vec (map #(or (wmap %) %) ktypes)))

  (defn- vals->ktypes
    "Takes multiple values, and returns a vector of their keyword types."
    [& vals]
    (vec (map #(or (tmap (class %)) (class %)) vals)))

  (defn- ktype-coerce
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

(defn- split
  "Split on underscores, then split on the beginning of camel case words."
  [#^String s]
  (mapcat #(.split #^String % "(?!^)((?=[A-Z][a-z])|(?<![A-Z0-9])(?=[A-Z]))")
          (.split s "_+")))

(defn- camel->lower-case
  "Converts a C-style GL_CONST_NAME or glFuncName to a Lisp-style gl-const-name or gl-func-name."
  ([s] (apply str (interpose \- (map #(.toLowerCase #^String %) (split s)))))
  ([s prefix]
     (apply str (interpose \- (remove #(= % prefix)
                                      (map #(.toLowerCase #^String %)
                                           (split s)))))))

(defn- get-or-def-multi [name]
  (var-get (or (ns-resolve *ns* (symbol name))
               (def-ev (symbol name)
                 (new clojure.lang.MultiFn name vals->ktypes
                      :default #'clojure.core/global-hierarchy)))))

(defn- def-gl-method-strong
  [#^clojure.lang.MultiFn multi #^Method method]
  (let [params (.getParameterTypes method)
        ktypes (ptypes->ktypes params)]
    (defmethod multi ktypes [& args]
      (.invoke method *opengl-context* (to-array args)))))
(defn- def-gl-method-weak
  [#^clojure.lang.MultiFn multi #^Method method]
  (let [params (.getParameterTypes method)
        ktypes (ptypes->ktypes params)
        ktypes-weak (weaken-ktypes ktypes)]
    (defmethod multi ktypes-weak [& args]
      (.invoke method *opengl-context*
               (to-array (map ktype-coerce ktypes args))))))
(defn def-gl-method-both
  [multi #^Method method]
  (let [params (.getParameterTypes method)
        ktypes (ptypes->ktypes params)
        ktypes-weak (weaken-ktypes ktypes)]
    (def-gl-method-strong multi method)
    (if (not= ktypes ktypes-weak)
      (def-gl-method-weak multi method))))

(defn- defn-from-method
  "Takes an instance method of GL and makes two multifunctions on opengl-context
of it.

One dispatches on exactly the argument types of the original, so [int int]
becomes [::int ::int], and is called with Integer arguments. They are passed
directly to the method.

The other dispatches on weaker forms of the argument types, so [int int] becomes
 [::num ::num], and can be called with numeric arguments of any sort. They will
be coerced to ints before the method is invoked on them."
  [#^Method meth]
  (let [name (camel->lower-case (.getName meth) "gl")
        #^clojure.lang.MultiFn multi (get-or-def-multi name)]
    (def-gl-method-both multi meth)))

(doseq [i gl-fields]
  (def-ev (symbol (camel->lower-case (i :name))) (i :value)))
(doseq [i gl-methods]
  (defn-from-method i))

(defn- stem
  "Returns a shorter version of a gl method name. For example, both vertex2i
and vertex3f become vertex."
  [#^Method method]
  (let [match (re-matches #"([a-z\\-]+)[0-9]?(?:i|f|d)"
                          (camel->lower-case (.getName method) "gl"))]
    (if match (second match))))

(defn- partition-methods
  "Partitions methods based on the name stemming scheme."
  [methods]
  (loop [methods methods partition nil]
    (if methods
      (let [method (first methods) match (stem method)]
        (if match
          (if (and partition (= match (stem (ffirst partition))))
            (recur (next methods) (cons (cons method (first partition))
                                        (rest partition)))
            (recur (next methods) (cons (list method) partition)))
          (recur (next methods) partition)))
      partition)))

;; .getDeclaredMethods seems to return them in the order i,f,d, which means that
;; if a weak method overwrites another weak method, the new one will be more
;; general.  If .getDeclaredMethods does not always return them in this order,
;; this will not work properly. Best approach is probably to explicitly sort
;; them by last letter, d > f > i.
(defn- defn-convenience-method
  "Wraps multiple versions of the 'same' method into a single mutlifn. For
example, vertex2i, vertex2f, vertex2d, (and 3i, 3f, 3d, etc) form a single
vertex function."
  [methods]
  (let [name (stem (first methods))
        #^clojure.lang.MultiFn multi (get-or-def-multi name)]
    (doseq [meth methods]
      (def-gl-method-both multi meth))

    ;; define a catch-all for seqs
    (defmethod multi [::nums] [s]
      (apply multi s))))

(doseq [methods (partition-methods gl-methods)]
  (if (and (> (count methods) 1)
           ;; don't define map since it will conflict with clojure.core
           (not (= (stem (first methods)) "map")))
    (defn-convenience-method methods)))

;; flush conflicts with the core library: provide gl-flush instead.
;; Without the anonymous fn, compiling this library breaks it:
;; http://groups.google.com/group/clojure/browse_thread/thread/74d7f7216fc9355b
((fn []
   (def gl-flush flush)
   (def #^{:private true} flush)))

;; This assumes all BufferedImages are byte-based, which I'm sure isn't true.
;; But I'm not sure what's the best way to handle images that might be based on
;; some other type, so stick with this until it breaks.
(defn- bi-get-pixels
  "Returns a byte array of the pixel data in a BufferedImage."
  [#^BufferedImage bi]
  (let [buffer (.. bi (getRaster) (getDataBuffer))]
    (.getData #^java.awt.image.DataBufferByte buffer)))

(defn texture-from-file
  "Given the file name of an image, returns an opengl texture representing it.

The texture will appear to be upside-down due to opengl and image formats having
different ideas about the location of (0,0). Simply place texture coordinates
upside-down as well.

This works for RGB and RGBA images. Other formats will probably break it."
  [#^String file]
  (let [texa (int-array 1)
        tex (do (gen-textures 1 texa 0)
                (nth (seq texa) 0))
        im (. ImageIO read (File. file))
        data (bi-get-pixels im)
        buf (java.nio.ByteBuffer/wrap data)
        format (if (.. im getColorModel hasAlpha) gl-rgba gl-rgb)]

    (bind-texture gl-texture-2d tex)
    (tex-parameterf gl-texture-2d gl-texture-min-filter gl-linear)
    (tex-parameterf gl-texture-2d gl-texture-mag-filter gl-linear)
    (tex-parameterf gl-texture-2d gl-texture-wrap-s gl-clamp)
    (tex-parameterf gl-texture-2d gl-texture-wrap-t gl-clamp)
    (tex-image2d gl-texture-2d 0 format (.getWidth im) (.getHeight im) 0
                 format gl-unsigned-byte buf)

    tex))

(if @*cloggle-time-load*
  (println "cloggle took"
           (double (/ (- (. java.lang.System nanoTime) @*cloggle-time-load*)
                      1000000))
           "msecs to load."))

