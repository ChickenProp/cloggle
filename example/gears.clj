;; Based on 's port of glxgears to clojure.
(import '(java.awt Frame)
	'(java.awt.event WindowListener WindowAdapter)
	'(javax.media.opengl GLCanvas GLEventListener GL)
	'(com.sun.opengl.util Animator))
(use 'net.philh.cloggle)

(defn nanotime []
  (double (/ (. java.lang.System nanoTime) 1000000000)))

(def gear1 (ref nil))
(def gear2 (ref nil))
(def gear3 (ref nil))
(def angle (ref 0.0))
(def frames (ref 0))
(def lasttime (ref (nanotime)))
(def longtime (ref @lasttime))

(defmacro gl-beg-end 
  [gl mode & body]
  (list 'do 
        (list '. 'gl 'glBegin (list '. 'GL mode))
        (cons 'do body)
        (list '. 'gl 'glEnd)))

(defn gear 
  [#^javax.media.opengl.GL gl inner-radius outer-radius width teeth tooth-depth]
  (let [r0 inner-radius
	r1 (- outer-radius (/ tooth-depth 2.0))
	r2 (+ outer-radius (/ tooth-depth 2.0))
	da (* 2.0 (/ (. Math PI) teeth 4.0))
	+width (* width 0.5)
	-width (* width -0.5)]
    (ctx gl
      (glShadeModel GL_FLAT)
      (glNormal3d 0.0 0.0 1.0)

      ;; draw front face
      (beg-end GL_QUAD_STRIP
	 (doseq [i (range (+ 1 teeth))]
	   (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
	     (glVertex3d (* r0 (. Math cos angle))
			 (* r0 (. Math sin angle))
			 +width)
	     (glVertex3d (* r1 (. Math cos angle))
			 (* r1 (. Math sin angle))
			 +width)
	     (when (< i (+ 1 teeth))
	       (glVertex3d (* r0 (. Math cos angle))
			   (* r0 (. Math sin angle))
			   +width)
	       (glVertex3d (* r1 (. Math cos (+ angle (* 3.0 da))))
			   (* r1 (. Math sin (+ angle (* 3.0 da))))
			   +width)))))
    
      ;; draw front sides of teeth
      (beg-end GL_QUADS
	(doseq [i (range (+ 1 teeth))]
	  (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
	    (glVertex3d (* r1 (. Math cos angle))
			(* r1 (. Math sin angle))
			+width)
	    (glVertex3d (* r2 (. Math cos (+ da angle)))
			(* r2 (. Math sin (+ angle da)))
			+width)
	    (glVertex3d (* r2 (. Math cos (+ angle (* 2.0 da))))
			(* r2 (. Math sin (+ angle (* 2.0 da))))
			+width)
	    (glVertex3d (* r1 (. Math cos (+ angle (* 3.0 da))))
			(* r1 (. Math sin (+ angle (* 3.0 da))))
			+width))))    

      ;; draw back face
      (beg-end GL_QUAD_STRIP
	(doseq [i (range (+ 1 teeth))]
	  (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
	    (glVertex3d (* r1 (. Math cos angle))
			 (* r1 (. Math sin angle))
			 -width)
	    (glVertex3d (* r0 (. Math cos angle))
			 (* r0 (. Math sin angle))
			 -width)
	    (glVertex3d (* r1 (. Math cos (+ angle (* 3.0 da))))
			 (* r1 (. Math sin (+ angle (* 3.0 da))))
			 -width)
	    (glVertex3d (* r0 (. Math cos angle))
			 (* r0 (. Math sin angle))
			 -width))))

      ;; draw back sides of teeth
      (beg-end GL_QUADS
	(doseq [i (range (+ 1 teeth))]
	  (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
	    (glVertex3d (* r1 (. Math cos (+ angle (* 3.0 da))))
			(* r1 (. Math sin (+ angle (* 3.0 da))))
			-width)
	    (glVertex3d (* r2 (. Math cos (+ angle (* 2.0 da))))
			(* r2 (. Math sin (+ angle (* 2.0 da))))
			-width)
	    (glVertex3d (* r2 (. Math cos (+ angle da)))
			(* r2 (. Math sin (+ angle da)))
			-width)
	    (glVertex3d (* r1 (. Math cos angle))
			(* r1 (. Math sin angle))
			-width))))

      ;; draw outward faces of teeth
      (beg-end GL_QUAD_STRIP
	(doseq [i (range (+ 1 teeth))]
	  (let [angle (/ (* i 2.0 (. Math PI)) teeth)
		u-div (- (* r2 (. Math cos (+ angle da)))
			 (* r1 (. Math cos angle)))
		v-div (- (* r2 (. Math sin (+ angle da)))
			 (* r1 (. Math sin angle)))
		len (. Math sqrt (+ (* u-div u-div) (* v-div v-div)))
		u1 (/ u-div len)
		v1 (/ v-div len)
		u2 (- (* r1 (. Math cos (+ angle (* 3 da))))
		      (* r2 (. Math cos (+ angle (* 2 da)))))
		v2 (- (* r1 (. Math sin (+ angle (* 3 da))))
		      (* r2 (. Math sin (+ angle (* 2 da)))))]
	    (glVertex3d (* r1 (. Math cos angle))
			(* r1 (. Math sin angle))
			+width)
	    (glVertex3d (* r1 (. Math cos angle))
			(* r1 (. Math sin angle))
			-width)
	    (glNormal3d v1 (- u1) 0)
	    (glVertex3d (* r2 (. Math cos (+ angle da)))
			(* r2 (. Math sin (+ angle da)))
			+width)
	    (glVertex3d (* r2 (. Math cos (+ angle da)))
			(* r2 (. Math sin (+ angle da)))
			-width)
	    (glNormal3d (. Math cos angle)
			(. Math sin angle)
			0.0)	    
	    (glVertex3d (* r2 (. Math cos (+ angle (* 2 da))))
			(* r2 (. Math sin (+ angle (* 2 da))))
			+width)
	    (glVertex3d (* r2 (. Math cos (+ angle (* 2 da))))
			(* r2 (. Math sin (+ angle (* 2 da))))
			-width)
	    (glNormal3d v2 (- u2) 0.0)
	    (glVertex3d (* r1 (. Math cos (+ angle (* 3 da))))
			(* r1 (. Math sin (+ angle (* 3 da))))
			+width)
	    (glVertex3d (* r1 (. Math cos (+ angle (* 3 da))))
			(* r1 (. Math sin (+ angle (* 3 da))))
			-width)
	    (glNormal3d (. Math cos angle) (. Math sin angle) 0.0)))
	(glVertex3d (* r1 (. Math cos 0)) (* r1 (. Math sin 0)) +width)
	(glVertex3d (* r1 (. Math cos 0)) (* r1 (. Math sin 0)) -width))
    
      (glShadeModel GL_SMOOTH)
    
      ;; draw inside radius cylinder
      (beg-end GL_QUAD_STRIP
	(doseq [i (range (+ 1 teeth))]
	  (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
	    (glNormal3d (- (. Math cos angle)) (- (. Math sin angle)) 0.0)
	    (glVertex3d (* r0 (. Math cos angle))
			(* r0 (. Math sin angle))
			-width)
	    (glVertex3d (* r0 (. Math cos angle))
			(* r0 (. Math sin angle)) +width)))))))
  
(defn go []
  (let [frame (new Frame)
	gl-canvas (new GLCanvas)
	animator (new Animator gl-canvas)
	view-rotx 20.0
	view-roty 30.0
	view-rotz 0.0]
    (. gl-canvas 
	 (addGLEventListener
	  (proxy [GLEventListener] []
	    (display [#^javax.media.opengl.GLAutoDrawable drawable]
		     (dosync
		      (ref-set frames (+ 1 @frames))
		      (let [curtime (nanotime)
			    deltime (- curtime @lasttime)
			    longdtime (- curtime @longtime)]
			(if (> longdtime 5)
			  (do (println @frames "frames in"
				       longdtime "seconds ="
				       (/ @frames longdtime) "FPS")
			      (ref-set longtime curtime)
			      (ref-set frames 0)))
			(ref-set angle (+ (* 0.05 deltime) @angle))))
		     (ctx (. drawable getGL)
			  (glClear GL_DEPTH_BUFFER_BIT)
			  (glClear GL_COLOR_BUFFER_BIT)
			  (glPushMatrix)
			  (glRotated view-rotx 1.0 0.0 0.0)
			  (glRotated view-roty 0.0 1.0 0.0)
			  (glRotated view-rotz 0.0 0.0 1.0)

			  (glPushMatrix)
			  (glTranslated -3.0 -2.0 0.0)
			  (glRotated @angle 0.0 0.0 1.0)
			  (glCallList @gear1)
			  (glPopMatrix)

			  (glPushMatrix)
			  (glTranslated 3.1 -2.0 0.0)
			  (glRotated (- (* -2.0 @angle) 9.0) 0.0 0.0 1.0)
			  (glCallList @gear2)
			  (glPopMatrix)

			  (glPushMatrix)
			  (glTranslated -3.1 4.2 0.0)
			  (glRotated (- (* -2.0 @angle) 25.0) 0.0 0.0 1.0)
			  (glCallList @gear3)
			  (glPopMatrix)

			  (glPopMatrix)))
	    (displayChanged [drawable mode-changed device-changed])
	    (init [#^javax.media.opengl.GLAutoDrawable drawable]
		  (let [gl (. drawable getGL)
			pos (float-array 4 '(5.0 5.0 10.0 0.0))
			red (float-array 4 '(0.8 0.1 0.0 1.0))
			green (float-array 4 '(0.0 0.8 0.2 1.0))
			blue (float-array 4 '(0.2 0.2 1.0 1.0))]
		    (.. System out
			(println (str "INIT GL IS: "
				      (.. gl (getClass) (getName)))))
		    (.. System out
			(println (str "Chosen GLCapabilities: "
				      (. drawable getChosenGLCapabilities))))
		    (ctx gl
		      (setSwapInterval 1)
		      (glLightfv GL_LIGHT0 GL_POSITION pos 0)
		      (glEnable GL_CULL_FACE)
		      (glEnable GL_LIGHTING)
		      (glEnable GL_LIGHT0)
		      (glEnable GL_DEPTH_TEST)
		    
		      (dosync (ref-set gear1 (glGenLists 1)))
		      (glNewList @gear1 GL_COMPILE)
		      (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE red 0)
		      (gear gl 1.0 4.0 1.0 20 0.7)
		      (glEndList)
		    
		      (dosync (ref-set gear2 (glGenLists 1)))
		      (glNewList @gear2 GL_COMPILE)
		      (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE green 0)
		      (gear gl 0.5 2.0 2.0 10 0.7)
		      (glEndList)
		    
		      (dosync (ref-set gear3 (glGenLists 1)))
		      (glNewList @gear3 GL_COMPILE)
		      (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE blue 0)
		      (gear gl 1.3 2.0 0.5 10 0.7)
		      (glEndList)
		    
		      (glEnable GL_NORMALIZE))))
	    
	    (reshape [#^javax.media.opengl.GLAutoDrawable drawable
		      x y width height]
		     (let [h (double (/ height width))]
		       (ctx (.getGL drawable)
			 (glMatrixMode GL_PROJECTION)
			 (glLoadIdentity)
			 (glFrustum -1.0 1.0 (- h) h 5.0 60.0)
			 (glMatrixMode GL_MODELVIEW)
			 (glLoadIdentity)
			 (glTranslated 0.0 0.0 -40.0)
			 (.. System out
			     (println (str "GL_VENDOR: "
					   (glGetString GL_VENDOR))))
			 (.. System out
			     (println (str "GL_RENDERER: "
					   (glGetString GL_RENDERER))))
			 (.. System out
			     (println (str "GL_VERSION: "
					   (glGetString GL_VERSION))))))))))
      (. frame add gl-canvas)
      (. frame (setSize 300 300))
      (. frame
	 (addWindowListener
	  (proxy [WindowAdapter] []
	    (windowClosing [event]
			   (. (new Thread
				   (fn []
				     (. animator stop)
				     (. frame dispose))) start)))))
      (. frame show)
      (. animator start)))

(go)
