;; Based on Dustin Withers' port of glxgears to clojure.
(import '(java.awt Frame)
        '(java.awt.event WindowListener WindowAdapter)
        '(javax.media.opengl GLCanvas GLEventListener GL)
        '(com.sun.opengl.util Animator))
(use 'net.philh.cloggle)

(set! *warn-on-reflection* true)

(defn nanotime []
  (double (/ (. java.lang.System nanoTime) 1000000000)))

(def gear1 (ref nil))
(def gear2 (ref nil))
(def gear3 (ref nil))
(def angle (ref 0.0))
(def frames (ref 0))
(def lasttime (ref (nanotime)))
(def longtime (ref @lasttime))

(defn gear 
  [inner-radius outer-radius width teeth tooth-depth]
  (let [r0 inner-radius
        r1 (- outer-radius (/ tooth-depth 2.0))
        r2 (+ outer-radius (/ tooth-depth 2.0))
        da (* 2.0 (/ (. Math PI) teeth 4.0))
        +width (* width 0.5)
        -width (* width -0.5)]
    (shade-model gl-flat)
    (normal3d 0.0 0.0 1.0)

    ;; draw front face
    (with-primitive gl-quad-strip
      (doseq [i (range (+ 1 teeth))]
        (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
          (vertex (* r0 (. Math cos angle))
                  (* r0 (. Math sin angle))
                  +width)
          (vertex (* r1 (. Math cos angle))
                  (* r1 (. Math sin angle))
                  +width)
          (when (< i (+ 1 teeth))
            (vertex (* r0 (. Math cos angle))
                    (* r0 (. Math sin angle))
                    +width)
            (vertex (* r1 (. Math cos (+ angle (* 3.0 da))))
                    (* r1 (. Math sin (+ angle (* 3.0 da))))
                    +width)))))
    
    ;; draw front sides of teeth
    (with-primitive gl-quads
      (doseq [i (range (+ 1 teeth))]
        (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
          (vertex (* r1 (. Math cos angle))
                  (* r1 (. Math sin angle))
                  +width)
          (vertex (* r2 (. Math cos (+ da angle)))
                  (* r2 (. Math sin (+ angle da)))
                  +width)
          (vertex (* r2 (. Math cos (+ angle (* 2.0 da))))
                  (* r2 (. Math sin (+ angle (* 2.0 da))))
                  +width)
          (vertex (* r1 (. Math cos (+ angle (* 3.0 da))))
                  (* r1 (. Math sin (+ angle (* 3.0 da))))
                  +width))))    

    ;; draw back face
    (with-primitive gl-quad-strip
      (doseq [i (range (+ 1 teeth))]
        (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
          (vertex (* r1 (. Math cos angle))
                  (* r1 (. Math sin angle))
                  -width)
          (vertex (* r0 (. Math cos angle))
                  (* r0 (. Math sin angle))
                  -width)
          (vertex (* r1 (. Math cos (+ angle (* 3.0 da))))
                  (* r1 (. Math sin (+ angle (* 3.0 da))))
                  -width)
          (vertex (* r0 (. Math cos angle))
                  (* r0 (. Math sin angle))
                  -width))))

    ;; draw back sides of teeth
    (with-primitive gl-quads
      (doseq [i (range (+ 1 teeth))]
        (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
          (vertex (* r1 (. Math cos (+ angle (* 3.0 da))))
                  (* r1 (. Math sin (+ angle (* 3.0 da))))
                  -width)
          (vertex (* r2 (. Math cos (+ angle (* 2.0 da))))
                  (* r2 (. Math sin (+ angle (* 2.0 da))))
                  -width)
          (vertex (* r2 (. Math cos (+ angle da)))
                  (* r2 (. Math sin (+ angle da)))
                  -width)
          (vertex (* r1 (. Math cos angle))
                  (* r1 (. Math sin angle))
                  -width))))

    ;; draw outward faces of teeth
    (with-primitive gl-quad-strip
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
          (vertex (* r1 (. Math cos angle))
                  (* r1 (. Math sin angle))
                  +width)
          (vertex (* r1 (. Math cos angle))
                  (* r1 (. Math sin angle))
                  -width)
          (normal3d v1 (- u1) 0.0)
          (vertex (* r2 (. Math cos (+ angle da)))
                  (* r2 (. Math sin (+ angle da)))
                  +width)
          (vertex (* r2 (. Math cos (+ angle da)))
                  (* r2 (. Math sin (+ angle da)))
                  -width)
          (normal3d (. Math cos angle)
                      (. Math sin angle)
                      0.0)            
          (vertex (* r2 (. Math cos (+ angle (* 2 da))))
                  (* r2 (. Math sin (+ angle (* 2 da))))
                  +width)
          (vertex (* r2 (. Math cos (+ angle (* 2 da))))
                  (* r2 (. Math sin (+ angle (* 2 da))))
                  -width)
          (normal3d v2 (- u2) 0.0)
          (vertex (* r1 (. Math cos (+ angle (* 3 da))))
                  (* r1 (. Math sin (+ angle (* 3 da))))
                  +width)
          (vertex (* r1 (. Math cos (+ angle (* 3 da))))
                  (* r1 (. Math sin (+ angle (* 3 da))))
                  -width)
          (normal3d (. Math cos angle) (. Math sin angle) 0.0)))
      (vertex (* r1 (. Math cos 0)) (* r1 (. Math sin 0)) +width)
      (vertex (* r1 (. Math cos 0)) (* r1 (. Math sin 0)) -width))
    
    (shade-model gl-smooth)
    
    ;; draw inside radius cylinder
    (with-primitive gl-quad-strip
      (doseq [i (range (+ 1 teeth))]
        (let [angle (/ (* i 2.0 (. Math PI)) teeth)]
          (normal3d (- (. Math cos angle)) (- (. Math sin angle)) 0.0)
          (vertex (* r0 (. Math cos angle))
                  (* r0 (. Math sin angle))
                  -width)
          (vertex (* r0 (. Math cos angle))
                  (* r0 (. Math sin angle)) +width))))))
  
(defn go []
  (let [frame (new Frame)
        canvas (new GLCanvas)
        animator (new Animator canvas)
        view-rotx 20.0
        view-roty 30.0
        view-rotz 0.0]
    (. canvas 
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
                        (ref-set angle (+ (* 50 deltime) @angle))
                        (ref-set lasttime curtime)))
                     (with-gl (. drawable getGL)
                       (clear gl-depth-buffer-bit)
                       (clear gl-color-buffer-bit)
                       (with-pushed-matrix
                         (rotated view-rotx 1.0 0.0 0.0)
                         (rotated view-roty 0.0 1.0 0.0)
                         (rotated view-rotz 0.0 0.0 1.0)

                         (with-pushed-matrix
                           (translated -3.0 -2.0 0.0)
                           (rotated @angle 0.0 0.0 1.0)
                           (call-list @gear1))

                         (with-pushed-matrix
                           (translated 3.1 -2.0 0.0)
                           (rotated (- (* -2.0 @angle) 9.0) 0.0 0.0 1.0)
                           (call-list @gear2))

                         (with-pushed-matrix
                           (translated -3.1 4.2 0.0)
                           (rotated (- (* -2.0 @angle) 25.0) 0.0 0.0 1.0)
                           (call-list @gear3)))))
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
                    (with-gl gl
                      (set-swap-interval 1)
                      (lightfv gl-light0 gl-position pos 0)
                      (enable gl-cull-face)
                      (enable gl-lighting)
                      (enable gl-light0)
                      (enable gl-depth-test)
                    
                      (dosync (ref-set gear1 (gen-lists 1)))
                      (new-list @gear1 gl-compile)
                      (materialfv gl-front gl-ambient-and-diffuse red 0)
                      (gear 1.0 4.0 1.0 20 0.7)
                      (end-list)
                    
                      (dosync (ref-set gear2 (gen-lists 1)))
                      (new-list @gear2 gl-compile)
                      (materialfv gl-front gl-ambient-and-diffuse green 0)
                      (gear 0.5 2.0 2.0 10 0.7)
                      (end-list)
                    
                      (dosync (ref-set gear3 (gen-lists 1)))
                      (new-list @gear3 gl-compile)
                      (materialfv gl-front gl-ambient-and-diffuse blue 0)
                      (gear 1.3 2.0 0.5 10 0.7)
                      (end-list)
                    
                      (enable gl-normalize))))
            
            (reshape [#^javax.media.opengl.GLAutoDrawable drawable
                      x y width height]
                     (let [h (double (/ height width))]
                       (with-gl (.getGL drawable)
                         (matrix-mode gl-projection)
                         (load-identity)
                         (frustum -1.0 1.0 (- h) h 5.0 60.0)
                         (matrix-mode gl-modelview)
                         (load-identity)
                         (translated 0.0 0.0 -40.0)
                         (.. System out
                             (println (str "GL_VENDOR: "
                                           (get-string gl-vendor))))
                         (.. System out
                             (println (str "GL_RENDERER: "
                                           (get-string gl-renderer))))
                         (.. System out
                             (println (str "GL_VERSION: "
                                           (get-string gl-version))))))))))
      (. frame add canvas)
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
