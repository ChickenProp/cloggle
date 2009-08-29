;; Opens a window and puts a triangle inside it. As simple as it gets.

(use 'net.philh.cloggle)

(let [frame (new java.awt.Frame)
        canvas (new javax.media.opengl.GLCanvas)]
    (.addGLEventListener canvas
       (proxy [javax.media.opengl.GLEventListener] []
         (init [x])
         (reshape [drawable x y w h]
                  (with-context (.getGL drawable)
                    (gl-matrix-mode GL_PROJECTION)
                    (gl-load-identity)
                    (gl-ortho 0 1 0 1 -1 1)
                    (gl-matrix-mode GL_MODELVIEW)
                    (gl-load-identity))) 
         (display [drawable]
                  (with-context (.getGL drawable)
                    (gl-clear GL_COLOR_BUFFER_BIT)
                    (gl-clear GL_DEPTH_BUFFER_BIT)
                    (color 1 0.5 1)
                    (with-primitive GL_TRIANGLES
                      (vertex 0 0)
                      (vertex [1 0.5]) ; make sure vector arguments work.
                      (vertex 0 1))))))
    (.setSize canvas 100 100)
    (.add frame canvas)
    (.addWindowListener frame
                        (proxy [java.awt.event.WindowAdapter] []
                          (windowClosing [event]
                                         (.dispose frame))))
    (.pack frame)
    (.show frame))
