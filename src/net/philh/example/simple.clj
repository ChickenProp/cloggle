;; Opens a window and puts a triangle inside it. As simple as it gets.

(use 'net.philh.cloggle)

(let [frame (new java.awt.Frame)
	canvas (new javax.media.opengl.GLCanvas)]
    (.addGLEventListener canvas
       (proxy [javax.media.opengl.GLEventListener] []
	 (init [x])
	 (reshape [drawable x y w h]
		  (ctx (.getGL drawable)
		    (glMatrixMode GL_PROJECTION)
		    (glLoadIdentity)
		    (glOrtho 0 1 0 1 -1 1)
		    (glMatrixMode GL_MODELVIEW)
		    (glLoadIdentity))) 
	 (display [drawable]
		  (ctx (.getGL drawable)
		    (glClear GL_COLOR_BUFFER_BIT)
		    (glClear GL_DEPTH_BUFFER_BIT)
		    (glColor3d 1 0.5 1)
		    (beg-end GL_TRIANGLES
		      (glVertex2d 0 0)
		      (glVertex2dv [1 0.5] 0)
		      (glVertex2d 0 1))))))
    (.setSize canvas 100 100)
    (.add frame canvas)
    (.addWindowListener frame
			(proxy [java.awt.event.WindowAdapter] []
			  (windowClosing [event]
					 (.dispose frame))))
    (.pack frame)
    (.show frame))
