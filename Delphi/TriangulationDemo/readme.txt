These 2 demos are intended to showcase Clipper triangulation. 

The widely used OpenGL graphics library is a good example of why Clipper's new triangulation unit may be useful, since OpenGL will only rasterize polygons (including text) once they've been triangulated. This demo only requires drivers for OpenGL 1.1.


Polygon Demo (Simple):
This demo renders polygons onto the application surface using OpenGL. 

Text Demo (Complex):
This demo is mostly a side project to rasterize truetype vector fonts. It uses OpenGL, Clipper triangulation and my Image32 graphics library to achieve this. This demo requires drivers for OpenGL 3.3 (or higher).

Please note that I'm not claiming any proficiency with OpenGL. If there are things that I'm doing wrong (which is likely), or I could do much better, I would value the feedback.