These 2 demos are intended to showcase Clipper triangulation. 

The widely used OpenGL graphics library is a good example of why Clipper's new triangulation nodule may be useful, since OpenGL will only rasterize polygons (including text) once they've been triangulated.


Polygon Demo (Simple):
This demo renders polygons onto the application surface using OpenGL. 

Text Demo (Complex):
This demo is mostly a side project to rasterize truetype (vector) fonts, and uses OpenGL and Clipper (and my new Image32 graphics library) to achieve this. It's now at the point that it renders consistently high quality text, whether it's viewed on high resolution or standard resolution monitors. Previous problems with blurring of smaller fonts and text colorization with sub-pixel rendering have been addressed. (This text demo requires OpenGL drivers vers 3.0 or higher.)

Finally, please note that I'm not claiming any proficiency with OpenGL. If there are things that I'm doing wrong, or could do much better, I would value the feedback.