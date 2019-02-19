These 2 demos are intended to showcase Clipper triangulation. 

The widely used OpenGL graphics library is a good example of why Clipper's new triangulation nodule may be useful, since OpenGL will only rasterize polygons (including text) once they've been triangulated.


Polygon Demo (Simple):
This demo renders polygons onto the application surface using OpenGL. 

Text Demo (Complex):
This demo is mostly a side project on rasterizing text, and uses OpenGL and Clipper (and my new Image32 graphics library) to achieve this. The rasterized text is consistently of high quality when viewed on high resolution monitors using font scaling (DPI > 96). However, on standard resolution monitors (dpi = 96), text rendering quality is of variable quality, though still mostly pretty good. With smaller font heights, sub-pixel rendering is recommended to avoid noticeable blurring though sometimes sub-pixel rendering can cause annoying text colorization. (This text demo requires OpenGL drivers vers 3.0 or higher.)

Finally, please note that I'm not claiming any proficiency with OpenGL. If there are things that I'm doing wrong, or could do much better, I would value the feedback.