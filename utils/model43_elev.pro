;+
; MODEL43_elev
; given         distance to radar on the on the surface of the earth       : dist_earth
;               altitude of radar beam                                     :height
;
;   the two inputs are assumed to be the same dimensions
;
; computes
;               elevation angle                     : elev
;
;   all distance in km   angles in degrees
;
; uses the 4/3 model by Doviak and Zrnic
; original function by Bernat
;-
PRO MODEL43_ELEV, dist_earth, height, elev, RADAR_HEIGHT=hrad, RADIUS=R

  ON_ERROR, 2
     
    IF N_ELEMENTS(dist_earth) EQ 0 OR N_ELEMENTS(height) EQ 0 THEN MESSAGE, 'Incorrect number of arguments'
       
    IF N_ELEMENTS(hrad) EQ 0 THEN hrad=0D
    IF N_ELEMENTS(R) EQ 0 THEN R=6371.0072D

    Re=R*4/3D
    
    RADEG=180/!DPI
     
    distRe=(dist_earth / Re)
    
    cos_distRe=COS(distRe)
    sin_distRe=SIN(TEMPORARY(distRe))
    
    ;; Doviak and Zrnic (2.28a)
    elev=((-1)/sin_distRe) * (((Re+hrad) / (height+Re)) - cos_distRe)
    elev=ATAN(elev)*RADEG
       
END
