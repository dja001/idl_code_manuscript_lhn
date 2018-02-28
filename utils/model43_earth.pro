;+
; MODEL43_EARTH 
; given         distance to radar on the  on the surface of the earth       : dist_earth
;               elevation angle                                             : elev
;
;   the two inputs are assumed to be the same dimensions
;
; computes
;               altitude of radar beam                                      :height
;               distance along the radar beam   (optionnal)                 :dist_beam
;
;   all distance in km   angles in degrees
;
; uses the 4/3 model by Doviak and Zrnic
; original function by Bernat
;-
PRO MODEL43_EARTH, dist_earth, elev, height, DIST_BEAM=dist_beam, RADAR_HEIGHT=hrad, RADIUS=R

  ON_ERROR, 2
  IF N_ELEMENTS(dist_earth) EQ 0 OR N_ELEMENTS(elev) EQ 0 THEN MESSAGE, 'Specify input arguments'
  IF N_ELEMENTS(dist_earth) NE N_ELEMENTS(elev) THEN MESSAGE, 'input arguments not of the same size'

  IF N_ELEMENTS(hrad) EQ 0 THEN hrad=0D
  IF N_ELEMENTS(R) EQ 0 THEN R=6371.0072D

  ;4/3 radius of earth
  Re=R*4/3D
       
  ;angles in radiants
  DTOR=!DPI/180
  elev_rad=elev*DTOR
     
  ;; Doviak and Zrnic (2.28a)
  height=(COS(elev_rad) / COS(elev_rad + dist_earth/Re)) * (Re+hrad) - Re
             
  IF ARG_PRESENT(dist_beam) THEN BEGIN
    ;; Doviak and Zrnic (2.28c)
    dist_beam=TAN(dist_earth/Re) * (Re+hrad) / (COS(elev_rad) - SIN(elev_rad) * TAN(dist_earth/Re))
  ENDIF
       
END
