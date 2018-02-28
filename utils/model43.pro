;+
; MODEL43
; given         distance along the radar beam       : dist_beam
;               elevation angle                     : elev
;
;   the two inputs are assumed to be the same dimensions
;
; computes
;               altitude of radar beam                                                  :height
;               distance to radar on the  on the surface of the earth   (optionnal)     :dist_earth
;
;   all distance in km   angles in degrees
;
; uses the 4/3 model by Doviak and Zrnic
; original function by Bernat
;-
PRO MODEL43, dist_beam, elev, height, DIST_EARTH=dist_earth, RADAR_HEIGHT=hrad, RADIUS=radius


  IF N_ELEMENTS(hrad) EQ 0 THEN hrad=0.

  IF ~KEYWORD_SET(radius) THEN R=6371.0072d   ;; Authalic radius of the eart ("equal area - hypothetical perfect sphere")
  Re=R*(4/3.)

  ;; Doviak and Zrnic (2.28b)
  height=SQRT(dist_beam^2. + (Re+hrad)^2. + 2.*dist_beam * (Re+hrad) * SIN(elev*!DTOR)) - Re
  
  IF ARG_PRESENT(dist_earth) THEN BEGIN
    
    ;; Doviak and Zrnic (2.28c)
    dist_earth=ATAN(dist_beam * COS(elev*!DTOR) / (dist_beam * SIN(elev*!DTOR) + Re + hrad)) * Re
    ;     dist_earth[*,i]=ASIN(dist_beam * COS(elev[i]*!DTOR) / (Re + height[*,i])) * Re ;; The same result
  ENDIF
END 
