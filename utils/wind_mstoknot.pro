;+
; WIND_MSTOKNOT
;
; This program transforms the wind speed m/s to knots units. The inverse 
; transformation is also available. 
;
;  1 international knot =
;    1 nautical mile per hour (exactly),
;    1.852 kilometres per hour (exactly),
;    0.514 meters per second,
;    1.15077945 miles per hour (approximately).
;
;  1.852 km is the length of the internationally-agreed nautical mile. 
;  The U.S. adopted the international definition in 1954, having previously used 
;  the U.S. nautical mile (1,853.248 m). [Wikipedia]
;
; @PARAM wind A numerical floating [array] value defining the wind speed 
; @KEYWORD INVERSE Use this keyword to transform knots to m/s 
;
; @RETURNS The wind transformed units
; @AUTHOR Bernat Puigdomenech <bernat3rs@hotmail.com>
;-
FUNCTION WIND_MSTOKNOT, wind, INVERSE=inverse

  knot=0.514444 ; 1knot = 0.514 444 m/s

  IF ~KEYWORD_SET(inverse) THEN RETURN, wind / knot $
  ELSE RETURN, wind * knot
END