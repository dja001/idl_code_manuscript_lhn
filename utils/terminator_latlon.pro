;   output array of lats and long of the terminator around the globe
;   based on cgTerminatorMap by david fanning
;
PRO cgTerminatorMap, time, tlon, tlat

   ;time = systime()
   ;time = julday(07,21,14,5,00,00)
   time_copy = time
   offset = 0.; time provided in GMT        GMT_OFFSEC()                      ; Find correction from local time to Universal Time (UT).
   DT_TM_INC, time_copy, offset               ; Convert time to UT.
   DT_TM_BRK, time_copy, date_part, time_part ; Break into a date and a time part.
   DATE2YMD, date_part, y, m, d               ; Break data into year, month, and day.
   jd = YMD2JD(y, m, d)                       ; Convert date to julian day.
   ut = SECSTR(time_part)/3600.               ; Convert time string (in seconds) to sun ephemeris time in hours.

    ; Calculate solar RA and DEC and distance (in AU).
   SUN, y, m, d, ut, $
      APP_RA=ra, $           ; Apparent right accession.
      APP_DEC=dec, $         ; Apparent declination.
      DIST=sun_distance      ; Distance to sun in AU.

   ; Calculate local mean sidereal time. LMST returns value as fraction of a day,
   lm_sidereal_time = LMST(jd, ut/24.0, 0) * 24

   ; Calculate sub-solar point.
   sun_lat = dec
   sun_lon = 15.0 * (ra - lm_sidereal_time)
   
   ; Calculate the terminus.
   earthRadius = 6.371009D6 ; The mean Earth radius.
   scanAngle = ASIN(earthRadius / (sun_distance * 1.4956D11))
   arc_distance = !DPI - 1.57080D - scanAngle  ; 90 degrees - scanangle (but here in radians)

   lon_terminus = FltArr(36)
   lat_Terminus = FltArr(36)
   FOR j=0,35 DO BEGIN
      count = j
      IF Keyword_Set(flipday) THEN count = -count
      azimuth =  count * 10.0 
      results = LL_Arc_Distance([sun_lon, sun_lat], arc_distance, azimuth, /Degrees)
      
      ; You have to add the center longitude to the result here. I don't know why!!
      lon_terminus[j] = results[0]
      lat_terminus[j] = results[1]
   ENDFOR

   tlon = lon_terminus
   tlat = lat_terminus

   print, tlon
   print, ''
   print, tlat
   
END
