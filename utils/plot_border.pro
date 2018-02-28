PRO PLOT_BORDER, FILE=file, VAR=var,    $
                 MAT_LAT=lat, MAT_LON=lon, $
                 COL=col, TH=th
    ;draws the border of the domain given in a standard file

    ;make sure file exists
    IF N_ELEMENTS(file) NE 0 THEN BEGIN
        IF ~FILE_TEST(file) THEN BEGIN
            MESSAGE, 'file: ', /INFORMATIONAL
            MESSAGE, file, /INFORMATIONAL
            MESSAGE, 'does no exist'
        ENDIF
    ENDIF

    ;if color not specified, use black
    IF N_ELEMENTS(col) EQ 0 THEN BEGIN
        LOADCT, 40, /S
        col=255
    ENDIF

    ;if lat and lon are not provided get them from file
    IF (N_ELEMENTS(lat) EQ 0) AND (N_ELEMENTS(lon) EQ 0) THEN BEGIN
        ;set default var to look for
        IF N_ELEMENTS(var) EQ 0 THEN var = 'UU'

        ;get lat/lon of domain
        CALL_GDLL, file, var, lat, lon
    ENDIF

    ;size of domain
    sz = SIZE(lat, /DIM)
    nx = sz[0]
    ny = sz[1]

    ;all points on the border avoiding repetition of corners
    lat_arr = [lat[*,0],REFORM(lat[nx-1,1:ny-1]),REVERSE(lat[0:nx-2,ny-1]),REVERSE(REFORM(lat[0,0:ny-2]))]
    lon_arr = [lon[*,0],REFORM(lon[nx-1,1:ny-1]),REVERSE(lon[0:nx-2,ny-1]),REVERSE(REFORM(lon[0,0:ny-2]))]

    ;plot border in device space
    PLOTS, lon_arr, lat_arr, COL=col, TH=th




END
