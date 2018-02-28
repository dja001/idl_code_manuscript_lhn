FUNCTION WRF_HEIGHT_INTERPOL, desired_alt, wrf_alt, wrf_var, $
    MISSING=missing, MISSIND_IND=missing_ind, SPL=spl, LOG=log, $
    XSTAG=xstag, YSTAG=ystag
    
    ;---------------------------------------------------
    ;does interpolation of wrf data to a constant altitude level

    ;IN:
    ;desired_alt        a scalar or vector set to the desired height of interpolation in meters
    ;wrf_alt            a 3D matrix of height for the raw wrf data      take care that zstaggering is taken into account
    ;wrf_var            a 3D matrix of the wrf variable for which interpolation is desired
    ;missing            optionnal keyword setting the value of missing values,  default is -9999.
    ;spl                set spline interpolation   default is linear
    ;log                a keyword to perform pressure interpolation in log units (may be usefull for pressure interpolation)
    ;[xy]xstag         aditionnally interpolate staggered variables to 'theta' levels'

    ;OUT
    ;returns an array of size   [nx_wrf, ny wrf, n_elements(desired_alt)]
    ;---------------------------------------------------
 



    ;default missing value if keyword not specified
    IF ~KEYWORD_SET(missing) THEN missing=-9999.


    ;get size of input data and initialize output data
    sz = SIZE(wrf_var, /DIM)
    nx = sz[0]
    ny = sz[1]
    nz = N_ELEMENTS(desired_alt)
    IF nz EQ 0 THEN BEGIN
        print, 'desired altitude(s) is not defined'
        STOP
    ENDIF


    ;destager variables if desired
    IF KEYWORD_SET(xstag) THEN BEGIN                    ;xstaggering
        wrf_var = (wrf_var + SHIFT(wrf_var, -1, 0, 0))/2. 
        wrf_var = REFORM(wrf_var[0:nx-2,*,*])
        nx --
    ENDIF ELSE BEGIN
        IF KEYWORD_SET(ystag) THEN BEGIN                ;ystaggering
            wrf_var = (wrf_var + SHIFT(wrf_var, 0, -1, 0))/2. 
            wrf_var = REFORM(wrf_var[*, 0:ny-2, *])
            ny --
        ENDIF 
    ENDELSE
        

    ;initialize output variable
    interp_var = FLTARR(nx,ny,nz)       ;size of matrix to be interpolated  

    ;check if altitude correspond to wrf data
    IF sz[2] NE N_ELEMENTS(wrf_alt[0,0,*]) THEN BEGIN
        print, 'wrf data and altitude mismatch, check staggering'
        stop
    ENDIF


    ;transfor in log unite if desired
    IF KEYWORD_SET(log) THEN wrf_var = ALOG(wrf_var)


    ;perform interpolation for every data column 
    FOR i=0, nx-1 DO BEGIN
        FOR j=0, ny-1 DO BEGIN
            interp_var[i,j,*] = INTERPOL(wrf_var[i,j,*], wrf_alt[i,j,*], desired_alt, SPLINE=spl)
            
            ;set missing values outside of bounds
            mm = WHERE(desired_alt LT wrf_alt[i,j,0], count)
            IF count NE 0 THEN interp_var[i,j,mm] = missing
            
        ENDFOR
    ENDFOR


    ;back to linear units if log interpol was desired
    IF KEYWORD_SET(log) THEN BEGIN
        aa = WHERE(interp_var EQ missing, count)
        interp_var = EXP(interp_var)
        IF count NE 0 THEN interp_var[aa] = missing
        
    ENDIF



    RETURN, REFORM(interp_var)

END
