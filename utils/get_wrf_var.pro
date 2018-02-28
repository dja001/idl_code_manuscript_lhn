PRO GET_WRF_VAR, filename, altitudes,                                                                                                $
                   U=wrf_u_cst, VV=wrf_v_cst, W=wrf_w_cst, POT_TEMP=wrf_pot_temp_cst,PERT_PRES=wrf_pert_pres_cst, $ 
                   REF=ref, RAIN=wrf_rain_cst, VAPOR=wrf_vapor_cst, CLOUD=wrf_cloud_cst, $
                   TEMP_K=temp_k, PRES_PA=pres_pa, NODATA=nodata, LAT=lat, LON=lon,     $
                   ALT_NOSTAG=wrf_alt, ALT_STAG=WRF_ALT_STAG     ;these keyword prevents altitude interpolation from being done, wrf_altitudes are returned through it



    ;takes in a model file read in desired variables and does the conversion to MAS/MC2 variables

    print, 'reading in '+filename
    nc_id = NCDF_OPEN(filename)

    NCDF_VARGET, nc_id, 'PH', geo
    NCDF_VARGET, nc_id, 'PHB', bgeo

    ;wrf altitudes
    wrf_alt_stag = (bgeo + geo)/9.81
    sz = SIZE(wrf_alt_stag, /DIM)
    wrf_alt = (wrf_alt_stag + SHIFT(wrf_alt_stag, 0, 0, -1))/2. 
    wrf_alt = wrf_alt[*, *, 0:sz[2]-2]

    ;read in wrf data
    IF ARG_PRESENT(lat) THEN  NCDF_VARGET, nc_id, 'XLAT', lat
    IF ARG_PRESENT(lon) THEN  NCDF_VARGET, nc_id, 'XLONG', lon
    IF ARG_PRESENT(wrf_u_cst) THEN  NCDF_VARGET, nc_id, 'U', wrf_u_sigma
    IF ARG_PRESENT(wrf_v_cst) THEN  NCDF_VARGET, nc_id, 'V', wrf_v_sigma
    IF ARG_PRESENT(wrf_w_cst) THEN  NCDF_VARGET, nc_id, 'W', wrf_w_sigma
    IF (ARG_PRESENT(wrf_pot_temp_cst) OR ARG_PRESENT(temp_k)) THEN NCDF_VARGET, nc_id, 'T', wrf_pot_temp_sigma
    IF (ARG_PRESENT(wrf_pert_pres_cst) OR ARG_PRESENT(pres_pa) OR ARG_PRESENT(temp_k)) THEN BEGIN 
        NCDF_VARGET, nc_id, 'P', wrf_pert_pres_sigma
        NCDF_VARGET, nc_id, 'PB', wrf_base_pres_sigma
    ENDIF
    IF ARG_PRESENT(wrf_rain_cst) OR ARG_PRESENT(ref) THEN NCDF_VARGET, nc_id, 'QRAIN', wrf_rain_sigma
    IF ARG_PRESENT(wrf_vapor_cst) THEN NCDF_VARGET, nc_id, 'QVAPOR', wrf_vapor_sigma
    IF ARG_PRESENT(wrf_cloud_cst) THEN NCDF_VARGET, nc_id, 'QCLOUD', wrf_cloud_sigma
    
    NCDF_CLOSE, nc_id

    ;do altitude interpolation if wrf_alt keywords not set
    IF ~ARG_PRESENT(wrf_alt) AND ~ARG_PRESENT(wrf_alt_stag) THEN BEGIN
        IF ARG_PRESENT(wrf_u_cst) THEN BEGIN 
            print, 'u'
            wrf_u_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_u_sigma, /XSTAG, MISSING=nodata )
        ENDIF
        IF ARG_PRESENT(wrf_v_cst) THEN BEGIN 
            print, 'v'
            wrf_v_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_v_sigma, /SPL, /YSTAG, MISSING=nodata )
        ENDIF
        IF ARG_PRESENT(wrf_w_cst) THEN BEGIN 
            print, 'w'
            wrf_w_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt_stag, wrf_w_sigma, /SPL, MISSING=nodata )
        ENDIF
        IF (ARG_PRESENT(wrf_pert_pres_cst) OR ARG_PRESENT(pres_pa) OR ARG_PRESENT(temp_k)) THEN BEGIN
            print, 'base_pres'
            wrf_base_pres_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_base_pres_sigma, /LOG, /SPL, MISSING=nodata )
            print, 'pert_pres'
            ;get total pressure for interpolation
            wrf_pres_sigma = wrf_base_pres_sigma + wrf_pert_pres_sigma
            wrf_pres_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_pres_sigma, /SPL, MISSING=nodata )
            ;retrieve perturbation pressure after interpolation
            wrf_pert_pres_cst = wrf_pres_cst - wrf_base_pres_cst
            ;insure nodata get into output variable
            aa = WHERE(wrf_pres_cst EQ nodata, count)
            IF count NE 0 THEN wrf_pert_pres_cst[aa] = nodata
            ;wrf variables in different units
            pres_pa = wrf_pres_cst 
        ENDIF
        IF (ARG_PRESENT(wrf_pot_temp_cst) OR ARG_PRESENT(temp_k)) THEN BEGIN
            print, 'pot_temp'
            wrf_pot_temp_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_pot_temp_sigma, /SPL, MISSING=nodata )
        ENDIF
        IF ARG_PRESENT(temp_k) THEN BEGIN
            P0 =  100.e3
            KAPPA =  0.286
            temp_k = wrf_pot_temp_cst * (pres_pa/P0)^KAPPA
            aa = WHERE(wrf_pot_temp_cst EQ nodata, count)
            IF count NE 0 THEN temp_k[aa] = nodata
        ENDIF
        IF ARG_PRESENT(wrf_rain_cst) OR ARG_PRESENT(ref) THEN BEGIN
            print, 'rain'
            wrf_rain_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_rain_sigma, MISSING=nodata )     ;no spline to avoid -ve
            IF ARG_PRESENT(ref) THEN BEGIN
                print, 'reflectivity'
                ref = 43.1 + 17.5*ALOG10(wrf_rain_cst*1000.)    ;values in dBZ
                aa = WHERE(wrf_rain_cst*1000. LT 1e-5, count)
                IF count NE 0 THEN ref[aa] = 0.
            ENDIF
        ENDIF
        IF ARG_PRESENT(wrf_vapor_cst) THEN BEGIN
            print, 'vapor'
            wrf_vapor_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_vapor_sigma,/SPL, MISSING=nodata )
        ENDIF
        IF ARG_PRESENT(wrf_cloud_cst) THEN BEGIN
            print, 'cloud'
            wrf_cloud_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_cloud_sigma, /SPL, MISSING=nodata )   ;no spline to avoid -ve
        ENDIF

    ENDIF ELSE BEGIN    ;if altitude interpolation not desired

        IF ARG_PRESENT(wrf_u_cst) THEN BEGIN 
            print, 'u'
            wrf_u_cst = wrf_u_sigma
        ENDIF
        IF ARG_PRESENT(wrf_v_cst) THEN BEGIN 
            print, 'v'
            wrf_v_cst = wrf_v_sigma
        ENDIF
        IF ARG_PRESENT(wrf_w_cst) THEN BEGIN 
            print, 'w'
            wrf_w_cst = wrf_w_sigma
        ENDIF
        IF (ARG_PRESENT(wrf_pert_pres_cst) OR ARG_PRESENT(pres_pa) OR ARG_PRESENT(temp_k)) THEN BEGIN
            print, 'base_pres'
            wrf_base_pres_cst = wrf_base_pres_sigma
            print, 'pert_pres'
            ;get total pressure for interpolation
            wrf_pres_sigma = wrf_base_pres_sigma + wrf_pert_pres_sigma
            wrf_pres_cst = wrf_pres_sigma
            ;retrieve perturbation pressure after interpolation
            wrf_pert_pres_cst = wrf_pres_cst - wrf_base_pres_cst
            ;insure nodata get into output variable
            aa = WHERE(wrf_pres_cst EQ nodata, count)
            IF count NE 0 THEN wrf_pert_pres_cst[aa] = nodata
            ;wrf variables in different units
            pres_pa = wrf_pres_cst 
        ENDIF
        IF (ARG_PRESENT(wrf_pot_temp_cst) OR ARG_PRESENT(temp_k)) THEN BEGIN
            print, 'pot_temp'
            wrf_pot_temp_cst = wrf_pot_temp_sigma
        ENDIF
        IF ARG_PRESENT(temp_k) THEN BEGIN
            P0 =  100.e3
            KAPPA =  0.286
            temp_k = wrf_pot_temp_cst * (pres_pa/P0)^KAPPA
            aa = WHERE(wrf_pot_temp_cst EQ nodata, count)
            IF count NE 0 THEN temp_k[aa] = nodata
        ENDIF
        IF ARG_PRESENT(wrf_rain_cst) OR ARG_PRESENT(ref) THEN BEGIN
            print, 'rain'
            wrf_rain_cst = wrf_rain_sigma
            IF ARG_PRESENT(ref) THEN BEGIN
                print, 'reflectivity'
                ref = 43.1 + 17.5*ALOG10(wrf_rain_cst*1000.)    ;values in dBZ
                aa = WHERE(wrf_rain_cst*1000. LT 1e-5, count)
                IF count NE 0 THEN ref[aa] = 0.
            ENDIF
        ENDIF
        IF ARG_PRESENT(wrf_vapor_cst) THEN BEGIN
            print, 'vapor'
            wrf_vapor_cst = wrf_vapor_sigma
        ENDIF
        IF ARG_PRESENT(wrf_cloud_cst) THEN BEGIN
            print, 'cloud'
            wrf_cloud_cst = WRF_HEIGHT_INTERPOL(altitudes, wrf_alt, wrf_cloud_sigma, /SPL, MISSING=nodata )   ;no spline to avoid -ve
        ENDIF
    ENDELSE





END
