PRO PLOT_W_PRECIP, ii, jj, exp_str, nat_str,missing,                         $ ;base info
                   ps,loc,      $ ;image position info
                   pan_w, pan_precip, lat_arr, lon_arr                         ;data input
    x0 = ps.x1
    y0 = ps.y1

    blue_orange = [ [013, 013, 134],$      ;dark blue
                    [000, 081, 192],$
                    [000, 126, 237],$
                    [000, 169, 191],$
                    [153, 216, 224],$
                    [204, 249, 255],$      ;pale bue
                    [255, 255, 169],$      ;pale orange
                    [255, 205, 124],$
                    [255, 159, 071],$
                    [255, 119, 051],$
                    [164, 053, 000],$
                    [104, 010, 000] ]      ;dark orange
    LEGS, RANGE=[0.,1.], $
          COLOR_ARR=['b_w'], DARK_POS=['high'],$
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-5], $
          OVER_HIGH='extend', UNDER_LOW='extend', $
          MAPPING=mapping_w

    LEGS, RANGE=[0,1.], $
          COLOR_ARR=['blue','green','orange','red','pink'], $
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-2], $
          OVER_HIGH='extend', UNDER_LOW='white',   $
          MAPPING=mapping_precip

    ;make pan-am image
    APPLY_PROJ, pan_w,      exp_str[jj].proj_ind, proj_w,      MISSING=missing
    APPLY_PROJ, pan_precip, exp_str[jj].proj_ind, proj_precip, MISSING=missing
    ;apply color mapping
    LEGS, DATA=proj_w,      MAPPING=mapping_w,      IMG_OUT=rgb_w
    LEGS, DATA=proj_precip, MAPPING=mapping_precip, IMG_OUT=rgb_precip
    ;mix the two images
    mm1 = 0.
    mm2 = .2
    LINMAP, proj_precip, alpha, [mm1,mm2],[0.,.5]
    ;alpha=FLTARR(SIZE(proj_acc_pan,/DIM))
    ;aa = WHERE(proj_acc_pan GE min_acc, caa)
    ;IF caa NE 0 THEN alpha[aa] = 1.
    IMG_MIX, rgb_blended_pan, rgb_w, rgb_precip, alpha
    
    ;finally plot image
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;image
    LOADCT,0,/S
    TV, rgb_w, pos[0], pos[1], XS=pos[2]-pos[0], YS=pos[3]-pos[1], /NORMAL, TRUE=3
    ;titles
    XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.88*(pos[3]-pos[1]),'color: water from reflectivity',/NORMAL,CHARSIZE=1.8, COL=0
    XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.95*(pos[3]-pos[1]),'b/w:   vertical velocity',/NORMAL,CHARSIZE=1.8, COL=0
    ;overlay grid
    MAPS, POS=pos, /GRID, /MAP, LOC=loc
    PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon
    PLOT_BORDER, MAT_LAT=exp_str[jj].lat, MAT_LON=exp_str[jj].lon
    
    ;;plot location of cross section
    LOADCT,40,/S
    IF N_ELEMENTS(lat_arr) NE 0 THEN PLOTS, lon_arr, lat_arr, TH=5., COL=210

    ;palettes
    nexp = N_ELEMENTS(exp_str)
    IF jj EQ nexp-1 THEN BEGIN
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        LEGS, MAPPING=mapping_precip, PALETTE=pos, PAL_PROP='equal', UNITS='mm in 1 min'
        x0 = x0 + ps.pal_w+2.*ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        LEGS, MAPPING=mapping_w, PALETTE=pos, UNITS='m/s'
    ENDIF
END
