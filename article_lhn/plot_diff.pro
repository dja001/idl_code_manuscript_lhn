PRO PLOT_DIFF, ii, jj, exp_str, nat_str, missing, range,                  $;base info
               ps, loc,      $;image position info
               this_diff,var, units, $
               LEN_ARR=len_arr, P_ARR=p_arr, LAT_ARR=lat_arr, LON_ARR=lon_arr, VERT=vert ;data input
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
    LEGS, RANGE=range, $
          COLOR_ARR=blue_orange, SOLID='supplied', $ 
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-3], $
          OVER_HIGH='extend', UNDER_LOW='extend', $
          MAPPING=mapping_diff

    IF KEYWORD_SET(vert) THEN BEGIN
        ;figure position
        fact = .7
        pos = [x0,y0,x0+ps.rec_w,y0+fact*ps.rec_h]

        ;plot image
        LEGS, DATA=this_diff, MAPPING=mapping_diff,  TV_OUT=pos
        ;axis
        nx_is = N_ELEMENTS(len_arr)
        nk_is = N_ELEMENTS(p_arr)
        PLOT, [0], /NODATA, /NOERASE, /NORMAL, POS=pos, COL=0,  $
                   XS=1, XRANGE=[len_arr[0],len_arr[nx_is-1]], XTITLE='distance [km]', $
                   YS=1, YRANGE=[  p_arr[0],  p_arr[nk_is-1]], YTITLE='Pressure [hPa]'
        ;titles
        XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.90*(pos[3]-pos[1]),var+'_exp - '+var+'_ref',/NORMAL,CHARSIZE=2., COL=0

        ;palettes
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+fact*ps.rec_h]
        LEGS, MAPPING=mapping_diff, PALETTE=pos, PAL_PROP='equal', UNITS=units
    ENDIF ELSE BEGIN
        ;make pan-am image
        APPLY_PROJ, this_diff, exp_str[jj].proj_ind, proj_diff_pan, MISSING=missing
        ;apply color mapping
        pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
        LEGS, DATA=proj_diff_pan, MAPPING=mapping_diff,  TV_OUT=pos

        ;titles
        XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.90*(pos[3]-pos[1]),var+'_exp - '+var+'_ref',/NORMAL,CHARSIZE=1.7, COL=0
        ;overlay grid
        MAPS, POS=pos, /GRID, /MAP, LOC=loc
        ;PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon
        PLOT_BORDER, MAT_LAT=exp_str[jj].lat, MAT_LON=exp_str[jj].lon

        ;;plot location of cross section
        LOADCT,40,/S
        IF N_ELEMENTS(lat_arr) NE 0 THEN PLOTS, lon_arr, lat_arr, TH=5., COL=0
        
        ;palette
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        LEGS, MAPPING=mapping_diff, PALETTE=pos, PAL_PROP='equal', UNITS=units
    ENDELSE
END
