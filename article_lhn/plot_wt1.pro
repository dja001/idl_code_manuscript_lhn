PRO PLOT_WT1, ii, jj, exp_str, nat_str,missing,                        $ ;base info
             ps,loc,     $ ;image position info
             wt1, p_wt1, lat_arr, lon_arr, len_arr, p_arr, wt1_dat         ;data input
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
    LEGS, RANGE=[-1.,1.], $
          COLOR_ARR=blue_orange, SOLID='supplied', $
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,.05], $
          OVER_HIGH='extend', UNDER_LOW='extend', $
          MAPPING=mapping_wt1

    ;qd cross section
    CROSS_SEC_IMG, exp_str[jj].lat, exp_str[jj].lon, wt1, p_wt1, $;in
                   lat_arr, lon_arr, len_arr, p_arr, wt1_dat,   $;out
                   I_IND=i_ind, J_IND=j_ind    
    MAXMIN, wt1_dat,  NAME='wt1 in CC: ', MISSING=missing

    ;coordinates for contour plot
    nx_is = N_ELEMENTS(len_arr)
    nk_is = N_ELEMENTS(p_arr)
    xx = REBIN(len_arr,nx_is,nk_is)
    yy = REBIN(TRANSPOSE(p_arr),nx_is,nk_is)


    ;apply color mapping
    LEGS, DATA=wt1_dat,     MAPPING=mapping_wt1,    IMG_OUT=wt1_rgb

    ;figure position
    fact = .7
    pos = [x0,y0,x0+ps.rec_w,y0+fact*ps.rec_h]

    ;plot images
    LOADCT,0,/S
    TV, wt1_rgb, pos[0], pos[1], XS=pos[2]-pos[0], YS=pos[3]-pos[1], /NORMAL, TRUE=3
    PLOT, [0], /NODATA, /NOERASE, /NORMAL, POS=pos, COL=0,  $
               XS=1, XRANGE=[len_arr[0],len_arr[nx_is-1]], XTITLE='distance [km]', $
               YS=1, YRANGE=[  p_arr[0],  p_arr[nk_is-1]], YTITLE='Pressure [hPa]'
    XYOUTS, pos[0]+.05*(pos[2]-pos[0]),pos[1]+.89*(pos[3]-pos[1]),'wt1 ',/NORMAL,CHARSIZE=2.

    ;palewt1es
    nexp = N_ELEMENTS(exp_str)
    IF jj EQ nexp-1 THEN BEGIN
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+fact*ps.rec_h]
        LEGS, MAPPING=mapping_wt1, PALETTE=pos, PAL_PROP='equal', UNITS='m/s',YTICKFORMAT='(f6.2)'
    ENDIF
END

