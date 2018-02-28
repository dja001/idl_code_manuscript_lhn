PRO PLOT_TT, ii, jj, exp_str, nat_str,missing,                        $ ;base info
             ps,loc,     $ ;image position info
             tt, p_tt, lat_arr, lon_arr, len_arr, p_arr, tt_dat         ;data input
    x0 = ps.x1
    y0 = ps.y1

    LEGS, RANGE=[-20.,30.], $
          COLOR_ARR=['blue','green','orange','red','pink'], $
          DARK_POS= ['low', 'low','high','high','high'], $
          EXCEP_VAL=[missing,-3000.], EXCEP_COL=['grey_230','brown'], EXCEP_TOL=[1e-3,1.], $
          OVER_HIGH='extend', UNDER_LOW='extend',$
          MAPPING=mapping_tt

    ;qd cross section
    CROSS_SEC_IMG, exp_str[jj].lat, exp_str[jj].lon, tt, p_tt, $;in
                   lat_arr, lon_arr, len_arr, p_arr, tt_dat,   $;out
                   I_IND=i_ind, J_IND=j_ind    
    MAXMIN, tt_dat,  NAME='TT in CC: ', MISSING=missing

    ;coordinates for contour plot
    nx_is = N_ELEMENTS(len_arr)
    nk_is = N_ELEMENTS(p_arr)
    xx = REBIN(len_arr,nx_is,nk_is)
    yy = REBIN(TRANSPOSE(p_arr),nx_is,nk_is)


    ;apply color mapping
    LEGS, DATA=tt_dat,     MAPPING=mapping_tt,    IMG_OUT=tt_rgb

    ;figure position
    fact = .7
    pos = [x0,y0,x0+ps.rec_w,y0+fact*ps.rec_h]

    ;plot images
    LOADCT,0,/S
    TV, tt_rgb, pos[0], pos[1], XS=pos[2]-pos[0], YS=pos[3]-pos[1], /NORMAL, TRUE=3
    PLOT, [0], /NODATA, /NOERASE, /NORMAL, POS=pos, COL=0,  $
               XS=1, XRANGE=[len_arr[0],len_arr[nx_is-1]], XTITLE='distance [km]', $
               YS=1, YRANGE=[  p_arr[0],  p_arr[nk_is-1]], YTITLE='Pressure [hPa]'
    XYOUTS, pos[0]+.05*(pos[2]-pos[0]),pos[1]+.89*(pos[3]-pos[1]),'TT ',/NORMAL,CHARSIZE=2.

    ;palettes
    nexp = N_ELEMENTS(exp_str)
    IF jj EQ nexp-1 THEN BEGIN
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+fact*ps.rec_h]
        LEGS, MAPPING=mapping_tt, PALETTE=pos, PAL_PROP='equal', UNITS='Deg C',YTICKFORMAT='(f6.2)'
    ENDIF
END
