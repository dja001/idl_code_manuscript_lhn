PRO PLOT_QD_ZET, ii, jj, exp_str, nat_str,missing,                           $ ;base info
                   ps,loc,      $ ;image position info
                   qd, p_qd, precip, p_precip, lat_arr, lon_arr, len_arr, p_arr, qd_dat  ;data input
    x0 = ps.x1
    y0 = ps.y1

    LEGS, RANGE=[0.,.7], $
          COLOR_ARR=['b_w'], DARK_POS=['high'], $
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,.1], $
          OVER_UNDER='extend',   $
          MAPPING=mapping_qd
    LEGS, RANGE=[0,1.], $
          COLOR_ARR=['blue','green','orange','red','pink'], $
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-2], $
          OVER_HIGH='extend', UNDER_LOW='white',   $
          MAPPING=mapping_accum

    ;qd to g/kg
    qd *= 1e3                        ;kg/kg -> g/kg
    ;reflectivity to g/kg
    precip  = 10.^((precip - 43.1)/17.5)  ;dBz -> g/kg

    ;qd cross section
    CROSS_SEC_IMG, exp_str[jj].lat, exp_str[jj].lon, qd, p_qd, $;in
                   lat_arr, lon_arr, len_arr, p_arr, qd_dat,           $;out
                   I_IND=i_ind, J_IND=j_ind    
    MAXMIN, qd_dat,  NAME='QD in CC: ', MISSING=missing
    ;zet cross section
    CROSS_SEC_IMG, exp_str[jj].lat, exp_str[jj].lon, precip,  p_precip,$;in
                   lat_arr, lon_arr, len_arr, p_arr, precip_dat              ;out
    MAXMIN, precip_dat, NAME='ZET in CC: ', MISSING=missing

    ;coordinates for contour plot
    nx_is = N_ELEMENTS(len_arr)
    nk_is = N_ELEMENTS(p_arr)
    xx = REBIN(len_arr,nx_is,nk_is)
    yy = REBIN(TRANSPOSE(p_arr),nx_is,nk_is)

    ;apply color mapping
    LEGS, DATA=qd_dat,     MAPPING=mapping_qd,    IMG_OUT=qd_rgb
    LEGS, DATA=precip_dat, MAPPING=mapping_accum, IMG_OUT=precip_rgb

    ;;determine transparency
    ;mm1 = 0.
    ;mm2 = .5
    ;LINMAP, qd_dat, alpha, [mm1,mm2],[0.,1.]
    alpha = REPLICATE(0.,nx_is,nk_is)
    aa = WHERE(qd_dat GE .1, naa)
    IF naa NE 0 THEN alpha[aa] = 1.
    
    ;;mix the two images
    IMG_MIX, rgb_out, precip_rgb, qd_rgb, alpha
    ;rgb_out = qd_rgb
    
    ;figure position
    fact = .7
    pos = [x0,y0,x0+ps.rec_w,y0+fact*ps.rec_h]

    ;plot images
    LOADCT,0,/S
    TV, rgb_out, pos[0], pos[1], XS=pos[2]-pos[0], YS=pos[3]-pos[1], /NORMAL, TRUE=3
    PLOT, [0], /NODATA, /NOERASE, /NORMAL, POS=pos, COL=0,  $
               XS=1, XRANGE=[len_arr[0],len_arr[nx_is-1]], XTITLE='distance [km]', $
               YS=1, YRANGE=[  p_arr[0],  p_arr[nk_is-1]], YTITLE='Pressure [hPa]'
    ;XYOUTS, pos[0]+.05*(pos[2]-pos[0]),pos[1]+.89*(pos[3]-pos[1]),'b/w: QD ',/NORMAL,CHARSIZE=1.5
    ;XYOUTS, pos[0]+.05*(pos[2]-pos[0]),pos[1]+.79*(pos[3]-pos[1]),'color: water=f(ZET)',/NORMAL,CHARSIZE=1.5

    ;;  blue,  green, orange, red, pink
    ;;   .2    .4    .6       .8   1.
    ;;    0     1     2        3   4
    ;r = [000, 000,  255,     158, 096]
    ;g = [081, 134,  086,     000, 056]
    ;b = [237, 000,  000,     013, 255]
    ;TVLCT, r,g,b
    ;CONTOUR, zet_dat, xx,yy,LEVELS=[.2], C_THICK=2.4, C_COL=0, /OVERPLOT
    ;CONTOUR, zet_dat, xx,yy,LEVELS=[.4], C_THICK=2.4, C_COL=1, /OVERPLOT
    ;CONTOUR, zet_dat, xx,yy,LEVELS=[.6], C_THICK=2.4, C_COL=2, /OVERPLOT
    ;CONTOUR, zet_dat, xx,yy,LEVELS=[.8], C_THICK=2.4, C_COL=3, /OVERPLOT
    ;CONTOUR, zet_dat, xx,yy,LEVELS=[1.], C_THICK=2.4, C_COL=4, /OVERPLOT

    ;palettes
    nexp = N_ELEMENTS(exp_str)
    IF jj EQ nexp-1 THEN BEGIN
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+fact*ps.rec_h]
        LEGS, MAPPING=mapping_accum, PALETTE=pos, PAL_PROP='equal', UNITS='mm per min'
        x0 = x0 + ps.pal_w+2.*ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+fact*ps.rec_h]
        LEGS, MAPPING=mapping_qd, PALETTE=pos, UNITS='g/kg'
    ENDIF
END
