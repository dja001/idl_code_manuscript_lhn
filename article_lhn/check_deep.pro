PRO check_deep
    
    exper = 'bbb'

    pic_dir = '~/documents/ps/'
    pic_name = pic_dir+'check_deep_'+exper+'.ps'

    missing = -9999.
    loc = 'nat_enkf'
    jtime=JULDAY(07,08,2014,13,00)
    JUL_TO_CMC, jtime, cmc_time
    jtime=JULDAY(07,08,2014,17,25)
    JUL_TO_CMC, jtime, cmc_time_mdt


    mod_file = '/local/drive2/arma/armadja/data/lhn_test_outputs/'+exper+'/2014070812_001'
    GET_GEM_DATA, mod_file, VAR_NAME='KF', VALUES=kf,    CMC_TIMESTAMP=cmc_time
    GET_GEM_DATA, mod_file, VAR_NAME='TREE', VALUES=tree,    CMC_TIMESTAMP=cmc_time
    GET_GEM_DATA, mod_file, VAR_NAME='TLHN', VALUES=tlhn, P_FROM_VAR=pres,    CMC_TIMESTAMP=cmc_time
    GET_GEM_DATA, mod_file, VAR_NAME='TLHS', VALUES=tlhs, CMC_TIMESTAMP=cmc_time
    ;GET_GEM_DATA, mod_file, VAR_NAME='TT', VALUES=tt,  CMC_TIMESTAMP=cmc_time
    ;GET_GEM_DATA, mod_file, VAR_NAME='HU', VALUES=hu,  CMC_TIMESTAMP=cmc_time
    ;GET_GEM_DATA, mod_file, VAR_NAME='2T', VALUES=t2,  CMC_TIMESTAMP=cmc_time
    sz = SIZE(kf,/DIM)
    nx = sz[0]
    ny = sz[1]
    nz = N_ELEMENTS(pres[0,0,*])

    ;aa = WHERE(tree GT 2.0 AND tree LT 3.0 , naa)
    aa = WHERE(tree EQ 3.0 , naa)
    print, naa
    yp = FLOOR(aa[0]/nx)
    xp = aa[0] - yp*nx
    for kk=nz-1,0,-1 do begin
        print, 300.*tlhn[xp,yp,kk],pres[xp,yp,kk]
    endfor
    print, 'AAAAA', tlhs[xp,yp]
    stop

    ;rh computation
    ;YAUA = 2.53e11                  
    ;YAUB = 5.42e3                   
    ;EPSIL = 0.622                   
    ;temp_k  = tt + 273.
    ;pres_pa = pres*100.
    ;es      = YAUA*exp(-1.*YAUB/temp_k)
    ;qvs     = EPSIL*es/pres_pa
    ;rh      = hu/qvs
    ;print, 'NPTS', naa, xp, yp
    ;print, tree[xp,yp]
    ;for kk=nz-1,0,-1 do begin
    ;    print, tlhn[xp,yp,kk],rh[xp,yp,kk], pres_pa[xp,yp,kk]
    ;endfor








   





    ;;m/s to mm/5min
    ;kf = kf*1e3*300.
    ;print, kf[*,200]
    ;MAXMIN, kf,NAME='KF'

    ;GET_GEM_DATA, mod_file,   GETVAR='PR', LAT=mod_lat, LON=mod_lon

    ;GET_GEM_DATA, mod_file, VAR_NAME='PR', VALUES=mod_pr,    CMC_TIMESTAMP=cmc_time
    ;GET_GEM_DATA, mod_file, VAR_NAME='PR', VALUES=mod_pr_mdt,CMC_TIMESTAMP=cmc_time_mdt
    ;mod_r = (mod_pr - mod_pr_mdt)/5.*60.*1000.
    ;stop

    ;;GET_GEM_DATA, mod_file, VAR_NAME='RDQI', VALUES=mod_r,CMC_TIMESTAMP=cmc_time_mdt


    ;sz = SIZE(mod_lat,/DIM)
    ;mod_nx = sz[0]
    ;mod_ny = sz[1]
    ;

    ;;projection of model data on common grid
    ;mod_ind='~/documents/idl_sav_files/model_inout_test.sav'
    ;    ;;Z buffer stuff to set projection indices
    ;    ;SET_PLOT, 'Z'
    ;    ;; projection image size in pixels
    ;    ;sz_zbuf = [800,800]     ;data square smaller than this because of boundaries
    ;    ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
    ;    ;DEVICE, DECOMPOSED=0
    ;    ;;map projection
    ;    ;sub_domain = [0,0.,1.,1.]
    ;    ;    MAPS, POS=sub_domain, LATS=mod_lat, LONS=mod_lon, MISSING=missing, PROJ_IND=proj_ind_mod, LOC=loc
    ;    ;        ;mis_proj_up
    ;    ;        arr_lat=mod_lat[*,mod_ny-1]
    ;    ;        arr_lon=mod_lon[*,mod_ny-1]
    ;    ;        nn=5
    ;    ;        mis_lon=REBIN(arr_lon,mod_nx,nn)
    ;    ;        mis_lat=FLTARR(mod_nx,nn)
    ;    ;        FOR ii=0D,nn-1 DO mis_lat[*,ii] = arr_lat + .5*ii
    ;    ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_up, LOC=loc
    ;    ;        missing_up = WHERE(proj_mis_up NE missing, nmis)
    ;    ;        IF nmis NE 0 THEN proj_ind_mod[missing_up]=missing
    ;    ;        ;mis_proj_down
    ;    ;        arr_lat=mod_lat[*,0]
    ;    ;        arr_lon=mod_lon[*,0]
    ;    ;        nn=5
    ;    ;        mis_lon=REBIN(arr_lon,mod_nx,nn)
    ;    ;        mis_lat=FLTARR(mod_nx,nn)
    ;    ;        FOR ii=nn-1,0,-1 DO mis_lat[*,ii] = arr_lat - .5*ii
    ;    ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_do, LOC=loc
    ;    ;        missing_do = WHERE(proj_mis_do NE missing, nmis)
    ;    ;        IF nmis NE 0 THEN proj_ind_mod[missing_do]=missing
    ;    ;    ;save projection indices
    ;    ;    help, proj_ind_mod
    ;    ;    SAVE, proj_ind_mod, FILENAME=mod_ind, /COMPRESS
    ;    ;DEVICE, Z_BUFFERING = 0
    ;RESTORE, mod_ind;, /VERBOSE
    ;
    ;;;projection of radar data on common grid
    ;;rad_ind='~/documents/idl_sav_files/lhn_rad.sav'
    ;;    ;;Z buffer stuff to set projection indices
    ;;    ;SET_PLOT, 'Z'
    ;;    ;; projection image size in pixels
    ;;    ;sz_zbuf = [800,800]     ;data square smaller than this because of boundaries
    ;;    ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
    ;;    ;DEVICE, DECOMPOSED=0
    ;;    ;;map projection
    ;;    ;sub_domain = [0,0.,1.,1.]
    ;;    ;    MAPS, POS=sub_domain, LATS=rad_lat, LONS=rad_lon, MISSING=missing, PROJ_IND=proj_ind_rad, LOC=loc
    ;;    ;    ;save projection indices
    ;;    ;    help, proj_ind_rad
    ;;    ;    SAVE, proj_ind_rad, FILENAME=rad_ind, /COMPRESS
    ;;    ;DEVICE, Z_BUFFERING = 0
    ;;RESTORE, rad_ind;, /VERBOSE

    ;;compute precip diff and factor for profile adjustment
    ;APPLY_PROJ, mod_r, proj_ind_mod, proj_model_r, MISSING=missing
    ;APPLY_PROJ, kf, proj_ind_mod, proj_kf, MISSING=missing
    ;;proj_diff = proj_model_r - proj_radar_r
    ;;aa = WHERE((proj_radar_r EQ missing), caa)
    ;;IF caa NE 0 THEN proj_diff[aa] = 0.

    ;;color mapping for rr
    ;base = 10.
    ;range = [base/64.,base/32.,base/16.,base/8.,base/4.,base/2.,base]
    ;;range = [.5,1.5,2.5,3.5]
    ;LEGS, RANGE=range, $
    ;      COLOR_ARR=['brown','blue','green','orange','red','pink'], $
    ;      ;COLOR_ARR=['blue','green','orange'], $
    ;      EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-2], $
    ;      OVER_HIGH='extend', UNDER_LOW='white',   $
    ;      MAPPING=mapping_rr

    ;;range for reflectivity
    ;range = [0., 60]
    ;;make mapping structure
    ;LEGS, RANGE=range, N_COL=6, OVER_HIGH='extend', UNDER_LOW='white', EXCEP_VAL=missing, EXCEP_COL='white', $
    ;      MAPPING=mapping_ref

    ;;color mapping for difference
    ;blue_orange = [ [013, 013, 134],$      ;dark blue
    ;                [000, 081, 192],$
    ;                [000, 126, 237],$
    ;                [000, 169, 191],$
    ;                [153, 216, 224],$
    ;                [204, 249, 255],$      ;pale bue
    ;                [255, 255, 169],$      ;pale orange
    ;                [255, 205, 124],$
    ;                [255, 159, 071],$
    ;                [255, 119, 051],$
    ;                [164, 053, 000],$
    ;                [104, 010, 000] ]      ;dark orange
    ; LEGS, RANGE=[-1.,1.]*5., $
    ;       COLOR_ARR=blue_orange, SOLID='supplied', $ 
    ;       EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-3], $
    ;       OVER_HIGH='extend', UNDER_LOW='extend', $
    ;       MAPPING=mapping_diff

    ;set up image coords setup 
    sq_w = 10.
    sq_h = .8*sq_w
    pic_h = 15.
    pic_w = 15.*3
    pal_sp = 1.3/pic_w
    pal_w = .25/pic_w
    rec_w = sq_w/pic_w
    rec_h = sq_h/pic_h
    sp_w = 2./pic_w
    sp_h = 2./pic_h
    x1 = 2.2/pic_w 
    x2 = x1 + rec_w + sp_w
    x3 = x2 + rec_w + sp_w + pal_w + sp_w
    y1 = 1./pic_h
    y2 = y1 + rec_h + sp_h
    
    y0 = y1
    PS_START, pic_name, pic_w, pic_h
        ;;plot model rr
        x0 = x1
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;;plot image
        ;LEGS, DATA=proj_model_r, MAPPING=mapping_rr,  TV_OUT=pos
        ;;title
        ;XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'Model',/NORMAL,CHARSIZE=2., COL=0
        ;;overlay grid
        ;MAPS, POS=pos, /GRID, /MAP, LOC=loc
        ;;PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon

        ;;plot kf
        ;x0 = x2
        ;pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;;plot image
        ;LEGS, DATA=proj_kf, MAPPING=mapping_rr,  TV_OUT=pos
        ;MAXMIN,proj_kf,NAME='proj kf'
        ;;title
        ;XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'KF',/NORMAL,CHARSIZE=2., COL=0
        ;;overlay grid
        ;;MAPS, POS=pos, /GRID, /MAP, LOC=loc
        ;;PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon

        ;    ;plot palette
        ;    x0 = x0 + rec_w + sp_w
        ;    pos = [x0,y0,x0+pal_w,y0+rec_h]
        ;    ;plot image
        ;    LEGS, MAPPING=mapping_rr, PALETTE=pos, UNITS='mm/h', PAL_PROP='equal', YTICKFORMAT='(f6.1)'

        ;;;plot diff
        ;;x0 = x3
        ;;pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;;;plot image
        ;;LEGS, DATA=proj_diff, MAPPING=mapping_diff,  TV_OUT=pos
        ;;;title
        ;;XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'Radar',/NORMAL,CHARSIZE=2., COL=0
        ;;;overlay grid
        ;;MAPS, POS=pos, /GRID, /MAP, LOC=loc
        ;;;PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon

        ;;    ;plot palette
        ;;    x0 = x0 + rec_w + sp_w
        ;;    pos = [x0,y0,x0+pal_w,y0+rec_h]
        ;;    ;plot image
        ;;    LEGS, MAPPING=mapping_diff, PALETTE=pos, UNITS='mm/h';, PAL_PROP='equal'

        avgp = [5.000e+01,  1.500e+02 , 2.500e+02 , 3.500e+02 , 4.500e+02 , 5.500e+02 , 6.500e+02 , 7.500e+02 , 8.500e+02 , 9.500e+02  ,1.050e+03]
        avgl = [-8.636e-14, 4.260e-06,  3.532e-04,  1.938e-03,  3.591e-03,  5.416e-03,  4.046e-03,  2.815e-03,  1.089e-03,  8.123e-05, -2.405e-04]

        minr = -.02
        maxr = .02
        LOADCT, 40, /S
        PLOT, [0], /NODATA, POS=pos, /NOERASE, /NORMAL, $
            TITLE='lhr',                                             $
            XS=1, XR=[minr,maxr], XTIT='rate of LHR',                  $
            YS=1, YR=[1000.,0.], YTIT='height [hPa]'

        ;OPLOT, avgl, avgp, COL=80, TH=3.
        OPLOT, tlhn[xp,yp,*], pres[xp,yp,*], COL=210




    PS_CLOSE, pic_name, /DEL_PS, FONT='lmroman', /V, /PDF

END

