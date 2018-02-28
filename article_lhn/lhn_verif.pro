PRO PLOT_DP_DT, ii, jj, exp_str, nat_str, missing,                         $;base info
                x0, y0, rec_w, rec_h, sp_w, sp_h, pal_sp, pal_w, loc,      $;image position info
                pan_dp_dt, pan_acc, nat_dp_dt, nat_acc, lat_arr, lon_arr   ;data input

    LEGS, RANGE=[0,1.], $
          COLOR_ARR=['blue','green','orange','red','pink'], $
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-2], $
          OVER_HIGH='extend', UNDER_LOW='white',   $
          MAPPING=mapping_accum

    black_white = [ [[000,000,000],[255,255,255]],[[000,000,000],[255,255,255]] ]
    LEGS, RANGE=[-.03,.03], $
          COLOR_ARR=black_white,DARK_POS=['low','high'], $
          EXCEP_VAL=missing, EXCEP_COL='grey_230',     $
          OVER_HIGH='extend', UNDER_LOW='extend',      $
          MAPPING=mapping_p0tend

    ;make national image
    APPLY_PROJ, nat_acc,   nat_str[jj].proj_ind, proj_acc_nat, MISSING=missing
    APPLY_PROJ, nat_dp_dt, nat_str[jj].proj_ind, proj_p0_nat,  MISSING=missing
    ;apply color mapping
    LEGS, DATA=proj_acc_nat, MAPPING=mapping_accum,  IMG_OUT=rgb_acc_nat
    LEGS, DATA=proj_p0_nat,  MAPPING=mapping_p0tend, IMG_OUT=rgb_p0_nat
    ;mix the two images
    mm1 = 0.
    mm2 = .2
    LINMAP, proj_acc_nat, alpha, [mm1,mm2],[0.,1.]
    ;alpha=FLTARR(SIZE(proj_acc_nat,/DIM))
    ;aa = WHERE(proj_acc_nat GE min_acc, caa)
    ;IF caa NE 0 THEN alpha[aa] = 1.
    IMG_MIX, rgb_blended_nat, rgb_p0_nat, rgb_acc_nat, alpha
    rgb_blended_nat = rgb_acc_nat
    
    ;make pan-am image
    APPLY_PROJ, pan_acc,   exp_str[jj].proj_ind, proj_acc_pan, MISSING=missing
    APPLY_PROJ, pan_dp_dt, exp_str[jj].proj_ind, proj_p0_pan,  MISSING=missing
    ;apply color mapping
    LEGS, DATA=proj_acc_pan, MAPPING=mapping_accum,  IMG_OUT=rgb_acc_pan
    LEGS, DATA=proj_p0_pan,  MAPPING=mapping_p0tend, IMG_OUT=rgb_p0_pan
    ;mix the two images
    LINMAP, proj_acc_pan, alpha, [mm1,mm2],[0.,1.]
    ;alpha=FLTARR(SIZE(proj_acc_pan,/DIM))
    ;aa = WHERE(proj_acc_pan GE min_acc, caa)
    ;IF caa NE 0 THEN alpha[aa] = 1.
    IMG_MIX, rgb_blended_pan, rgb_p0_pan, rgb_acc_pan, alpha
    
    ;replace inner part with panam data
    alpha=FLTARR(SIZE(proj_p0_pan,/DIM))
    aa = WHERE((proj_p0_pan NE missing) OR (proj_acc_pan NE missing), caa)
    IF caa NE 0 THEN alpha[aa] = 1.
    IMG_MIX, rgb_blended_2dom, rgb_blended_nat, rgb_blended_pan, alpha
    
    ;finally plot image
    pos = [x0,y0,x0+rec_w,y0+rec_h]
    ;image
    LOADCT,0,/S
    TV, rgb_blended_2dom, pos[0], pos[1], XS=pos[2]-pos[0], YS=pos[3]-pos[1], /NORMAL, TRUE=3
    ;titles
    XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'color: 5 min precip accum',/NORMAL,CHARSIZE=1.7, COL=0
    XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.85*(pos[3]-pos[1]),'b&w:   P0 tendencies',/NORMAL,CHARSIZE=1.7, COL=0
    ;overlay grid
    MAPS, POS=pos, /GRID, /MAP, LOC=loc
    PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon
    ;PLOT_BORDER, MAT_LAT=exp_str[jj].lat, MAT_LON=exp_str[jj].lon
    
    ;;plot location of cross section
    LOADCT,40,/S
    IF N_ELEMENTS(lat_arr) NE 0 THEN PLOTS, lon_arr, lat_arr, TH=5., COL=210

    ;palettes
    nexp = N_ELEMENTS(exp_str)
    IF jj EQ nexp-1 THEN BEGIN
        x0 = x0 + rec_w + pal_sp
        pos = [x0,y0,x0+pal_w,y0+rec_h]
        LEGS, MAPPING=mapping_accum, PALETTE=pos, PAL_PROP='equal', UNITS='mm in 5 min'
        x0 = x0 + pal_w+2.*pal_sp
        pos = [x0,y0,x0+pal_w,y0+rec_h]
        ;LEGS, MAPPING=mapping_p0tend, PALETTE=pos, UNITS='Pa/min'
    ENDIF
END



PRO IMG_WRAP, pic_dir, ii, str_savename, loc, diff

    RESTORE, str_savename

    ;load rmn lib
    LOAD_RMN_LIB
    SPAWN,'. ssmuse-sh -d cmdn/201502/06/base'


    ;set up image coords setup 
    nexp = N_ELEMENTS(exp_str)
    sq_w = 10.
    sq_h = 10.
    IF loc EQ 'nat_enkf' THEN sq_h = .8*sq_h
    ;pic_h = 42.
    pic_h = 12.     ;only preci + p0 tend
    ;pic_h = 23.     ;only preci + p0 tend + 1 vertcal cross section
    pic_w =4. + 15.0*(nexp + diff)
    pal_sp = 1.3/pic_w
    pal_w = .25/pic_w
    rec_w = sq_w/pic_w
    rec_h = sq_h/pic_h
    sp_w = 2./pic_w
    sp_h = 2./pic_h
    x1 = 2.2/pic_w 
    y1 = 1./pic_h
    y2 = y1 + rec_h + sp_h
    y3 = y2 + .7*rec_h + sp_h
    y4 = y3 + .7*rec_h + sp_h


    pic_name = pic_dir+'sample_'+nat_str[0].img_time_str[ii]+'.ps'
    PS_START, pic_name, pic_w, pic_h
    !P.CHARSIZE=1.15
    ;white square to avoid problems in animations
    LOADCT, 0, /S
    TV, [255], 0, 0, XS=1, YS=1, /NORMAL
    LOADCT, 40, /S

    ;FOR jj=0, nexp-1 DO BEGIN
    ;    ;image position
    ;    x0= x1 + jj*(rec_w+sp_w)
    ;    y0= y1

    ;    ;time
    ;    IF jj EQ 0 THEN XYOUTS, x0+.001*rec_w,1.-1./pic_h,nat_str[0].img_time_str[ii],/NORMAL,CHARSIZE=2.
    ;    ;plot title
    ;    XYOUTS, x0+.02*rec_w,1.-2./pic_h,exp_str[jj].desc,/NORMAL,CHARSIZE=1.7

    ANIM_PANDOM_IMG, ii, jj, nexp, nat_str, exp_str,                                            $
                     pic_dir, mapping_qd, mapping_accum, mapping_p0tend, mapping_w, mapping_t,  $
                     mapping_mpnc, mapping_mpqc, mapping_hu,mapping_iir,mapping_icr,            $
                     x1, y1, rec_w, rec_h, sp_w, sp_h, pal_w, pal_sp, pic_w, pic_h, loc, diff
    ;ENDFOR


    PS_CLOSE, pic_name,  /DEL_PS, DENSITY=400, GEOMETRY='20%', FONT='lmroman', /V, /JPG
    ;PS_CLOSE, pic_name, /DEL_PS, FONT='lmroman', /V, /PDF
    print, ''
    print, ''
    print, ''
    print, ''
    print, ''
    print, ''
    print, ''
END


PRO LHN_IMG, ii, jj, nexp, nat_str, exp_str,                                           $
                     pic_dir, mapping_qd, mapping_accum, mapping_p0tend, mapping_w, mapping_t, $
                     mapping_mpnc, mapping_mpqc, mapping_hu, mapping_iir, mapping_icr,         $
                     xbase, ybase, rec_w, rec_h, sp_w, sp_h, pal_w, pal_sp, pic_w, pic_h, loc, diff


    missing = -9999.
    ;min_acc = .12
    min_acc = .05
    fact = .7

    ;print title
    XYOUTS, xbase+.001*rec_w,1.-1./pic_h,nat_str[0].img_time_str[ii],/NORMAL,CHARSIZE=2.

    ;number of experiments to plot
    nexp = N_ELEMENTS(exp_str)
    ;plot title of each experiment
    FOR jj=0, nexp-1 DO XYOUTS, xbase+jj*(rec_w + sp_w),1.-2./pic_h,exp_str[jj].desc,/NORMAL,CHARSIZE=1.7
    IF diff EQ 1 THEN   XYOUTS, xbase+jj*(rec_w + 3.2*sp_w),1.-2./pic_h,'Difference',/NORMAL,CHARSIZE=1.7



    ;----------------------------
    ;P0 tend + acumulations
    FOR jj=0, nexp-1 DO BEGIN

        ;panam
        ;calculate dp0/dt
        IF (exp_str[jj].file_t[ii] EQ 'not_avail') OR (exp_str[jj].file_mdt[ii] EQ 'not_avail') THEN BEGIN
            pan_dp_dt = REPLICATE(missing, exp_str[jj].nx, exp_str[jj].ny)
        ENDIF ELSE BEGIN
            GET_GEM_DATA, exp_str[jj].file_t[ii],   P0=p0,                     CMC_TIMESTAMP=exp_str[jj].time_t[ii]
            GET_GEM_DATA, exp_str[jj].file_mdt[ii], P0=p0_mdt,                 CMC_TIMESTAMP=exp_str[jj].time_mdt[ii]
            pan_dp_dt = ((p0 - p0_mdt)/exp_str[jj].model_dt)
        ENDELSE
        ;record difference if needed
        IF diff EQ 1 THEN BEGIN
            CASE jj OF
                0: BEGIN
                       pan_dp_dt_ref = pan_dp_dt
                   END
                1: BEGIN
                       pan_dp_dt_new = pan_dp_dt
                   END
                ELSE: MESSAGE, 'No more than 2 experiences with the diff keyword'
            ENDCASE
        ENDIF
        ;calculate accumuated PR
        IF (exp_str[jj].file_pr[ii] EQ 'not_avail') OR (exp_str[jj].file_pr_mdt[ii] EQ 'not_avail') THEN BEGIN
           pan_acc = REPLICATE(missing, exp_str[jj].nx, exp_str[jj].ny)
        ENDIF ELSE BEGIN
            GET_GEM_DATA, exp_str[jj].file_pr[ii], PR=pr, CMC_TIMESTAMP=exp_str[jj].time_pr[ii]
        
            ;five minute after the hour, PR need not be substracted
            cmd='r.date -V '+ STRING(exp_str[jj].time_pr_mdt[ii], FORMAT='(i12)')
            SPAWN, cmd, time
            minute = STRMID(time, 10,2)
            read_pr = 1
            IF (minute EQ '00') THEN BEGIN    ;continuous forecasts
                CASE jj OF
                    0 : print, 'pr as usual '
                    ;1 : print, 'pr as usual '
                    ELSE: BEGIN
                              print, 'use 0 for pr_mdt'
                              pr_mdt = REPLICATE(0.,SIZE(pr,/DIM))
                              read_pr = 0
                          END
                ENDCASE
            ENDIF 
            IF read_pr EQ 1 THEN BEGIN
                GET_GEM_DATA, exp_str[jj].file_pr_mdt[ii], PR=pr_mdt, CMC_TIMESTAMP=exp_str[jj].time_pr_mdt[ii]
            ENDIF
        
            dpr = (pr - pr_mdt)
            pan_acc = REPLICATE(missing, exp_str[jj].nx, exp_str[jj].nx)
            pr_sz = SIZE(pr_mdt, /DIM)
            pan_clip = (exp_str[jj].nx - pr_sz[0])/2.
            pan_acc[pan_clip:exp_str[jj].nx-pan_clip-1,pan_clip:exp_str[jj].nx-pan_clip-1] = (pr - pr_mdt)      ;accumulated precip for dt minutes
        ENDELSE
        ;record difference if needed
        IF diff EQ 1 THEN BEGIN
            CASE jj OF
                0: BEGIN
                       pan_acc_ref = pan_acc
                   END
                1: BEGIN
                       pan_acc_new = pan_acc
                   END
                ELSE: MESSAGE, 'No more than 2 experiences with the diff keyword'
            ENDCASE
        ENDIF

        ;nat_domain
        ;calculate dp0/dt
        IF (nat_str[jj].file_t[ii] EQ 'not_avail') OR (nat_str[jj].file_mdt[ii] EQ 'not_avail') THEN BEGIN
            nat_dp_dt = REPLICATE(missing, nat_str[jj].nx, nat_str[jj].ny)
        ENDIF ELSE BEGIN
            GET_GEM_DATA, nat_str[jj].file_t[ii],   P0=p0,     CMC_TIMESTAMP=nat_str[jj].time_pr[ii]
            GET_GEM_DATA, nat_str[jj].file_mdt[ii], P0=p0_mdt, CMC_TIMESTAMP=nat_str[jj].time_pr_mdt[ii]
            nat_dp_dt = ((p0 - p0_mdt)/nat_str[jj].model_dt)
        ENDELSE
        ;calculate accumuated PR
        IF (nat_str[jj].file_pr[ii] EQ 'not_avail') OR (nat_str[jj].file_pr_mdt[ii] EQ 'not_avail') THEN BEGIN
            nat_acc = REPLICATE(missing, nat_str[jj].nx, nat_str[jj].ny)
        ENDIF ELSE BEGIN
            GET_GEM_DATA, nat_str[jj].file_pr[ii],     PR=pr,     CMC_TIMESTAMP=nat_str[jj].time_pr[ii]
            GET_GEM_DATA, nat_str[jj].file_pr_mdt[ii], PR=pr_mdt, CMC_TIMESTAMP=nat_str[jj].time_pr_mdt[ii]
            nat_acc = REPLICATE(missing, nat_str[jj].nx, nat_str[jj].ny)
            pr_sz = SIZE(pr_mdt, /DIM)
            nat_clip = (nat_str[jj].nx - pr_sz[0])/2.
            IF nat_clip LT 0 THEN BEGIN
                MESSAGE, 'nat clip negative, something is going wrong'
            ENDIF ELSE BEGIN
                nat_acc[nat_clip:nat_str[jj].nx-nat_clip-1,nat_clip:nat_str[jj].ny-nat_clip-1] = (pr - pr_mdt)      ;accumulated precip for dt minutes
            ENDELSE
        ENDELSE

        ;save scores
        COMP_SCORE, missing, 'mm in 5 min',VAR1=pan_acc, EXP_NAME_1=exp_str[jj].code, SCORE_NAME='tot', VAR_NAME='PR', $
                    DATE_STR=exp_str[jj].time_str[ii], DATE_CMC=exp_str[jj].time_t[ii]
        COMP_SCORE, missing, 'Pa/min', VAR1=ABS(pan_dp_dt), EXP_NAME_1=exp_str[jj].code, SCORE_NAME='tot', VAR_NAME='P0tend', $
                    DATE_STR=exp_str[jj].time_str[ii], DATE_CMC=exp_str[jj].time_t[ii]

        ;plot image
        y0 = ybase + 0.*(rec_h + sp_h)
        x0 = xbase + jj*(rec_w + sp_w)
        PLOT_DP_DT, ii, jj, exp_str, nat_str,missing,                         $ ;base info
                    x0, y0, rec_w, rec_h, sp_w, sp_h, pal_sp, pal_w,loc,      $ ;image position info
                    pan_dp_dt, pan_acc, nat_dp_dt, nat_acc, lat_arr, lon_arr    ;data input

    ENDFOR

END




PRO LHN_VERIF, FROM_SAV=from_sav, NOERASE=noerase, SERIAL=serial
;generates images of precip and graphs of various verification scores for precipitation




pic_dir = '~/documents/ps/anim_lhn_verif/'
;make dir if ti does not exist
IF ~FILE_TEST(pic_dir) THEN FILE_MKDIR, pic_dir
;wipe out picture dir
IF ~KEYWORD_SET(noerase) THEN SPAWN, 'rm -f '+pic_dir+'/*'

missing = -9999.

;domain
loc = 'nat_enkf'
;time
t0 = JULDAY(07,08,2014,12,00,0)
tf = JULDAY(07,09,2014,00,00,0)
;experiments
nat_list = ['/local/drive2/arma/armadja/data/lhn_test_outputs/aaa']
nat_desc = ['model']
nat_dt   = 5D
pr_dt    = 5D       ;accumulation time for precip
score_dt = 5D       ;temporal resolution scores to be computed in minutes
img_dt   = 60D      ;temporal resolution of figures to be generated in minutes
num_cpus = 12

nat_ind = '~/documents/idl_sav_files/lhn_verif.sav'
str_savename = '~/documents/idl_sav_files/lhn_verif_str.sav'

;get lat/lon for national domain
nat_files = FILE_SEARCH(nat_list[0]+'/*', COUNT=n_nat)
IF n_nat EQ 0 THEN MESSAGE, 'No files found '
GET_GEM_DATA, nat_files[1], LAT=nat_lat, LON=nat_lon, GETVAR='RT'
;determine nx and ny from latitude matrix
sz = SIZE(nat_lat, /DIM)
nat_nx = sz[0]
nat_ny = sz[1]
print, 'Nat domain ','nx ', nat_nx, ' ny ', nat_ny
    ;;Z buffer stuff to set projection indices
    ;SET_PLOT, 'Z'
    ;; projection image size in pixels
    ;sz_zbuf = [800,800]     ;data square smaller than this because of boundaries
    ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
    ;DEVICE, DECOMPOSED=0
    ;;map projection
    ;sub_domain = [0,0.,1.,1.]
    ;    MAPS, POS=sub_domain, LATS=nat_lat, LONS=nat_lon, MISSING=missing, PROJ_IND=proj_ind_nat, LOC=loc
    ;        ;mis_proj_up
    ;        arr_lat=nat_lat[*,nat_ny-1]
    ;        arr_lon=nat_lon[*,nat_ny-1]
    ;        nn=5
    ;        mis_lon=REBIN(arr_lon,nat_nx,nn)
    ;        mis_lat=FLTARR(nat_nx,nn)
    ;        FOR ii=0D,nn-1 DO mis_lat[*,ii] = arr_lat + .5*ii
    ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_up, LOC=loc
    ;        missing_up = WHERE(proj_mis_up NE missing, nmis)
    ;        IF nmis NE 0 THEN proj_ind_nat[missing_up]=missing
    ;        ;mis_proj_down
    ;        arr_lat=nat_lat[*,0]
    ;        arr_lon=nat_lon[*,0]
    ;        nn=5
    ;        mis_lon=REBIN(arr_lon,nat_nx,nn)
    ;        mis_lat=FLTARR(nat_nx,nn)
    ;        FOR ii=nn-1,0,-1 DO mis_lat[*,ii] = arr_lat - .5*ii
    ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_do, LOC=loc
    ;        missing_do = WHERE(proj_mis_do NE missing, nmis)
    ;        IF nmis NE 0 THEN proj_ind_nat[missing_do]=missing
    ;    ;save projection indices
    ;    help, proj_ind_nat
    ;    SAVE, proj_ind_nat, FILENAME=nat_ind, /COMPRESS
    ;DEVICE, Z_BUFFERING = 0
RESTORE, nat_ind;, /VERBOSE


IF ~KEYWORD_SET(from_sav) THEN BEGIN

    ;nat domain
    FILE_LIST, t0, tf, nat_dt, pr_dt, img_dt,                                   $  ;input related to time
               nat_list, nat_desc, nat_lat, nat_lon, proj_ind_nat, /NEAREST,    $  ;input related to files list & projection
               nat_str                                                             ;output
    SAVE, nat_str, FILENAME=str_savename
ENDIF ELSE BEGIN
    RESTORE, str_savename
ENDELSE

;;debug
;FOR i=0, count-1 DO BEGIN
;    natnum = 0
;    cmd='r.date -V '+ STRING(nat_str[natnum].time_t[i], FORMAT='(i12)')
;    SPAWN, cmd, time
;    print, time
;    print, nat_str[natnum].file_t[i]
;
;    cmd='r.date -V '+ STRING(nat_str[natnum].time_mdt[i], FORMAT='(i12)')
;    SPAWN, cmd, time
;    print, time
;    print, nat_str[natnum].file_mdt[i]
;    print, ''
;ENDFOR
;print, 's'
;stop




;make the images
    n_task = exp_str[0].nt

IF KEYWORD_SET(serial) THEN BEGIN
    ;serially
    FOR i=0, n_task-1 DO begin
    ;FOR i=0, 62 DO begin
        IMG_WRAP,pic_dir,i,str_savename,loc,diff
    ENDFOR
    stop
ENDIF ELSE BEGIN
    ;in parallel
    print, 'Computing in parallel...'
    n_cpu=num_cpus
    ; Initialize and get the process manager
    cpu_pm=Get_CPU_Process_Manager(n_cpu)
    ; Setup by setting to all slaves the same working directory, IDL path and compiling the same routines as the current IDL session     
    cpu_pm->Setup
    ; Send one unit of work to each session
    FOR i=0, MIN([n_cpu-1,n_task-1]) DO task_id=cpu_pm->Call_Procedure('IMG_WRAP',pic_dir,i,str_savename,loc,diff)
    ; If necessary, loop over getting new work requests until there is no more work to be done
    IF n_task - n_cpu GE 1 THEN BEGIN
        FOR i=n_cpu, n_task-1 DO BEGIN
          ; Wait until one of the processes finish
          task_id=cpu_pm->WaitEnd()
          ; Receive result and save it
          dummy=cpu_pm->GetResult(task_id)
          ; Send a new work unit
          task_id=cpu_pm->Call_Procedure('IMG_WRAP',pic_dir,i,str_savename,loc,diff)
        ENDFOR
    ENDIF
    ; There is no more work to be done, so receive all the outstanding results from the slaves
    FOR i=0, n_cpu-1 DO BEGIN
      task_id=cpu_pm->WaitEnd()
      dummy=cpu_pm->GetResult(task_id)
    ENDFOR
    print, 'Done'
ENDELSE



;mencoder "mf://*.jpg" -mf fps=12 -o ~/public_html/movies/test.avi -ovc lavc -lavcopts vcodec=msmpeg4v2:vbitrate=7000

;print, 'creating movie: ~/public_html/ps/anim_2dom.gif'
;SPAWN, 'convert -delay 15   -loop 0   '+pic_dir+'sample*.gif ~/public_html/movies/anim_2dom.gif'
;print, 'done'


END
