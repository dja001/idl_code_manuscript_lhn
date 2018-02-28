pro img_wrap, pic_dir, ii, str_savename, diff

    restore, str_savename

    ;load rmn lib
    load_rmn_lib
    spawn,'. ssmuse-sh -d cmdn/201502/06/base'

    ;set up image coords setup 
    ps = {pic_w:0.,pic_h:0.,sq_w:0.,sq_h:0.,pal_sp:0.,pal_w:0.,rec_w:0.,rec_h:0.,sp_w:0.,sp_h:0.,$
             x1:0.,   x2:0.,  x3:0.,  x4:0.,    x5:0.,   x6:0.,   x7:0.,   x8:0.,  x9:0.,        $
             y1:0.,   y2:0.,  y3:0.,  y4:0.,    y5:0.,   y6:0.,   y7:0.,   y8:0.,  y9:0.}
    ps.sq_w = 9.
    ps.sq_h = .8*ps.sq_w
    if exp_struc.loc eq 'nat_enkf' then ps.sq_h = .8*ps.sq_h
    if exp_struc.plot_score eq 1 then begin
        ps.pic_h = 60.
    endif else begin
        ;ps.pic_h = 38.
        ps.pic_h = 22.
            ;ps.pic_h = 13.  ;one img
    endelse
    ;ps.pic_h = 12.     ;only preci + p0 tend
    ;ps.pic_h = 26.     ;only preci + p0 tend + 2d pic
    ;ps.pic_h = 37.     ;only preci + p0 tend + 2 * 2d pic
    ;ps.pic_h = 54.
    ;ps.pic_h = 23.     ;only preci + p0 tend + 1 vertcal cross section
    ps.pic_w  = 2. + 17.0*(exp_struc.nexp + diff)
    ;if ps.pic_w lt 40. then ps.pic_w = 40.
    ps.pal_sp = 1.3/ps.pic_w
    ps.pal_w  = .25/ps.pic_w
    ps.rec_w  = ps.sq_w/ps.pic_w
    ps.rec_h  = ps.sq_h/ps.pic_h
    ps.sp_w   = 2./ps.pic_w
    ps.sp_h   = 2./ps.pic_h
    ps.x1     = 2.5/ps.pic_w 
    ps.y1     = 2./ps.pic_h
    ps.y2     = ps.y1 + ps.rec_h + ps.sp_h
    ps.y3     = ps.y2 + .7*ps.rec_h + ps.sp_h
    ps.y4     = ps.y3 + .7*ps.rec_h + ps.sp_h


    pic_name = pic_dir+'sample_'+exp_struc.img_time_str[ii,0,0]+'.ps'
    ps_start, pic_name, ps.pic_w, ps.pic_h, /white, charsize=1.0


    img_content, ii, exp_struc, ps, diff

    if exp_struc.no_pic eq 1 then begin
        ;no conversion for rapid execution
        ps_close, pic_name,  /del_ps, /v
    endif else begin
        ;preferred option for avi movies
        ;ps_close, pic_name,  /del_ps, density=1200, geometry='50%', font='lmroman', /v, /jpg, num_cpus=9
        ps_close, pic_name,  /del_ps, density=400, geometry='50%', font='lmroman', /v, /jpg, num_cpu=10
        ;ps_close, pic_name, /del_ps, font='lmroman', /v, /pdf
    endelse

    print, ''
    print, ''
    print, ''
    print, ''
    print, ''
    print, ''
    print, ''
end


PRO IMG_CONTENT, ii, exp_struc, ps, diff

    ;printing stuff on the image
    missing = -9999.
    fact = .7

    ;plot title of each experiment
        ;dark                    pale
    r_arr = [[000, 000, 000, 153, 204],$      ;blue
             [164, 255, 255, 255, 255],$      ;orange
             [000, 000, 000, 000, 000],$      ;green
             [000, 220, 000, 000, 000]]       ;pink
    g_arr = [[081, 126, 169, 216, 249],$
             [053, 119, 159, 205, 255],$
             [000, 134, 000, 000, 000],$   
             [000, 000, 000, 000, 000]]    
    b_arr = [[192, 237, 191, 224, 255],$
             [000, 051, 071, 124, 169],$
             [000, 000, 000, 000, 000],$
             [000, 255, 000, 000, 000]]     
    for nn=0, exp_struc.nexp-1 do begin
        ;;color rectangle for title
        ;y0 = 1.-3./ps.pic_h
        ;x0 = ps.x1 + nn*(ps.rec_w + ps.sp_w)
        ;pos = [x0,y0,x0+ps.rec_w,y0+.45*ps.rec_h]
        ;tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
        ;tv,[1], x0, y0, xs=ps.rec_w, ys=1.7/ps.pic_h, /normal
        
        if nn eq 0 then begin
            ;title text
            time_str = exp_struc.time_str[ii,nn,0]
            yy = strmid(time_str, 0,4)
            mo = strmid(time_str, 5,2)
            dd = strmid(time_str, 8,2)
            hh = strmid(time_str,12,2)
            mi = strmid(time_str,15,2)
            date_str = yy+'-'+mo+'-'+dd+' '+hh+':'+mi+' UTC'
            loadct, 40, /s
            xyouts, ps.x1+nn*(ps.rec_w + ps.sp_w),1.-2.7/ps.pic_h,date_str,/normal,charsize=1.4, col=255
        endif
    endfor
    if diff eq 1 then xyouts, ps.x1+nn*(ps.rec_w + 3.2*ps.sp_w),1.-2.7/ps.pic_h,'difference',/normal,charsize=1.7

    ;;----------------------------
    ;;qty of moisture injected
    ;FOR jj=0, exp_struc.nexp-1 DO BEGIN

    ;    ;panam
    ;    ;get data
    ;    GET_GEM_DATA, exp_str[jj].file_t[ii],   VAR_NAME='TLHS',VALUES=exp_tlhs, CMC_TIMESTAMP=exp_str[jj].time_t[ii], IS_THERE=is_there
    ;    IF is_there EQ 0 THEN exp_tlhs = REPLICATE(missing, exp_str[jj].nx, exp_str[jj].ny)
    ;    ON_MODEL_GRID, exp_tlhs, exp_str[jj].nx, exp_str[jj].ny, missing

    ;    ;nat_domain
    ;    ;get_data
    ;    GET_GEM_DATA, nat_str[jj].file_t[ii],   VAR_NAME='TLHS',VALUES=nat_rdqi, CMC_TIMESTAMP=nat_str[jj].time_t[ii], IS_THERE=is_there
    ;    IF is_there EQ 0 THEN nat_rdqi = REPLICATE(missing, exp_str[jj].nx, exp_str[jj].ny)
    ;    ON_MODEL_GRID, nat_rdqi, nat_str[jj].nx, nat_str[jj].ny, missing


    ;    ;plot image
    ;    y0 = ps.y1 + 2.*(ps.rec_h + ps.sp_h)
    ;    x0 = ps.x1 + jj*(ps.rec_w + ps.sp_w)
    ;    PLOT_TLHS, ii, jj, exp_str, nat_str,missing,                         $ ;base info
    ;               ps,loc,      $ ;image position info
    ;               exp_tlhs, nat_rdqi, lat_arr, lon_arr                        ;data input

    ;ENDFOR

    ;;----------------------------
    ;;LHN decision tree
    ;FOR jj=0, exp_struc.nexp-1 DO BEGIN

    ;    ;panam
    ;    ;get data
    ;    GET_GEM_DATA, exp_str[jj].file_t[ii],   VAR_NAME='KF',VALUES=exp_rdqi, CMC_TIMESTAMP=exp_str[jj].time_t[ii], IS_THERE=is_there
    ;    IF is_there EQ 0 THEN exp_rdqi = REPLICATE(missing, exp_str[jj].nx, exp_str[jj].ny)
    ;    ON_MODEL_GRID, exp_rdqi, exp_str[jj].nx, exp_str[jj].ny, missing
    ;    exp_rdqi *= 3e5 ;m/s to mm/5min

    ;    ;nat_domain
    ;    ;get_data
    ;    GET_GEM_DATA, nat_str[jj].file_t[ii],   VAR_NAME='KF',VALUES=nat_rdqi, CMC_TIMESTAMP=nat_str[jj].time_t[ii], IS_THERE=is_there
    ;    IF is_there EQ 0 THEN nat_rdqi = REPLICATE(missing, exp_str[jj].nx, exp_str[jj].ny)
    ;    ON_MODEL_GRID, nat_rdqi, nat_str[jj].nx, nat_str[jj].ny, missing
    ;    nat_rdqi *= 3e5 ;m/s to mm/5min


    ;    ;plot image
    ;    y0 = ps.y1 + 1.*(ps.rec_h + ps.sp_h)
    ;    x0 = ps.x1 + jj*(ps.rec_w + ps.sp_w)
    ;    PLOT_KF, ii, jj, exp_str, nat_str,missing,                         $ ;base info
    ;             ps,loc,      $ ;image position info
    ;             exp_rdqi, nat_rdqi, lat_arr, lon_arr                        ;data input

    ;ENDFOR

    ;;;----------------------------
    ;;rain rates from nowcast forecast
    ;for nn=0, exp_struc.nexp-1 do begin
    ;endfor
    
    
    ;;----------------------------
    ;plot scores if desired
    tresh_ind=0
    if exp_struc.plot_score eq 1 then begin
        plot_corr_bp, ii, nn, exp_struc, missing,$ ;base info
               2.05, ps                            ;image position info
        plot_spec,    ii, nn, exp_struc, missing,$ ;base info
               2.05, ps                            ;image position info
        plot_hist,    ii, nn, exp_struc, missing,$ ;base info
               2.05, ps                            ;image position info
        
        
        plot_score, ii, nn, exp_struc, missing,$
                    2.00, ps,                  $
                   'POD', [0.,.75],             exp_struc.pod,           tresh_ind=tresh_ind, yticks=3
        plot_score, ii, nn, exp_struc, missing,$
                    1.75, ps,                  $
                   'FAR', [0.7,1.0],            exp_struc.far,           tresh_ind=tresh_ind, yticks=3
        plot_score, ii, nn, exp_struc, missing,$
                    1.50, ps,                  $
                   'CSI', [0.,.45],             exp_struc.csi,           tresh_ind=tresh_ind
        plot_score, ii, nn, exp_struc, missing,$
                    1.25, ps,                  $
                   'GSS', [0.,.45],             exp_struc.gss,           tresh_ind=tresh_ind
        plot_score, ii, nn, exp_struc, missing,$
                    1.00, ps,                  $
                   'Lmin',  [0.,225],           exp_struc.lmin,          tresh_ind=tresh_ind
        plot_score, ii, nn, exp_struc, missing,$ 
                    0.75, ps,                  $
                    'Correlation', [-.3,.7],     exp_struc.tot_corr,      tresh_ind=tresh_ind, yticks=2
        plot_score, ii, nn, exp_struc, missing,$
                    0.50, ps,                  $
                   'freq. Bias',  [-.1,.1],     exp_struc.fbias,         tresh_ind=tresh_ind
        plot_score, ii, nn, exp_struc, missing,$
                    0.25, ps,                  $
                   'Areal coverage',  [0.,1.], exp_struc.areal_cov,      tresh_ind=tresh_ind, rad_cov=exp_struc.rad_areal_cov
        plot_score, ii, nn, exp_struc, missing,$
                    0.00, ps,                  $
                   'avg(abs(dp/dt))', [0.003,.008], exp_struc.dpdt,      tresh_ind=missing,   yticks=3

        voffset = 2.55
    endif else begin
        voffset = 0.
    endelse

    ;;----------------------------
    ;rain rates from reflectivity
    for nn=0, exp_struc.nexp-1 do begin

        ;only one radar fig
        ;if nn ne exp_struc.nexp-1 then continue

        extension='_12_avg_09_min'
        ;extension='_08_avg_05_min'
        ;extension='_02_avg_01_min'
        data_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7'+extension
        radar_dir_name='radar'+extension
        rdt = 10    ;temporal res of data in minutes
        plot_rdbr, ii, nn, exp_struc, missing, data_dir, rdt,  $ ;base info
                   1.+voffset, ps,                             $ ;image position info
                   radar_mmh, radar_qi                           ;output data
    endfor
    
    ;;;----------------------------
    ;;decision tree
    ;for nn=0, exp_struc.nexp-1 do begin
    ;    plot_tree, ii, nn, exp_struc, missing, $ ;base info
    ;               0., ps                        ;image position info
    ;endfor
    
    ;----------------------------
    ;plot forecast precip and p0 tend
    ;P0 tend + acumulations for gem forecasts, only accum for nowcast foracast
    for nn=0, exp_struc.nexp-1 do begin
        if exp_struc.code[nn,0] eq 'now' then begin
            ;for nowcast forecasts
            data_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/nowcasts/'
            rdt = 5 ;temporal res of data in minutes
            plot_rdbr, ii, nn, exp_struc, missing, data_dir, rdt,  $ ;base info
                       0.+voffset, ps,/nowcast,                    $ ;image info
                       pr_acc                                   ;output data
            dp_dt = replicate(0., size(pr_acc,/dim))
            ;Confusingly, valid nowcasts may contain both -999. and -9999.
            ;for verification purposes, those are set to 0.
            aa = where(pr_acc lt 0., naa)
            if naa ne 0 then pr_acc[aa] = 0.
            ;if there is no valid precip above zero, then fill matrix with missing value
            ;so that scores are not computed. 
            if naa eq n_elements(pr_acc) then pr_acc = 0.*pr_acc+missing
        endif else begin
            ;gem forecasts
            plot_dp_dt, ii, nn, exp_struc, missing, $ ;base info
                        0.+voffset, ps,             $ ;image position info
                        cc_lat_arr, cc_lon_arr,     $ ;cross section position
                        dp_dt, pr_acc                 ;data output
        endelse
    
        ;----------------------------
        ;;images showing fractions for the computation of the fraction skill score
        ;plot_frac, ii, nn, exp_struc, missing,$ ;base info
        ;           4., ps,                    $ ;image position info
        ;           tresh_ind, pr_acc[0:exp_struc.nx[0]-1,0:exp_struc.ny[0]-1,0]  ;input data
        ;plot_frac, ii, nn, exp_struc, missing,$ ;base info
        ;           5., ps,                    $ ;image position info
        ;           tresh_ind, radar_mmh  ;input data
    
        if exp_struc.ndom gt 1 then message, 'not compatible with more than one domain'
        dd=0


        ;computation of scores if desired
        if exp_struc.comp_score eq 1 then begin
            comp_score, missing, exp_struc, nn,                                                   $
                        fcst_field=pr_acc[0:exp_struc.nx[0]-1,0:exp_struc.ny[0]-1,dd],            $
                        dpdt_field= dp_dt[0:exp_struc.nx[0]-1,0:exp_struc.ny[0]-1,dd],            $
                        refer_field=radar_mmh, radar_qi=radar_qi,                                 $
                        fcst_name =exp_struc.code[nn,dd],                                         $
                        date_str=exp_struc.time_str[ii,0,dd], date_cmc=exp_struc.time_t[ii,0,dd], $
                        radar_dir_name=radar_dir_name
        endif

    endfor
        

END




pro     anim_pandom, ctl=ctl, from_sav=from_sav, noerase=noerase, serial=serial, proj_gen=proj_gen
;generates an animation of model precip output, radar images on panam domain




pic_dir = '~/documents/ps/anim_pandom/'
;pic_dir = '~/documents/ps/lhn_results/'
;make dir if ti does not exist
if ~file_test(pic_dir) then file_mkdir, pic_dir
;wipe out picture dir
if ~keyword_set(noerase) then spawn, 'rm -f '+pic_dir+'/*'

missing = -9999.



;;domain
;loc = 'mid_us'
;;time
;t0 = JULDAY(05,25,2015,08,00,0)
;tf = JULDAY(05,25,2015,20,00,0)
;;experiments
;nat_list = ['/local/raid/armadja/data/recycle_outputs/nad',$
;            '/local/raid/armadja/data/recycle_outputs/nag']
;exp_list = ['/local/raid/armadja/data/recycle_outputs/pce',$
;            '/local/raid/armadja/data/recycle_outputs/pcp']
;exp_desc = ['continuous driver ',$
;            'cycled driver ']
;exp_dt   = 1D
;nat_dt   = 5D
;pr_dt    = 5D       ;accumulation time for precip
;img_dt   = 1D       ;temporal resolution of figures to be generated in miuntes
;diff = 1            ;switch to activate diff mode
;num_cpus = 35
;out_list = [[nat_list],[exp_list]]  ;layer 1, layer 2, ...
;out_desc = exp_desc
;out_dt   = [ nat_dt   , exp_dt   ]


;defining control variables
if n_elements(ctl) eq 0 then begin
    ;ctl structure is not present, use the following manual controls
    ;forecast_time
    fcst_time_txt = '2014070200'
    ;domain
    loc = 'can_us_radars'
    ;time
    t0 = JULDAY(07,02,2014,00,00,0)
    tf = JULDAY(07,02,2014,09,00,0)
    ;experiments
    ;nat_list = ['/local/raid/armadja/data/lhn_test_outputs/control_2014070312/',$
    nat_list = ['/local/raid/armadja/data/lhn_pr_p0_outputs/lhn_001/2014070200/', $
                '/local/raid/armadja/data/lhn_pr_p0_outputs/now/dum/']
                ;'/local/raid/armadja/data/lhn_test_outputs/lhn_2014070312_modulation_tresh_dotone/'];,$
               ; '/local/raid/armadja/data/lhn_test_outputs/lhn_2014070312_switch_test_modulation/'];,$
               ; '/local/raid/armadja/data/lhn_test_outputs/lhn_001',$
               ; '/local/raid/armadja/data/lhn_test_outputs/now'];,$
               ; '/local/raid/armadja/data/lhn_test_outputs/aae'];,$
               ; '/local/raid/armadja/data/lhn_test_outputs/aad']
    nat_desc = ['control','now']
    model_res = 10.    ;model resolution in km
    pan_dt   = 1D
    nat_dt   = 5D
    pr_dt    = 5D      ;accumulation time for precip
    img_dt   = 10D     ;temporal resolution of figures to be generated in miuntes
    diff = 0           ;switch to activate diff mode
    comp_score = 0
    plot_score = 0
    no_pic     = 0
    num_cpus = 30
    out_list = [nat_list]
    out_desc = [nat_desc]
    out_dt   = [nat_dt]
endif else begin
    ;a ctl structure was passed, use it to define experiment
    fcst_time_txt = ctl.fcst_time_txt
    loc           = ctl.loc 
    t0            = ctl.t0 
    tf            = ctl.tf 
    nat_list      = ctl.exp_list
    nat_desc      = ctl.exp_desc
    model_res     = ctl.model_res
    pan_dt        = ctl.pan_dt   
    nat_dt        = ctl.nat_dt   
    pr_dt         = ctl.pr_dt    
    img_dt        = ctl.img_dt   
    diff          = ctl.diff
    comp_score    = ctl.comp_score 
    plot_score    = ctl.plot_score 
    no_pic        = ctl.no_pic 
    num_cpus      = ctl.num_cpus 
    out_list      = [nat_list]
    out_desc      = [nat_desc]
    out_dt        = [nat_dt]
endelse

case loc of
    'can_us_radars':proj_file = '~/documents/idl_sav_files/anim_pandom_can_us_radars.sav'
    'mid_us':       proj_file = '~/documents/idl_sav_files/anim_pandom_mid_us.sav'
    'mid_us_closeup':proj_file= '~/documents/idl_sav_files/anim_pandom_mid_us_closeup.sav'
    'pan_enkf':     proj_file = '~/documents/idl_sav_files/anim_pandom_pan_enkf.sav'
    'nat_enkf':     proj_file = '~/documents/idl_sav_files/anim_pandom_nat_enkf.sav'
    'hrenkf_west':  proj_file = '~/documents/idl_sav_files/anim_pandom_hrenkf_west.sav'
    else: message, 'unsuported domain'
endcase


;how many domains to plot, how many experiments ?
sz = DIM8(out_list)
nexp = sz[0]
if sz[1] eq 0 then begin
    nd = 1 
endif else begin 
    nd = sz[1]     
endelse

;directory where scores are kept
score_dir = '/local/raid/armadja/data/lhn_scores_img_movies/'

;scales for wavelet decomposition
nscales=7
scales=2.^(indgen(nscales)+1)*model_res

;thresholds for verification
;base = 10.
;tresh_arr = [base/32.,base/16.,base/8.,base/4.,base/2.,base]
tresh_arr = [.5,1.,5.]
ntresh = n_elements(tresh_arr)

;minimum quality index for considering radar data in verification
min_qi = 0.2

;number of bins for averaged power spectra
k_nbins = 31

;bounds for verification domain
;;verif_domain = [250,50,500,250]    ;reduced domain
;verif_domain = [550,220,855,450]    ;original domain 
;verif_domain = [600,200,900,500]    ;300x300 domain for scale decomp and pow_spectra
verif_domain = [400,200,900,700]    ;500x500 domain for scale decomp and pow_spectra

;number of bins for histograms
h_nbins = 100
hist_min = .1       ;min and max values for histograms
hist_max = 20.













print, 'Number of experiments:', nexp
print, 'Number of domains:', nd
print, 'Number scales for decomposition:', nscales
print, scales
print, 'Number thresholds for precip verification:', ntresh
print, tresh_arr



;latitudes and longitude of device 
sz_zbuf = [800,800]     ;data square smaller than this because of boundaries
;determine latitudes and longitudes on device used for projection
set_plot, 'Z'
; projection image size in pixels
device, set_resolution=sz_zbuf, set_pixel_depth=24
device, decomposed=0
;map projection
sub_domain = [0,0.,1.,1.]
maps, pos=sub_domain, /map, /grid, loc=loc
pnx = sz_zbuf[0]
pny = sz_zbuf[1]
dev_lat_lon,sz_zbuf,dev_lat, dev_lon
device, z_buffering = 0

;get projection indices for the different domains
if keyword_set(proj_gen) then begin
    nx_acc = lonarr(nd)
    ny_acc = lonarr(nd)
    maxnum=1150
    lat_acc = fltarr(maxnum,maxnum,nd)
    lon_acc = fltarr(maxnum,maxnum,nd)
    proj_acc = lonarr(sz_zbuf[0],sz_zbuf[1],nd)
    for dd=0, nd-1 do begin
        print, 'Projection indices for domain: ',dd+1 
        ;get lat/lon for national domain
        files = FILE_SEARCH(out_list[0,dd]+'/*', COUNT=nf)
        IF nf EQ 0 THEN MESSAGE, 'No files found '
        GET_GEM_DATA, files[0], LAT=lat, LON=lon, VAR_NAME='P0'
        ;determine nx and ny from latitude matrix
        sz = SIZE(lat, /DIM)
        nx = sz[0]
        ny = sz[1]

        ;nearest neighbor with kdll
        kdll, lat, lon, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
              proj_ind                                                 ;output

        ;save projected data
        nx_acc[dd] = nx
        ny_acc[dd] = ny
        lat_acc[0:nx-1,0:ny-1,dd] = lat
        lon_acc[0:nx-1,0:ny-1,dd] = lon
        proj_acc[*,*,dd] = proj_ind
        DEVICE, Z_BUFFERING = 0
    endfor
    ;save projection indices
    SAVE, proj_acc, nx_acc, ny_acc, maxnum, lat_acc, lon_acc, FILENAME=proj_file, /COMPRESS
endif else begin
    RESTORE, proj_file;, /VERBOSE
endelse



;generate file structure if needed
str_savename = '~/documents/idl_sav_files/anim_pandom_str.sav'
IF ~KEYWORD_SET(from_sav) THEN BEGIN

    ;get number of images to generate
    TIME_LIST, t0, tf, img_dt, pr_dt, img_dt, $ ;in
               NUM_ONLY=nt                      ;out

    ;initialize data structure 
    exp_struc={nt:0L, ndom:0L, nexp:0L, nscales:0L,maxnum:0L, pr_dt:0L, loc:'',                                                                       $
               plot_score:0b, comp_score:0b, no_pic:0b,                                                                                               $
               fcst_time_txt:'',                                                                                                                      $
               nx:      lonarr(nd),                                                                                                                   $
               ny:      lonarr(nd),                                                                                                                   $
               model_res:lonarr(nd),model_dt:lonarr(nd),                                                                                              $
               proj_ind:lonarr(size(proj_acc,/dim)),                                                                                                  $
               lat:fltarr(maxnum,maxnum,nd),                                                                                                          $
               lon:fltarr(maxnum,maxnum,nd),                                                                                                          $
               desc:    strarr(nexp,nd),   code:        strarr(nexp,nd),                                                                              $
               time_jul:dblarr(nt,nexp,nd),                                                                                                           $
               time_str:strarr(nt,nexp,nd),img_time_str:strarr(nt,nexp,nd),                                                                           $
               time_t:  lonarr(nt,nexp,nd),time_mdt:    lonarr(nt,nexp,nd),                                                                           $
               file_t:  strarr(nt,nexp,nd),file_mdt:    strarr(nt,nexp,nd),                                                                           $
               time_pr: lonarr(nt,nexp,nd),time_pr_mdt: lonarr(nt,nexp,nd),                                                                           $
               file_pr: strarr(nt,nexp,nd),file_pr_mdt: strarr(nt,nexp,nd) ,                                                                          $
               dev_lat:fltarr(sz_zbuf),                                                                                                               $
               dev_lon:fltarr(sz_zbuf),                                                                                                               $
               pod:fltarr(nt,ntresh,nexp),far:fltarr(nt,ntresh,nexp),sr:fltarr(nt,ntresh,nexp),                                                       $
               fbias:fltarr(nt,ntresh,nexp),                                                                                                          $
               areal_cov:fltarr(nt,ntresh,nexp),rad_areal_cov:fltarr(nt,ntresh), dpdt:fltarr(nt,nexp),                                                $
               tot_corr:fltarr(nt,ntresh,nexp), corr_hf:fltarr(nt,nscales,nexp), corr_bf:fltarr(nt,nscales,nexp),                                     $
               corr_lf:fltarr(nt,nscales,nexp),                                                                                                       $
               hist_bin_val:fltarr(h_nbins), hist_bounds:fltarr(h_nbins+1), ps_karr:fltarr(k_nbins),                                                  $
               hist_nlow:fltarr(nt,nexp),     hist_nhigh:fltarr(nt,nexp),    hist_count:fltarr(nt,h_nbins,nexp),     ps:fltarr(nt,k_nbins,nexp),      $
               rad_hist_nlow:fltarr(nt),  rad_hist_nhigh:fltarr(nt),     rad_hist_count:fltarr(nt,h_nbins),      rad_ps:fltarr(nt,k_nbins),           $
               csi:fltarr(nt,ntresh,nexp),gss:fltarr(nt,ntresh,nexp),lmin:fltarr(nt,ntresh,nexp),                                                     $
               scales:fltarr(nscales),                                                                                                                $
               score_dir:'',min_qi:0.,k_nbins:0l,verif_domain:lonarr(4),h_nbins:0l,hist_min:0.,hist_max:0.,                                           $
               ntresh:0,tresh_arr:fltarr(ntresh)}

    ;fill up data structure
    exp_struc.nt        = nt
    exp_struc.ndom      = nd
    exp_struc.nexp      = nexp
    exp_struc.nscales   = nscales
    exp_struc.scales    = scales
    exp_struc.maxnum    = maxnum
    exp_struc.pr_dt     = pr_dt
    exp_struc.loc       = loc
    exp_struc.fcst_time_txt = fcst_time_txt
    exp_struc.ntresh    = ntresh
    exp_struc.tresh_arr = tresh_arr
    exp_struc.dev_lat   = dev_lat
    exp_struc.dev_lon   = dev_lon
    exp_struc.comp_score= comp_score
    exp_struc.plot_score= plot_score
    exp_struc.no_pic    = no_pic
    ;verif quantities
    exp_struc.score_dir    =  score_dir
    exp_struc.min_qi       =  min_qi
    exp_struc.k_nbins      =  k_nbins
    exp_struc.verif_domain =  verif_domain
    exp_struc.h_nbins      =  h_nbins
    exp_struc.hist_min     =  hist_min
    exp_struc.hist_max     =  hist_max    

    for dd=0, nd-1 do begin ;for each domain
        exp_struc.nx[dd]           = nx_acc[dd]
        exp_struc.ny[dd]           = ny_acc[dd]
        exp_struc.model_res[dd]    = model_res[dd]
        exp_struc.model_dt[dd]     = out_dt[dd]
        exp_struc.proj_ind[*,*,dd] = proj_acc[*,*,dd]
        exp_struc.lat[*,*,dd]      = lat_acc[*,*,dd]
        exp_struc.lon[*,*,dd]      = lon_acc[*,*,dd]

        ;generate time list for this domain
        if dd eq 0 then begin
            nearest = 1
            pr_nearest = 0 
        endif else begin
            nearest = 0
            pr_nearest = 1 
        endelse
        time_list, t0, tf, out_dt[dd], pr_dt, img_dt,        $ ;in
                 nearest=nearest, pr_nearest=pr_nearest, $
                 time=time                                 ;out
        ;;debug
        ;print, nearest
        ;print, pr_nearest
        ;print, time.t_str
        ;print, ''
        ;print, time.mdt_str
        ;print, ''

        for nn=0, nexp-1 do begin  ;for each experiment
            ;output of what is going on
            print, 'domain: ',dd,'             experiment number: ',nn 
            print, out_desc[nn]
            print, out_list[nn,dd]
            print, ''

            ;if experiment is same as the previous one, just replicate everything
            if nn ne 0 then begin
                if out_list[nn,dd] eq out_list[nn-1,dd] then begin
                    exp_struc.desc[nn,dd]          = exp_struc.desc[nn-1,dd]          
                    exp_struc.code[nn,dd]          = exp_struc.code[nn-1,dd]          
                    exp_struc.time_str[nn,dd]      = exp_struc.time_str[nn-1,dd]      
                    exp_struc.img_time_str[nn,dd]  = exp_struc.img_time_str[nn-1,dd]  
                    exp_struc.time_t[*,nn,dd]      = exp_struc.time_t[*,nn-1,dd]      
                    exp_struc.file_t[*,nn,dd]      = exp_struc.file_t[*,nn-1,dd]      
                    exp_struc.time_mdt[*,nn,dd]    = exp_struc.time_mdt[*,nn-1,dd]    
                    exp_struc.file_mdt[*,nn,dd]    = exp_struc.file_mdt[*,nn-1,dd]    
                    exp_struc.time_pr[*,nn,dd]     = exp_struc.time_pr[*,nn-1,dd]     
                    exp_struc.file_pr[*,nn,dd]     = exp_struc.file_pr[*,nn-1,dd]     
                    exp_struc.time_pr_mdt[*,nn,dd] = exp_struc.time_pr_mdt[*,nn-1,dd] 
                    exp_struc.file_pr_mdt[*,nn,dd] = exp_struc.file_pr_mdt[*,nn-1,dd] 
                    continue
                endif 
            endif

            ;time for P0 tendency computation
            MATCH_TIME, time.t_cmc,      out_list[nn,dd], 'P0', $
                        file_t,          AVAIL_TIME_ARR=avail_time_arr_p0, AVAIL_FILES=avail_files_p0
            MATCH_TIME, time.mdt_cmc,    out_list[nn,dd], 'P0', $
                        file_mdt,        AVAIL_TIME_ARR=avail_time_arr_p0, AVAIL_FILES=avail_files_p0, REUSE=1
            ;time for PR accum computation
            MATCH_TIME, time.pr_cmc,     out_list[nn,dd], 'PR', $
                        file_pr,         AVAIL_TIME_ARR=avail_time_arr_pr, AVAIL_FILES=avail_files_pr
            MATCH_TIME, time.pr_mdt_cmc,     out_list[nn,dd], 'PR', $
                        file_pr_mdt,     AVAIL_TIME_ARR=avail_time_arr_pr, AVAIL_FILES=avail_files_pr, REUSE=1

            ;fill in values
            exp_struc.desc[nn]             = out_desc[nn]
            exp_struc.code[nn,dd]          = file_basename(file_dirname(out_list[nn,dd]))
            exp_struc.time_str[*,nn,dd]    = time.t_str
            exp_struc.time_jul[*,nn,dd]    = time.t_jul
            exp_struc.img_time_str[*,nn,dd]= time.img_str
            exp_struc.time_t[*,nn,dd]      = time.t_cmc
            exp_struc.file_t[*,nn,dd]      = file_t
            exp_struc.time_mdt[*,nn,dd]    = time.mdt_cmc
            exp_struc.file_mdt[*,nn,dd]    = file_mdt
            exp_struc.time_pr[*,nn,dd]     = time.pr_cmc
            exp_struc.file_pr[*,nn,dd]     = file_pr
            exp_struc.time_pr_mdt[*,nn,dd] = time.pr_mdt_cmc
            exp_struc.file_pr_mdt[*,nn,dd] = file_pr_mdt

            ;get result of previously computed scores
            print, 'finding scores'

            read_score, missing, fcst_time_txt, long(time.t_cmc),    $
                        fcst_name  = exp_struc.code[nn,dd],          $
                        nscales=nscales, ntresh=ntresh, k_nbins=k_nbins, h_nbins=h_nbins,  $
                        fbias      = fbias     , $     
                        pod        = pod       , $
                        far        = far       , $
                        sr         = sr        , $
                        csi        = csi       , $
                        gss        = gss       , $
                        lmin       = lmin      , $
                        tot_corr   = tot_corr  , $
                        corr_hf    = corr_hf   , $
                        corr_bf    = corr_bf   , $
                        corr_lf    = corr_lf   , $
                        hist_nlow  = hist_nlow , $
                        hist_nhigh = hist_nhigh, $
                        hist_count = hist_count, $
                        ps_arr     = ps        , $
                        areal_cov  = areal_cov , $
                        dpdt       = dpdt

                    exp_struc.fbias[*,*,nn] = fbias
                      exp_struc.pod[*,*,nn] = pod
                      exp_struc.far[*,*,nn] = far
                       exp_struc.sr[*,*,nn] = sr
                      exp_struc.csi[*,*,nn] = csi
                      exp_struc.gss[*,*,nn] = gss
                     exp_struc.lmin[*,*,nn] = lmin*model_res
                 exp_struc.tot_corr[*,*,nn] = tot_corr
                  exp_struc.corr_hf[*,*,nn] = corr_hf
                  exp_struc.corr_bf[*,*,nn] = corr_bf
                  exp_struc.corr_lf[*,*,nn] = corr_lf
                exp_struc.hist_nlow[*,nn]   = hist_nlow 
               exp_struc.hist_nhigh[*,nn]   = hist_nhigh
               exp_struc.hist_count[*,*,nn] = hist_count
                       exp_struc.ps[*,*,nn] = ps       
                exp_struc.areal_cov[*,*,nn] = areal_cov       
                     exp_struc.dpdt[*,nn]   = dpdt
        endfor  ;experiments
        ;get result of power spectrum and histograms for radar data
        read_score, missing, fcst_time_txt, long(time.t_cmc),                          $
                    fcst_name  = 'radar_12_avg_09_min',                                $
                    nscales=nscales, ntresh=ntresh, k_nbins=k_nbins, h_nbins=h_nbins,  $
                    hist_bin_val = hist_bin_val ,                                      $
                    hist_bounds  = hist_bounds  ,                                      $
                    hist_nlow  = hist_nlow ,                                           $
                    hist_nhigh = hist_nhigh,                                           $
                    hist_count = hist_count,                                           $
                    ps_karr    = ps_karr   ,                                           $
                    ps_arr     = ps       ,                                            $
                    areal_cov  = areal_cov   
         exp_struc.hist_bin_val       = hist_bin_val
         exp_struc.hist_bounds        = hist_bounds
         exp_struc.rad_hist_nlow      = hist_nlow 
        exp_struc.rad_hist_nhigh      = hist_nhigh
        exp_struc.rad_hist_count      = hist_count
                exp_struc.ps_karr     = ps_karr
                exp_struc.rad_ps      = ps       
             exp_struc.rad_areal_cov  = areal_cov       

    endfor  ;domains
    save, exp_struc, filename=str_savename, /compress
endif else begin
    restore, str_savename
endelse





;make the images
    n_task = exp_struc.nt

if keyword_set(serial) then begin
    ;serially
    for i=4, n_task-1 do begin
        img_wrap,pic_dir,i,str_savename,diff
        stop
    endfor
endif else begin
    ;in parallel
    print, 'computing in parallel...'
    n_cpu=num_cpus
    ; initialize and get the process manager
    cpu_pm=get_cpu_process_manager(n_cpu)
    ; setup by setting to all slaves the same working directory, idl path and compiling the same routines as the current idl session     
    cpu_pm->setup
    ; send one unit of work to each session
    for i=0, min([n_cpu-1,n_task-1]) do task_id=cpu_pm->call_procedure('img_wrap',pic_dir,i,str_savename,diff)
    ; if necessary, loop over getting new work requests until there is no more work to be done
    if n_task - n_cpu ge 1 then begin
        for i=n_cpu, n_task-1 do begin
          ; wait until one of the processes finish
          task_id=cpu_pm->waitend()
          ; receive result and save it
          dummy=cpu_pm->getresult(task_id)
          ; send a new work unit
          task_id=cpu_pm->call_procedure('img_wrap',pic_dir,i,str_savename,diff)
        endfor
    endif
    ; there is no more work to be done, so receive all the outstanding results from the slaves
    for i=0, n_cpu-1 do begin
      task_id=cpu_pm->waitend()
      dummy=cpu_pm->getresult(task_id)
    endfor
    print, 'done'
endelse

;make movie
if exp_struc.no_pic eq 0 then begin
    ;allow processes to finish converting ps to jpegs
    print, 'waiting 60 secs...'
    wait, 60
        ;for movies
        ;movie_dir = '/local/raid/armadja/data/lhn_scores_img_movies/'
        ;if ctl.plot_score eq 0 then begin
        ;    movie_name = movie_dir+exp_struc.fcst_time_txt
        ;endif else begin
        ;    movie_name = movie_dir+exp_struc.fcst_time_txt+'_scores'
        ;endelse
        ;cd, pic_dir, current=home_dir
        ;cmd = 'mencoder mf://*.jpg -mf fps=10 -o '+movie_name+'.avi -ovc lavc -lavcopts vcodec=msmpeg4v2:vbitrate=7000'
        ;print, cmd
        ;;spawn, cmd
        ;cd, home_dir
    ;copy one image per hour
    img_dir = '/local/raid/armadja/data/lhn_scores_img_movies/img/'+exp_struc.fcst_time_txt+'/'
    if ~file_test(img_dir) then file_mkdir, img_dir
    cmd = 'cp -f '+pic_dir+'*00_00.jpg '+ img_dir
    spawn, cmd

endif




;print, 'creating movie: ~/public_html/ps/anim_2dom.gif'
;SPAWN, 'convert -delay 15   -loop 0   '+pic_dir+'sample*.gif ~/public_html/movies/anim_2dom.gif'
;print, 'done'


end
