PRO FILL_STR, time_t, time_mdt, time_pr, time_pr_mdt, data_dir, str
    ;fill data structure where data files are associated to desired time for plotting

    ;time for P0 tendency computation
    MATCH_TIME, time_t,          data_dir, 'P0', $
                file_t,          AVAIL_TIME_ARR=avail_time_arr_p0, AVAIL_FILES=avail_files_p0   
    MATCH_TIME, time_mdt,        data_dir, 'P0', $
                file_mdt,        AVAIL_TIME_ARR=avail_time_arr_p0, AVAIL_FILES=avail_files_p0  
    ;time for PR accum computation
    MATCH_TIME, time_pr,         data_dir, 'PR', $
                file_pr,         AVAIL_TIME_ARR=avail_time_arr_pr, AVAIL_FILES=avail_files_pr   
    MATCH_TIME, time_pr_mdt,     data_dir, 'PR', $
                file_pr_mdt,     AVAIL_TIME_ARR=avail_time_arr_pr, AVAIL_FILES=avail_files_pr   
    ;fill structure
    str.time_t      = time_t
    str.file_t      = file_t
    str.time_mdt    = time_mdt
    str.file_mdt    = file_mdt
    str.time_pr     = time_pr
    str.file_pr     = file_pr
    str.time_pr_mdt = time_pr_mdt
    str.file_pr_mdt = file_pr_mdt

END

PRO FST_FILE_LIST, t0, tf, model_dt, pr_dt, img_dt,               $  ;input related to time
                   exp_list, exp_desc, mat_lat, mat_lon, proj_ind,$  ;input related to files list & projection
                   NEAREST=nearest, PR_NEAREST=pr_nearest,        $
                   exp_struc                                         ;output
    
    ;generates a list of files to use for the generation of images/statistics
    ;first a list of times to consider is made
    ;the program then search in wich files the desired outputs are found for a given list of experiments

    ;insure that there is a description for each experiment
    nexp   = N_ELEMENTS(exp_list)
    ndesc  = N_ELEMENTS(exp_desc)
    IF nexp NE ndesc THEN MESSAGE, 'number of experiments must be equal to number of descriptions'

    ;size of model domain
    sz = SIZE(mat_lat,/DIM)
    nx = sz[0]
    ny = sz[1]

    ;generate list of times
    TIME_LIST, t0, tf, model_dt, pr_dt, img_dt,       $ ;in
               NEAREST=nearest, PR_NEAREST=pr_nearest,$
               time                                     ;out

    ;initialize data structure 
    nt = time.nt
    template={desc:'',code:'',nt:0L,                     $
             time_str:STRARR(nt),img_time_str:STRARR(nt),$
             time_t:LONARR(nt),  time_mdt:LONARR(nt),    $
             file_t:STRARR(nt),  file_mdt:STRARR(nt),    $
             time_pr:LONARR(nt), time_pr_mdt:LONARR(nt), $
             file_pr:STRARR(nt), file_pr_mdt:STRARR(nt), $
             nx:0L, ny:0L, model_dt:0L,pr_dt:0L,         $
             proj_ind:LONARR(SIZE(proj_ind,/DIM)),       $
             lat:FLTARR(SIZE(mat_lat,/DIM)),             $
             lon:FLTARR(SIZE(mat_lon,/DIM))}
    ;replicate for a given number of experiments:
    exp_struc= REPLICATE(template,nexp)

    ;fill data structure 
    FOR nn=0, nexp-1 DO BEGIN
        ;if experiment is same as the previous one, just replicate everything
        IF nn NE 0 THEN BEGIN
            IF exp_list[nn] EQ exp_list[nn-1] THEN BEGIN
                exp_struc[nn] = exp_struc[nn-1]
                CONTINUE
            ENDIF 
        ENDIF

        ;fill in values
        exp_struc[nn].nt           = nt
        exp_struc[nn].code         = FILE_BASENAME(exp_list[nn])
        exp_struc[nn].desc         = exp_desc[nn]
        exp_struc[nn].time_str     = time.t_str
        exp_struc[nn].img_time_str = time.img_str
        dummy = exp_struc[nn]
        FILL_STR, time.t_cmc, time.mdt_cmc, time.pr_cmc, time.pr_mdt_cmc, exp_list[nn]+'/', dummy
        exp_struc[nn]              = dummy 
        exp_struc[nn].nx           = nx
        exp_struc[nn].ny           = ny
        exp_struc[nn].model_dt     = model_dt
        exp_struc[nn].pr_dt        = pr_dt
        exp_struc[nn].proj_ind     = proj_ind
        exp_struc[nn].lat          = mat_lat
        exp_struc[nn].lon          = mat_lon
    ENDFOR

END
