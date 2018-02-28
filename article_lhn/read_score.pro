pro read_score, missing, fcst_time_txt, desired_times,                 $
                fcst_name  = fcst_name,                         $
                nscales=nscales, ntresh=ntresh, k_nbins=k_nbins, h_nbins=h_nbins,  $
                fbias        = fbias_arr     , $     
                pod          = pod_arr       , $
                far          = far_arr       , $
                sr           = sr_arr        , $
                csi          = csi_arr       , $
                gss          = gss_arr       , $
                lmin         = lmin_arr      , $
                tot_corr     = tot_corr_arr  , $
                corr_hf      = corr_hf_arr   , $
                corr_bf      = corr_bf_arr   , $
                corr_lf      = corr_lf_arr   , $
                hist_bin_val = hist_bin_val  , $
                hist_bounds  = hist_bounds   , $
                hist_nlow    = hist_nlow_arr , $
                hist_nhigh   = hist_nhigh_arr, $
                hist_count   = hist_count_arr, $
                ps_karr      = ps_karr       , $
                ps_arr       = ps_arr        , $
                dpdt         = dpdt_arr      , $
                areal_cov    = areal_cov_arr 

    ;get various scores computed by the sister routine comp_score
    ;    scores are returned in an array matching desired_times

    ;directory where scores are kept
    score_dir = '/local/raid/armadja/data/lhn_scores_img_movies/'

    ;initialize output arr
    nt = n_elements(desired_times)
    pod_arr        = replicate(float(missing),nt,ntresh)
    far_arr        = replicate(float(missing),nt,ntresh)
    sr_arr         = replicate(float(missing),nt,ntresh)
    fbias_arr      = replicate(float(missing),nt,ntresh)
    csi_arr        = replicate(float(missing),nt,ntresh)
    gss_arr        = replicate(float(missing),nt,ntresh)
    lmin_arr       = replicate(float(missing),nt,ntresh)
    tot_corr_arr   = replicate(float(missing),nt,ntresh)
    corr_hf_arr    = replicate(float(missing),nt,nscales)
    corr_bf_arr    = replicate(float(missing),nt,nscales)
    corr_lf_arr    = replicate(float(missing),nt,nscales)
    hist_bin_val   = replicate(float(missing),h_nbins)
    hist_bounds    = replicate(float(missing),h_nbins+1)
    hist_nlow_arr  = replicate(float(missing),nt)
    hist_nhigh_arr = replicate(float(missing),nt)
    hist_count_arr = replicate(float(missing),nt,h_nbins)
    ps_karr        = replicate(float(missing),k_nbins)
    ps_arr         = replicate(float(missing),nt,k_nbins)
    dpdt_arr       = replicate(float(missing),nt)
    areal_cov_arr  = replicate(float(missing),nt,ntresh)
    
    ;fill output array
    dir_name=score_dir+fcst_name+'/'+fcst_time_txt+'/'
    ff = file_search(dir_name+'*', count=nf)
    count=0
    for nn=0, nf-1 do begin
        restore, ff[nn] 
        ;look for right place to put the data
        aa = value_locate(desired_times, score.time)
    
        ;if index is out of range, keep searching
        if (aa eq 0) or (aa ge nt-1) then continue
    
        ;we have a match
        if strmid(fcst_name,0,5) eq 'radar' then begin
            if count eq 0 then begin
                hist_bin_val     = score.hist_bin_val
                hist_bounds      = score.hist_bounds
                ps_karr          = score.ps_karr
            endif
             hist_nlow_arr[aa]   = score.hist_nlow
            hist_nhigh_arr[aa]   = score.hist_nhigh
            hist_count_arr[aa,*] = score.hist_count
                    ps_arr[aa,*] = score.ps         
             areal_cov_arr[aa,*] = score.areal_cov
        endif else begin
                 fbias_arr[aa,*] = score.fbias
                   pod_arr[aa,*] = score.pod
                   far_arr[aa,*] = score.far
                    sr_arr[aa,*] = score.sr 
                   csi_arr[aa,*] = score.csi
                   gss_arr[aa,*] = score.gss
                  lmin_arr[aa,*] = score.lmin
              tot_corr_arr[aa,*] = score.tot_corr
               corr_hf_arr[aa,*] = score.corr_hf
               corr_bf_arr[aa,*] = score.corr_bf
               corr_lf_arr[aa,*] = score.corr_lf
             hist_nlow_arr[aa]   = score.hist_nlow
            hist_nhigh_arr[aa]   = score.hist_nhigh
            hist_count_arr[aa,*] = score.hist_count
                    ps_arr[aa,*] = score.ps         
                  dpdt_arr[aa,*] = score.dpdt
             areal_cov_arr[aa,*] = score.areal_cov
        endelse
    
        if (nn mod 100) eq 0 then print, nn, ' of ', nf-1
    endfor
end
