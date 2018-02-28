
pro comp_fss, this_l, nx, ny, imin, imax, jmin, jmax, fcst_true, refer_true, $  ;in
              this_fss                                                          ;out
;compute FSS for a given radius
          

    ;make index for this circle
    sq_nx=2L*this_l+1L
    xx = rebin(findgen(sq_nx),sq_nx,sq_nx) - this_l
    yy = rotate(xx,1)
    aa = where(sqrt(xx^2.+yy^2.) le this_l, num_pts)
    model_n = fltarr(nx, ny)
    radar_n = fltarr(nx, ny)
    for ii=imin, imax-1 do begin
        for jj=jmin, jmax-1 do begin
            model_n[ii-imin,jj-jmin] = mean( fcst_true[ii+xx[aa],jj+yy[aa]])
            radar_n[ii-imin,jj-jmin] = mean(refer_true[ii+xx[aa],jj+yy[aa]])
        endfor
    endfor
    mse     = total((radar_n - model_n)^2.)/(nx*ny)
    mse_ref = (total(radar_n^2.) + total(model_n^2.))/(nx*ny)
    this_fss = 1 - mse/mse_ref

end

pro comp_score, missing, exp_struc, nn,                     $
                fcst_field=fcst_field,                      $
                dpdt_field=dpdt_field,                      $
                refer_field=refer_field, radar_qi=radar_qi, $
                fcst_name =fcst_name,                       $
                date_str  =date_str,   date_cmc   =date_cmc,$
                radar_dir_name=radar_dir_name
    ;compute various scores and save the outcome in idl sav files

    ;insure that size of 3 input fields is the same
    sz1 = size(fcst_field)
    sz2 = size(refer_field)
    sz3 = size(radar_qi)
    if (sz1[0] ne sz2[0]) or (sz1[0] ne sz3[0]) then message, 'input fields should be of the same size'
    if (sz1[1] ne sz2[1]) or (sz1[1] ne sz3[1]) then message, 'input fields should be of the same size'

    ;directory where scores are kept
    score_dir = exp_struc.score_dir

    ;minimum quality index for considering radar data
    min_qi = exp_struc.min_qi

    ;number of bins for averaged power spctra
    k_nbins = exp_struc.k_nbins

    ;verification domain
    imin = exp_struc.verif_domain[0]
    jmin = exp_struc.verif_domain[1]
    imax = exp_struc.verif_domain[2]
    jmax = exp_struc.verif_domain[3]

    ;thresholds for score computation
    tresh_arr = exp_struc.tresh_arr
    ntresh    = exp_struc.ntresh

    ;number of levels for wavelet decomp
    n_wav     = exp_struc.nscales

    ;number of bins for histogram
    h_nbins = exp_struc.h_nbins
    hist_min = alog(exp_struc.hist_min)
    hist_max = alog(exp_struc.hist_max)
    hist_delta = (hist_max-hist_min)/h_nbins
    interval, hist_min, hist_max, hist_delta, n_bin=hist_n_bin, val_arr=hist_bin_val, bound_arr=hist_bounds, mm=m, bb=b
    ;transform bins into non-log units
    hist_bin_val= exp(hist_bin_val)
    hist_bounds = exp(hist_bounds)

    ;name of file to save
    dir_name=score_dir+fcst_name+'/'+exp_struc.fcst_time_txt+'/'
    if ~file_test(dir_name,/directory) then file_mkdir, dir_name
    filename=date_str+'.sav'
    savename = dir_name+filename

    ;initialize output structure
    score = {time:0L,$
             ntresh:0L,tresh:fltarr(ntresh),                                                $
             pod:fltarr(ntresh),far:fltarr(ntresh),sr:fltarr(ntresh),                       $
             csi:fltarr(ntresh),gss:fltarr(ntresh),lmin:fltarr(ntresh),                     $
             fbias:fltarr(ntresh),                                                          $
             tot_corr:fltarr(ntresh), corr_hf:fltarr(n_wav),corr_bf:fltarr(n_wav),corr_lf:fltarr(n_wav),$
             hist_nlow:0l, hist_nhigh:0l, hist_count:lonarr(hist_n_bin),                    $
             dpdt:0., areal_cov:fltarr(ntresh),         $
             ps:fltarr(k_nbins)}
    score.time   = date_cmc
    score.ntresh = ntresh
    score.tresh  = tresh_arr


    ;if there was problems reading forecast files, all scores are set to missing and program returns
    aa = where(fcst_field eq missing, naa)
    if naa ne 0 then begin
        score.pod        = missing  
        score.far        = missing 
        score.sr         = missing 
        score.csi        = missing 
        score.gss        = missing 
        score.lmin       = missing 
        score.fbias      = missing 
        score.tot_corr   = missing
        score.corr_hf    = missing
        score.corr_bf    = missing
        score.corr_lf    = missing
        score.hist_nlow  = missing
        score.hist_nhigh = missing
        score.hist_count = missing
        score.ps         = missing

        ;record score
        save, score, filename=savename, /compress
        return
    endif


    ;ones within verification region
    verif_true = bytarr(exp_struc.nx[0],exp_struc.ny[0])
        ;;use a fixed domain for verification
        ;verif_true[imin:imax-1,jmin:jmax-1] = 1
    ;;use quality index for verification
    aa = where(radar_qi gt min_qi, naa)
    if naa gt 0 then verif_true[aa] = 1

    

    ;-------------------------------------------------------
    ;contingency scores + fss decomp
    for tt=0, ntresh-1 do begin
        fcst_true  = fcst_field  ge tresh_arr[tt]
        refer_true = refer_field ge tresh_arr[tt]

        score.fbias[tt] = total((refer_field*refer_true-fcst_field*fcst_true)*verif_true)/total(verif_true)
        x = float(total( fcst_true and  refer_true and verif_true)) ;points don't get counted if they are outside of verification region
        y = float(total(~fcst_true and  refer_true and verif_true))
        z = float(total( fcst_true and ~refer_true and verif_true))
        w = float(total(~fcst_true and ~refer_true and verif_true))
        score.pod[tt] = x/(x+y)           ;probability of detection  % of events that are forecasted
        score.far[tt] = z/(x+z)           ;false allarm rate        (# of false alarms) / (# of predicted events)
        score.sr[tt]  = 1.-score.far[tt]  ;sucess ratio             (# of hits) / (# of predicted events)
        score.csi[tt] = x/(x+y+z)         ;critical sucess index    (# of hits) / (# of events  +  # of false alarm)
        c = (x+y)*(x+z)/(x+y+z+w)         ;event frequency
        score.gss[tt] = (x-c)/(x-c+y+z)   ;Gilbert skill score      # of correct forecasts in excess of those expected by chance 
                                          ;                       / # of cases when there was a threat that would not have been forecasted by chance
                                          ;think about it as csi corrected by chance

        ;compute Lmin for different tresholds
        lmin = 1L    ;minumum radius in pixels must be an odd number
        lmax = 50L   ;maximum radius in pixels
        nx = imax - imin
        ny = jmax - jmin

        ;target FSS to achieve skill
        fss_target = .5 + float(mean(refer_true[imin:imax-1,jmin:jmax-1]))/2.

        ;to save time, check if target fss will be reached with maximum radius
        this_l = lmax
        comp_fss, this_l, nx, ny, imin, imax, jmin, jmax, fcst_true, refer_true, $  ;in
                  this_fss                                                          ;out
        print, '  tresh [mm/h]:',tresh_arr[tt], '  radius [grdpt]:',this_l, '  this_fss:', this_fss, '  fss_target:',fss_target

        if this_fss gt fss_target then begin
            ;fss target will be reached, find pt where this occurs
            this_l = lmin
            this_fss = 0
            while this_fss le fss_target do begin

                comp_fss, this_l, nx, ny, imin, imax, jmin, jmax, fcst_true, refer_true, $  ;in
                          this_fss                                                          ;out

                print, '  tresh [mm/h]:',tresh_arr[tt], '  radius [grdpt]:',this_l, '  this_fss:', this_fss, '  fss_target:',fss_target

                this_l = this_l + 1     ;only odd number of points are tested
            endwhile

            score.lmin[tt] = this_l
        endif else begin
            print, 'FSS target not reached with maximum radius, moving on.'
            ;fss target will not be reached, put NAN and continue with next treshold
            score.lmin[tt] = !values.f_nan
        endelse
    endfor


    ;-------------------------------------------------------
    ;correlation  in verification area
    dum1 = fcst_field[imin:imax-1,jmin:jmax-1]
    dum2 = refer_field[imin:imax-1,jmin:jmax-1]
    dum3 = radar_qi[imin:imax-1,jmin:jmax-1]
    ;zero out all pts where radar quality is insufficient
    aa = where(dum3 lt min_qi, naa)
    if naa ne 0 then begin
        dum1[aa] = 0.
        dum2[aa] = 0.
    endif
    for tt=0, ntresh-1 do begin
        this_dum1 = dum1
        this_dum2 = dum2
        ;replace low values with zeroes
        aa = where(this_dum1 lt tresh_arr[tt], naa)
        if naa ne 0 then this_dum1[aa] = 0.
        aa = where(this_dum2 lt tresh_arr[tt], naa)
        if naa ne 0 then this_dum2[aa] = 0.
        ;select pts for correlation including zeroes
        flat = where(finite(this_dum1))
        score.tot_corr[tt] = correlate(this_dum1[flat],this_dum2[flat])
    endfor


    ;-------------------------------------------------------
    ;wavelet correlation 
    dum1 = fcst_field[imin:imax-1,jmin:jmax-1]
    dum2 = refer_field[imin:imax-1,jmin:jmax-1]
    dum3 = radar_qi[imin:imax-1,jmin:jmax-1]
    ;replace missing with zeroes
    aa = where(dum1 lt 0., naa)
    if naa ne 0 then dum1[aa] = 0.
    aa = where(dum2 lt 0., naa)
    if naa ne 0 then dum2[aa] = 0.
    ;zero out all pts where radar quality is insufficient
    aa = where(dum3 lt min_qi, naa)
    if naa ne 0 then begin
        dum1[aa] = 0.
        dum2[aa] = 0.
    endif
    ;compute wavelet decompositions
    corr_scale_gen, dum1, dum2, scales, corr_lf, corr_bf, corr_hf, nn=n_wav, delta=exp_struc.model_res, verif_dom=verif_domain
    score.corr_hf = corr_hf
    score.corr_bf = corr_bf
    score.corr_lf = corr_lf

    ;-------------------------------------------------------
    ;histogram
    dum1 = alog(fcst_field[imin:imax-1,jmin:jmax-1])
    dum2 = radar_qi[imin:imax-1,jmin:jmax-1]
    aa = where(dum2 gt min_qi, naa)
    if naa gt 0 then begin
        data = dum1[aa]
        ;hist below min thresh
        aa = where(data lt hist_min, naa)
        if naa ne 0 then score.hist_nlow = naa
        ;hist above max thresh
        bb = where(data ge hist_max, nbb)
        if naa ne 0 then score.hist_nhigh = nbb
        ;count pts within range
        cc = where((data ge hist_min) and (data lt hist_max), ncc)
        for ii=0, ncc-1 do begin
            this_ind = floor(m*data[cc[ii]] + b)
            score.hist_count[this_ind] += 1
        endfor
    endif else begin
        score.hist_count = missing
    endelse


    ;-------------------------------------------------------
    ;power spectrum
    dum1 = fcst_field[imin:imax-1,jmin:jmax-1]
    dum2 = radar_qi[imin:imax-1,jmin:jmax-1]
    aa = where(dum2 gt min_qi, naa, complement=bb, ncomplement=nbb)
    if naa gt 0 then begin
        ;set all missing data to zero
        dum1[bb] = 0.
        pow_nx = n_elements(dum1[*,0])
        ;compute model power spectrum
        dct, dum1, dum1_spec
        ;from 2D to 1D power spectrum
        pow_avg, dum1_spec, k_nbins, 1., pow_nx, karr, pow_1d
        score.ps = pow_1d
    endif else begin
        score.ps = missing
    endelse

    ;-------------------------------------------------------
    ;average absolute surface pressure tendencies in verification domain
    score.dpdt = mean(abs(dpdt_field[imin:imax-1,jmin:jmax-1]))

    ;-------------------------------------------------------
    ;aerial coverage of precip
    dum1 = fcst_field[imin:imax-1,jmin:jmax-1]
    dum3 = radar_qi[imin:imax-1,jmin:jmax-1]
    for tt=0, ntresh-1 do begin
        ;select pts 
        aa = where(dum3 gt min_qi, nbase)
        if nbase ne 0 then begin
            bb = where((dum1 ge tresh_arr[tt]) and (dum3 gt min_qi), npts)
            score.areal_cov = float(npts)/float(nbase)*100.
        endif else begin
            score.areal_cov = missing
        endelse
    endfor

    ;record score
    save, score, filename=savename, /compress




    ;compute scores
    ;statistics of radar data------------------------------------------
    if nn eq 0 then begin
        ;name of file to save
        ;dir_name=score_dir+'radar_stats'+'/'+exp_struc.fcst_time_txt+'/'
        dir_name=score_dir+radar_dir_name+'/'+exp_struc.fcst_time_txt+'/'
        if ~file_test(dir_name,/directory) then file_mkdir, dir_name
        filename=date_str+'.sav'
        savename = dir_name+filename

        ;initialize output structure
        score = {time:0L,$
                 hist_nlow:0l, hist_nhigh:0l,                                                                      $
                 hist_count:lonarr(hist_n_bin), hist_bin_val:fltarr(hist_n_bin), hist_bounds:fltarr(hist_n_bin+1), $
                 areal_cov:fltarr(ntresh),                                                                         $
                 ps:fltarr(k_nbins), ps_karr:fltarr(k_nbins)}
        score.time   = date_cmc
        score.hist_bin_val = hist_bin_val
        score.hist_bounds  = hist_bounds

        ;histogram-------------------------------------------------------
        dum1 = refer_field[imin:imax-1,jmin:jmax-1]
        dum2 = radar_qi[imin:imax-1,jmin:jmax-1]
        aa = where(dum2 gt min_qi, naa)
        if naa gt 0 then begin
            data = alog(dum1[aa])

            ;hist below min thresh
            aa = where(data lt hist_min, naa)
            if naa ne 0 then score.hist_nlow = naa
            ;hist above max thresh
            bb = where(data ge hist_max, nbb)
            if naa ne 0 then score.hist_nhigh = nbb
            ;count pts within range
            cc = where((data ge hist_min) and (data lt hist_max), ncc)
            for ii=0, ncc-1 do begin
                this_ind = floor(m*data[cc[ii]] + b)
                score.hist_count[this_ind] += 1
            endfor
        endif else begin
            score.hist_count = missing
        endelse

        ;-------------------------------------------------------
        ;power spectrum
        dum1 = refer_field[imin:imax-1,jmin:jmax-1]
        dum2 = radar_qi[imin:imax-1,jmin:jmax-1]
        aa = where(dum2 gt min_qi, naa, complement=bb, ncomplement=nbb)
        if naa gt 0 then begin
            ;set all missing data to zero
            dum1[bb] = 0.
            pow_nx = n_elements(dum1[*,0])
            ;compute radar power spectrum
            dct, dum1, dum1_spec
            ;from 2D to 1D power spectrum
            pow_avg, dum1_spec, k_nbins, 1., pow_nx, karr, pow_1d
            score.ps = pow_1d
        endif else begin
            score.ps = missing
        endelse

        ;-------------------------------------------------------
        ;aerial coverage of precip
        dum1 = refer_field[imin:imax-1,jmin:jmax-1]
        dum3 = radar_qi[imin:imax-1,jmin:jmax-1]
        for tt=0, ntresh-1 do begin
            ;select pts 
            aa = where(dum3 gt min_qi, nbase)
            if nbase ne 0 then begin
                bb = where((dum1 ge tresh_arr[tt]) and (dum3 gt min_qi), npts)
                score.areal_cov = float(npts)/float(nbase)*100.
            endif else begin
                score.areal_cov = missing
            endelse
        endfor

        ;save wavenumber from pow_spec
        score.ps_karr = karr

        ;record score
        save, score, filename=savename, /compress
    endif

end
