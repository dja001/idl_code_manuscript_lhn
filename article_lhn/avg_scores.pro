;main around line 640


pro scores_multi_fcsts, ss
    ;plot average scores for all the forecast cases defined in avg_scores


    ;set up image coords setup 
    ps = {pic_w:0.,pic_h:0.,sq_w:0.,sq_h:0.,pal_sp:0.,pal_w:0.,rec_w:0.,rec_h:0.,sp_w:0.,sp_h:0.,$
             x1:0.,   x2:0.,  x3:0.,  x4:0.,    x5:0.,   x6:0.,   x7:0.,   x8:0.,  x9:0.,        $
             y1:0.,   y2:0.,  y3:0.,  y4:0.,    y5:0.,   y6:0.,   y7:0.,   y8:0.,  y9:0.}
    ps.sq_w   = 14.
    ps.sq_h   = 10.
    ps.pic_h  = 70.
    ps.pic_w  = 300
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

    pic_dir = '/local/raid/armadja/data/lhn_scores_img_movies/pdfs/' 
    ;pic_name = pic_dir+'avg_scores_all_08_avg_05_min.ps'
    ;pic_name = pic_dir+'avg_scores_all_12_avg_09_min.ps'
    pic_name = pic_dir+'avg_scores_all_12_avg_09_min_002_005_006.ps'
    ;pic_name = pic_dir+'avg_scores_all_02_avg_01_min.ps'
    ps_start, pic_name, ps.pic_w, ps.pic_h, /white, charsize=2.

    ;print title
    xyouts, ps.x1+.001*ps.rec_w, 1.-1./ps.pic_h, 'Averaged scores', /normal,charsize=2.
    for ff=0., ss.nfcsts-1 do begin 
        xyouts, ps.x1+.001*ps.rec_w, 1.-5./ps.pic_h-(ff*1.)/ps.pic_h, ss.fcst_time_txt[ff], /normal,charsize=2.
        print, 1.-3./ps.pic_h-(ff*1.)/ps.pic_h
    endfor

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
    for nn=0, ss.nexp-1 do begin
        ;color rectangle for title
        y0 = 1.-3./ps.pic_h
        x0 = ps.x1 + nn*(ps.rec_w + ps.sp_w)
        pos = [x0,y0,x0+ps.rec_w,y0+.45*ps.rec_h]
        tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
        tv,[1], x0, y0, xs=ps.rec_w, ys=1.7/ps.pic_h, /normal
        ;title text
        loadct, 40, /s
        xyouts, ps.x1+nn*(ps.rec_w + ps.sp_w),1.-2.7/ps.pic_h,ss.desc[nn],/normal,charsize=3., col=255
    endfor

    ;maximum lead time to display
    max_lt = 12d    ;hours
    score_dt = 10.  ;minutes
    lt_arr = long(score_dt*dindgen(60.*max_lt/score_dt+1))    ;time array in minutes
    nt = n_elements(lt_arr)

    ;plot average score for each tresholds
    for tt=0, ss.ntresh-1 do begin
        tresh_ind = tt
        tresh_txt = string(ss.tresh_arr[tt],format='(f4.1)')
        missing = ss.missing

        voffset = 1.95*tt

        plot_score_lt, nn, ss, lt_arr, missing, $
                       voffset+1.50, ps,  multi_forecast=ff,$
                       'POD'+tresh_txt, [0.,.60], ss.pod,tresh_ind=tresh_ind,yticks=2
        plot_score_lt, nn, ss, lt_arr, missing, $
                       voffset+1.25, ps,  multi_forecast=ff,$
                       'FAR'+tresh_txt, [0.35,.85], ss.far,tresh_ind=tresh_ind,yticks=4
        plot_score_lt, nn, ss, lt_arr, missing, $
                       voffset+1., ps,    multi_forecast=ff,$
                       'CSI'+tresh_txt, [0.,.45], ss.csi,tresh_ind=tresh_ind
        ;plot_score_lt, nn, ss, lt_arr, missing, $
        ;               voffset+0.75, ps,  multi_forecast=ff,$
        ;               'GSS'+tresh_txt, [0.,.45], ss.gss,tresh_ind=tresh_ind
        plot_score_lt, nn, ss, lt_arr, missing, $
                       voffset+0.75, ps,  multi_forecast=ff,$
                       'Lmin'+tresh_txt,  [0.,250], ss.lmin,tresh_ind=tresh_ind, yticks=2
        plot_score_lt, nn, ss, lt_arr, missing, $ 
                       voffset+0.50, ps, multi_forecast=ff,$
                       'Correlation', [0.,.7], ss.tot_corr, tresh_ind=tresh_ind, yticks=2
        plot_score_lt, nn, ss, lt_arr, missing, $ 
                       voffset+0.25, ps, multi_forecast=ff,$
                       'dpdt', [0.004,.008], ss.dpdt, tresh_ind=tresh_ind, yticks=4, /bot
        ;plot_score_lt, nn, ss, lt_arr, missing, $
        ;               voffset+0.25, ps,  multi_forecast=ff,$
        ;               'freq. Bias'+tresh_txt,  [-.1,0.3], -1.*ss.fbias,tresh_ind=tresh_ind, /bot
    endfor

    ;averages
    avg_dt = 1.    ;width of averaging window in hours
    ;transform time_arr in hours starting at -3
    time_arr = (lt_arr/60d) -3.
    t0 = min(time_arr, max=tf)
    ;how many periods?
    n_windows = floor((tf-t0)/(avg_dt))
    avg_wavelets = fltarr(ss.nscales,ss.nexp,n_windows)
    this_start = t0
    count = 0l
    x0 = 68./ps.pic_w

    nscales = ss.nscales
    scales  = ss.scales
    xtickv = findgen(ss.nscales)
    xtickname = strarr(nscales)
    for sc=0, nscales-1 do xtickname[nscales-1-sc] = strtrim(string(scales[sc],format='(i6)'))

    while count lt n_windows do begin
         
        ;determine stop time
        this_stop = this_start + avg_dt

        ;time at start and end of period
        start_txt = string(this_start,format='(i02)')
        stop_txt  = string(this_stop, format='(i02)')

        tx0 = x0 + count*(ps.rec_w+ps.sp_w)
        aa = where(time_arr gt this_start and time_arr le this_stop, naa)
        print, count, this_start, this_stop

        ;Wavelet averages----------------------------------------
        ;determine position
        y0 = 20./ps.pic_h
        xr = [-.2,6.2]
        yr = [-.1,1.01]
        pos = [tx0,y0,tx0+ps.rec_w,y0+.55*ps.rec_h]
        loadct, 0, /s
        plot, [0], /nodata, /noerase, /normal, pos=pos, col=0,  $
              title='jul'+start_txt+'h-jul'+stop_txt+'h',       $
              xs=1, xrange=xr, xtitle='scale [km]', xtickv=xtickv, xticks=n_elements(xtickv)-1,xminor=1, xtickname=xtickname,  $
              ys=1, yrange=yr, ytitle='corr coef', yticklen=.004, yminor=1, yticks=4.
        ;line at zero
        oplot, xr, [0,0], col=100
        ;for each experiment
        ;set possible missing values to nan
        bb = where(ss.corr_bf eq missing, nbb)
        if nbb ne 0 then ss.corr_bf[bb] = !values.f_nan
        for nn=0, ss.nexp-1 do begin
            ;load color associated with this experiment
            tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
                ;;plot maximum and minimum
                ;max_r = fltarr(ss.nscales)
                ;min_r = fltarr(ss.nscales)+1.
                ;for t_ind=0, naa-1 do begin
                ;    these_vals = ss.corr_bf[aa[t_ind],*,nn,ff]
                ;    ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

                ;    for ss=0, nscales-1 do begin
                ;        if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                ;        if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
                ;    endfor
                ;endfor
            ;sum within averaging window
            tot1   = total(       reform(ss.corr_bf[aa,*,nn,*]),  1,/nan)
            count1 = total(finite(reform(ss.corr_bf[aa,*,nn,*])), 1)
            ;sum for each forecasts
            tot2   = total(tot1,  2)
            count2 = total(count1,2)
            this_avg = tot2/count2
            oplot, reverse(xtickv), this_avg, col=1, thick=6.5
            ;oplot, reverse(xtickv), (max_r), col=1, thick=1.5
            ;oplot, reverse(xtickv), (min_r), col=1, thick=1.5
        endfor

        ;spectrum averages----------------------------------------
        y0 = 12./ps.pic_h
        pos = [tx0,y0,tx0+ps.rec_w,y0+.55*ps.rec_h]
        xr = [1.,300]
        yr = [1e-8,1e-1]
        loadct, 0, /s
        plot, [0], /nodata, /noerase, /normal, pos=pos, col=0,  $
              xs=1, xrange=xr, xtitle='Wavenumber', xlog=1, $;xtickv=xtickv, xticks=n_elements(xtickv)-1,xminor=1, xtickname=xtickname,  $
              ys=1, yrange=yr, ytitle='power', yticklen=.004, yminor=1, ylog=1
        ;for radar data
            ;max_r = fltarr(ss.k_nbins)+yr[1]
            ;min_r = fltarr(ss.k_nbins)+yr[0]
            ;for t_ind=0, naa-1 do begin
            ;    these_vals = ss.rad_ps[aa[t_ind],*,ff]
            ;    ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

            ;    for ss=0, ss.k_nbins-1 do begin
            ;        if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
            ;        if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
            ;    endfor
            ;endfor
        ;plot average
        ;set possible missing values to nan
        bb = where(ss.rad_ps eq missing, nbb)
        if nbb ne 0 then ss.rad_ps[bb] = !values.f_nan

        ;sum within averaging window
        tot1   = total(       reform(ss.rad_ps[aa,*,*]),  1,/nan)
        count1 = total(finite(reform(ss.rad_ps[aa,*,*])), 1)
        ;sum for each forecasts
        tot2   = total(tot1,  2)
        count2 = total(count1,2)
        this_avg = tot2/count2
        oplot, ss.ps_karr, this_avg, col=0, thick=5.5
        ;oplot, ss.ps_karr, max_r,    col=0, thick=1.5
        ;oplot, ss.ps_karr, min_r,    col=0, thick=1.5


        ;for each experiment
        bb = where(ss.ps eq missing, nbb)
        if nbb ne 0 then ss.ps[bb] = !values.f_nan
        for nn=0, ss.nexp-1 do begin
            ;load color associated with this experiment
            tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
                ;;plot individual components
                ;max_r = fltarr(ss.k_nbins)+xr[1]
                ;min_r = fltarr(ss.k_nbins)+xr[0]
                ;for t_ind=0, naa-1 do begin
                ;    these_vals = ss.ps[aa[t_ind],*,nn,ff]
                ;    ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

                ;    for ss=0, ss.k_nbins-1 do begin
                ;        if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                ;        if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
                ;    endfor
                ;endfor
            ;plot average
            ;sum within averaging window
            tot1   = total(       reform(ss.ps[aa,*,nn,*]),  1,/nan)
            count1 = total(finite(reform(ss.ps[aa,*,nn,*])), 1)
            ;sum for each forecasts
            tot2   = total(tot1,  2)
            count2 = total(count1,2)
            this_avg = tot2/count2
            oplot, ss.ps_karr, this_avg, col=1, thick=5.5
            ;oplot, ss.ps_karr, max_r,    col=1, thick=1.5
            ;oplot, ss.ps_karr, min_r,    col=1, thick=1.5
        endfor

        ;histogram averages----------------------------------------
        y0 = 4./ps.pic_h
        pos = [tx0,y0,tx0+ps.rec_w,y0+.55*ps.rec_h]
        xr = [.1,20.]
        yr = [0.,100]
        loadct, 0, /s
        plot, [0], /nodata, /noerase, /normal, pos=pos, col=0,  $
              xs=1, xrange=xr, xtitle='precip rate [mm/h]', xlog=1, $;xtickv=xtickv, xticks=30,xminor=1, xtickname=xtickname,  $
              ys=1, yrange=yr, ytitle='count', yticklen=.004, yminor=1, yticks=4., ylog=0
            ;;for radar data
            ;max_r = fltarr(ss.h_nbins)+xr[1]
            ;min_r = fltarr(ss.h_nbins)+xr[0]
            ;for t_ind=0, naa-1 do begin
            ;    these_vals = ss.rad_hist_count[aa[t_ind],*,ff]
            ;    ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1
            ;    for ss=0, ss.h_nbins-1 do begin
            ;        if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
            ;        if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
            ;    endfor
            ;endfor

        ;set possible missing values to nan
        bb = where(ss.rad_hist_count eq missing, nbb)
        if nbb ne 0 then ss.rad_hist_count[bb] = !values.f_nan

        ;plot average
        ;sum within averaging window
        tot1   = total(       reform(ss.rad_hist_count[aa,*,*]),  1,/nan)
        count1 = total(finite(reform(ss.rad_hist_count[aa,*,*])), 1)
        ;sum for each forecasts
        tot2   = total(tot1,  2)
        count2 = total(count1,2)
        this_avg = tot2/count2
        oplot, ss.hist_bin_val, this_avg, col=0, thick=5.5
        ;oplot, ss.hist_bin_val, max_r,    col=0, thick=1.5
        ;oplot, ss.hist_bin_val, min_r,    col=0, thick=1.5
        ;for each experiment
        for nn=0, ss.nexp-1 do begin
            ;load color associated with this experiment
            tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
                ;;plot individual components
                ;max_r = fltarr(ss.h_nbins)+xr[1]
                ;min_r = fltarr(ss.h_nbins)+xr[0]
                ;for t_ind=0, naa-1 do begin
                ;    these_vals = ss.hist_count[aa[t_ind],*,nn,ff]
                ;    ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

                ;    for ss=0, ss.h_nbins-1 do begin
                ;        if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                ;        if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
                ;    endfor
                ;endfor
            ;set possible missing values to nan
            bb = where(ss.hist_count eq missing, nbb)
            if nbb ne 0 then ss.hist_count[bb] = !values.f_nan
            ;plot average
            ;sum within averaging window
            tot1   = total(       reform(ss.hist_count[aa,*,nn,*]),  1,/nan)
            count1 = total(finite(reform(ss.hist_count[aa,*,nn,*])), 1)
            ;sum for each forecasts
            tot2   = total(tot1,  2)
            count2 = total(count1,2)
            this_avg = tot2/count2
            oplot, ss.hist_bin_val, this_avg, col=1, thick=5.5
            ;oplot, ss.hist_bin_val, max_r,    col=1, thick=1.5
            ;oplot, ss.hist_bin_val, min_r,    col=1, thick=1.5
        endfor



        ;update start time
        count ++
        this_start = t0 + count*avg_dt
    endwhile













    ps_close, pic_name, /del_ps, font='lmroman', /v, /pdf


end







pro scores_one_time, ss, this_time_txt
    ;plot scores for an individual forecast
    ;wavelet decomp, power_spec and histograms are averaged over a number of hours

    ;get forecast time index
    ff = where(ss.fcst_time_txt eq this_time_txt,naa)
    if naa ne 1 then message, 'none or too many time indexes'

    avg_dt = 48d ;averaging window in hours





    ;set up image coords setup 
    ps = {pic_w:0.,pic_h:0.,sq_w:0.,sq_h:0.,pal_sp:0.,pal_w:0.,rec_w:0.,rec_h:0.,sp_w:0.,sp_h:0.,$
             x1:0.,   x2:0.,  x3:0.,  x4:0.,    x5:0.,   x6:0.,   x7:0.,   x8:0.,  x9:0.,        $
             y1:0.,   y2:0.,  y3:0.,  y4:0.,    y5:0.,   y6:0.,   y7:0.,   y8:0.,  y9:0.}
    ps.sq_w   = 14.
    ps.sq_h   = 10.
    ps.pic_h  = 70.
    ps.pic_w  = 180
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

    pic_dir = '/local/raid/armadja/data/lhn_scores_img_movies/pdfs/' 
    pic_name = pic_dir+'scores_'+ss.fcst_time_txt[ff]+'.ps'
    ps_start, pic_name, ps.pic_w, ps.pic_h, /white

    ;print title
    xyouts, ps.x1+.001*ps.rec_w, 1.-1./ps.pic_h, ss.fcst_time_txt[ff], /normal,charsize=2.

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
    for nn=0, ss.nexp-1 do begin
        ;color rectangle for title
        y0 = 1.-3./ps.pic_h
        x0 = ps.x1 + nn*(ps.rec_w + ps.sp_w)
        pos = [x0,y0,x0+ps.rec_w,y0+.45*ps.rec_h]
        tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
        tv,[1], x0, y0, xs=ps.rec_w, ys=1.7/ps.pic_h, /normal
        ;title text
        loadct, 40, /s
        xyouts, ps.x1+nn*(ps.rec_w + ps.sp_w),1.-2.7/ps.pic_h,ss.desc[nn],/normal,charsize=3., col=255
    endfor

    ;plot_corr_bp, ii, nn, ss, missing,$ ;base info
    ;       1.55, ps                            ;image position info
    ;plot_spec,    ii, nn, ss, missing,$ ;base info
    ;       1.55, ps                            ;image position info
    ;plot_hist,    ii, nn, ss, missing,$ ;base info
    ;       1.55, ps                            ;image position info
        
    for tt=0, ss.ntresh-1 do begin
        tresh_ind = tt
        tresh_txt = string(ss.tresh_arr[tt],format='(f4.1)')
        missing = ss.missing
        ii = missing

        voffset = 1.95*tt

        plot_score, ii, nn, ss, missing, $
                   voffset+1.50, ps,  multi_forecast=ff,$
                   'POD'+tresh_txt, [0.,.75], ss.pod,tresh_ind=tresh_ind,yticks=3
        plot_score, ii, nn, ss, missing, $
                   voffset+1.25, ps,  multi_forecast=ff,$
                   'FAR'+tresh_txt, [0.7,1.0], ss.far,tresh_ind=tresh_ind,yticks=3
        plot_score, ii, nn, ss, missing, $
                   voffset+1., ps,    multi_forecast=ff,$
                   'CSI'+tresh_txt, [0.,.45], ss.csi,tresh_ind=tresh_ind
        plot_score, ii, nn, ss, missing, $
                   voffset+0.75, ps,  multi_forecast=ff,$
                   'GSS'+tresh_txt, [0.,.45], ss.gss,tresh_ind=tresh_ind
        plot_score, ii, nn, ss, missing, $
                   voffset+0.50, ps,  multi_forecast=ff,$
                   'Lmin'+tresh_txt,  [0.,100], ss.lmin,tresh_ind=tresh_ind
        plot_score, ii, nn, ss, missing, $ 
                   voffset+0.25, ps, multi_forecast=ff,$
                   'Correlation', [0.,.7], ss.tot_corr, yticks=2
        plot_score, ii, nn, ss, missing, $
                   voffset+0.00, ps,  multi_forecast=ff,$
                   'freq. Bias'+tresh_txt,  [-.1,0.3], -1.*ss.fbias,tresh_ind=tresh_ind
    endfor


    ;averages
    time_arr = ss.time_jul[*,ff]
    t0 = min(time_arr, max=tf)
    ;how many periods?
    n_windows = floor((tf-t0)/(avg_dt/24.))
    avg_wavelets = fltarr(ss.nscales,ss.nexp,n_windows)
    this_start = t0
    count = 0l
    x0 = 35./ps.pic_w

    nscales = ss.nscales
    scales  = ss.scales
    xtickv = findgen(ss.nscales)
    xtickname = strarr(nscales)
    for ss=0, nscales-1 do xtickname[nscales-1-ss] = strtrim(string(scales[ss],format='(i6)'))

    while count lt n_windows do begin
         

        ;determine stop time
        this_stop = this_start + avg_dt/24d

        ;time at start and end of period
        caldat, this_start, mo, dd, yy, hh, mm, se
        start_txt = string(dd,hh,format='(2i02)')
        caldat, this_stop,  mo, dd, yy, hh, mm, se
        stop_txt  = string(dd,hh,format='(2i02)')

        tx0 = x0 + count*(ps.rec_w+ps.sp_w)
        aa = where(time_arr gt this_start and time_arr le this_stop, naa)

        ;Wavelet averages----------------------------------------
        ;determine position
        y0 = 20./ps.pic_h
        xr = [-.2,6.2]
        yr = [-.1,1.01]
        pos = [tx0,y0,tx0+ps.rec_w,y0+.55*ps.rec_h]
        loadct, 0, /s
        plot, [0], /nodata, /noerase, /normal, pos=pos, col=0,  $
              title='jul'+start_txt+'h-jul'+stop_txt+'h',       $
              xs=1, xrange=xr, xtitle='scale [km]', xtickv=xtickv, xticks=n_elements(xtickv)-1,xminor=1, xtickname=xtickname,  $
              ys=1, yrange=yr, ytitle='corr coef', yticklen=.004, yminor=1, yticks=4.
        ;line at zero
        oplot, xr, [0,0], col=100
        ;for each experiment
        for nn=0, ss.nexp-1 do begin
            ;load color associated with this experiment
            tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
            ;plot individual components
            max_r = fltarr(ss.nscales)
            min_r = fltarr(ss.nscales)+1.
            for t_ind=0, naa-1 do begin
                these_vals = ss.corr_bf[aa[t_ind],*,nn,ff]
                ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

                for ss=0, nscales-1 do begin
                    if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                    if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
                endfor
            endfor
            ;plot average
            this_avg = total(ss.corr_bf[aa,*,nn,ff],1)/naa
            oplot, reverse(xtickv), (this_avg), col=1, thick=5.5
            ;oplot, reverse(xtickv), (max_r), col=1, thick=1.5
            ;oplot, reverse(xtickv), (min_r), col=1, thick=1.5
        endfor

        ;spectrum averages----------------------------------------
        y0 = 12./ps.pic_h
        pos = [tx0,y0,tx0+ps.rec_w,y0+.55*ps.rec_h]
        xr = [1.,300]
        yr = [1e-8,1e-1]
        loadct, 0, /s
        plot, [0], /nodata, /noerase, /normal, pos=pos, col=0,  $
              xs=1, xrange=xr, xtitle='Wavenumber', xlog=1, $;xtickv=xtickv, xticks=n_elements(xtickv)-1,xminor=1, xtickname=xtickname,  $
              ys=1, yrange=yr, ytitle='power', yticklen=.004, yminor=1, ylog=1
        ;for radar data
        max_r = fltarr(ss.k_nbins)+yr[1]
        min_r = fltarr(ss.k_nbins)+yr[0]
        for t_ind=0, naa-1 do begin
            these_vals = ss.rad_ps[aa[t_ind],*,ff]
            ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

            for ss=0, ss.k_nbins-1 do begin
                if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
            endfor
        endfor
        ;plot average
        this_avg = total(ss.rad_ps[aa,*,ff],1)/naa
        oplot, ss.ps_karr, this_avg, col=0, thick=5.5
        ;oplot, ss.ps_karr, max_r,    col=0, thick=1.5
        ;oplot, ss.ps_karr, min_r,    col=0, thick=1.5
        ;for each experiment
        for nn=0, ss.nexp-1 do begin
            ;load color associated with this experiment
            tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
            ;plot individual components
            max_r = fltarr(ss.k_nbins)+xr[1]
            min_r = fltarr(ss.k_nbins)+xr[0]
            for t_ind=0, naa-1 do begin
                these_vals = ss.ps[aa[t_ind],*,nn,ff]
                ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

                for ss=0, ss.k_nbins-1 do begin
                    if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                    if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
                endfor
            endfor
            ;plot average
            this_avg = total(ss.ps[aa,*,nn,ff],1)/naa
            oplot, ss.ps_karr, this_avg, col=1, thick=5.5
            ;oplot, ss.ps_karr, max_r,    col=1, thick=1.5
            ;oplot, ss.ps_karr, min_r,    col=1, thick=1.5
        endfor

        ;histogram averages----------------------------------------
        y0 = 4./ps.pic_h
        pos = [tx0,y0,tx0+ps.rec_w,y0+.55*ps.rec_h]
        xr = [.1,20.]
        yr = [0.,100]
        loadct, 0, /s
        plot, [0], /nodata, /noerase, /normal, pos=pos, col=0,  $
              xs=1, xrange=xr, xtitle='precip rate [mm/h]', xlog=1, $;xtickv=xtickv, xticks=30,xminor=1, xtickname=xtickname,  $
              ys=1, yrange=yr, ytitle='count', yticklen=.004, yminor=1, yticks=4., ylog=0
        ;for radar data
        max_r = fltarr(ss.h_nbins)+xr[1]
        min_r = fltarr(ss.h_nbins)+xr[0]
        for t_ind=0, naa-1 do begin
            these_vals = ss.rad_hist_count[aa[t_ind],*,ff]
            ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1
            for ss=0, ss.h_nbins-1 do begin
                if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
            endfor
        endfor
        ;plot average
        this_avg = total(ss.rad_hist_count[aa,*,ff],1)/naa
        oplot, ss.hist_bin_val, this_avg, col=0, thick=5.5
        ;oplot, ss.hist_bin_val, max_r,    col=0, thick=1.5
        ;oplot, ss.hist_bin_val, min_r,    col=0, thick=1.5
        ;for each experiment
        for nn=0, ss.nexp-1 do begin
            ;load color associated with this experiment
            tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
            ;plot individual components
            max_r = fltarr(ss.h_nbins)+xr[1]
            min_r = fltarr(ss.h_nbins)+xr[0]
            for t_ind=0, naa-1 do begin
                these_vals = ss.hist_count[aa[t_ind],*,nn,ff]
                ;oplot, reverse(xtickv), reverse(these_vals), col=1, thick=1.5, linestyle=1

                for ss=0, ss.h_nbins-1 do begin
                    if these_vals[ss] gt max_r[ss] then max_r[ss] = these_vals[ss]
                    if these_vals[ss] lt min_r[ss] then min_r[ss] = these_vals[ss]
                endfor
            endfor
            ;plot average
            this_avg = total(ss.hist_count[aa,*,nn,ff],1)/naa
            oplot, ss.hist_bin_val, this_avg, col=1, thick=5.5
            ;oplot, ss.hist_bin_val, max_r,    col=1, thick=1.5
            ;oplot, ss.hist_bin_val, min_r,    col=1, thick=1.5
        endfor



        ;update start time
        count ++
        this_start = t0 + count*avg_dt/24d
    endwhile


    ps_close, pic_name, /del_ps, font='lmroman', /v, /pdf
end















;MAIN-------------------------------------------------
;-----------------------------------------------------
;-----------------------------------------------------
;-----------------------------------------------------
pro avg_scores
;display verification scores for one or many forecasts, 
;average score can also be computed and displayed

pr_dt     = 5d      ;accumulation time for precip
img_dt    = 10d     ;temporal resolution of figures to be generated in miuntes
model_dt  = 5d
model_res = 10d
missing   = -9999.

nscales=7
scales=2.^(indgen(nscales)+1)*model_res
tresh_arr = [.1,1.,10.]
ntresh = n_elements(tresh_arr)
k_nbins=31
h_nbins=100


;experiments
t0 = julday(07, 02, 2014, 12, 00, 00)
tf = julday(07, 31, 2014, 12, 00, 00)
analy_dt   = 12d ;hours
count=0d
this_time = t0
while this_time le tf do begin
    ;time of nowcast is offet with analysis time
    caldat, this_time, mo, dd, yy, hh, mi, ss
    if count eq 0 then begin
        analy_hours_txt = string(yy,mo,dd,hh,format='(i04,3i02)')
    endif else begin
        analy_hours_txt = [analy_hours_txt,string(yy,mo,dd,hh,format='(i04,3i02)')]
    endelse
    count ++
    this_time = t0 + count*analy_dt/24d 
endwhile

;analy_hours_txt = ['2014070212', $
;                   '2014070300'];, $
;                   ;'2014070312']

exp_list = ['/local/raid/armadja/data/lhn_pr_p0_outputs/lhn_001/', $
            '/local/raid/armadja/data/lhn_pr_p0_outputs/lhn_002/'];, $
            ;'/local/raid/armadja/data/lhn_pr_p0_outputs/lhn_006/'];, $
            ;'/local/raid/armadja/data/lhn_pr_p0_outputs/now/']
desc     = ['control', $
            'lhn'];     $
            ;'init/2']
            ;'moist x2',$

prev_time = 3d      ;hours of forecast before analysis time
fcst_time = 9d      ;hours of forecast after  analysis time

;number of forecasts and experients to consider
nfcsts = n_elements(analy_hours_txt)
nexp   = n_elements(exp_list)

;get number of timesteps for ach forecast: nt
tt0 = JULDAY(07,02,2014,09,00,0)
ttf = JULDAY(07,02,2014,21,00,0)
time_list, tt0, ttf, img_dt, pr_dt, img_dt, $ ;in
           num_only=nt                      ;out

;structure to contain all scores and necessary info for averaging and displaying scores
ss={nt:0L, nexp:0L, nfcsts:0l, nscales:0L,        $
    missing:0d,                                   $
    fcst_time_txt: strarr(nfcsts),                $ 
    desc:          strarr(nexp),                  $ 
    code:          strarr(nexp),                  $
    time_jul:      dblarr(nt,nfcsts),             $
    fbias:         fltarr(nt,ntresh,nexp,nfcsts), $
    pod:           fltarr(nt,ntresh,nexp,nfcsts), $
    far:           fltarr(nt,ntresh,nexp,nfcsts), $
    sr:            fltarr(nt,ntresh,nexp,nfcsts), $
    csi:           fltarr(nt,ntresh,nexp,nfcsts), $
    gss:           fltarr(nt,ntresh,nexp,nfcsts), $
    lmin:          fltarr(nt,ntresh,nexp,nfcsts), $
    dpdt:          fltarr(nt,ntresh,nexp,nfcsts), $
    tot_corr:      fltarr(nt,ntresh,nexp,nfcsts), $
    corr_hf:       fltarr(nt,nscales,nexp,nfcsts),$
    corr_bf:       fltarr(nt,nscales,nexp,nfcsts),$
    corr_lf:       fltarr(nt,nscales,nexp,nfcsts),$
    hist_nlow:     fltarr(nt,nexp,nfcsts),        $
    hist_nhigh:    fltarr(nt,nexp,nfcsts),        $
    hist_count:    fltarr(nt,h_nbins,nexp,nfcsts),$   
    ps:            fltarr(nt,k_nbins,nexp,nfcsts),$
    rad_hist_nlow: fltarr(nt,nfcsts),             $
    rad_hist_nhigh:fltarr(nt,nfcsts),             $
    rad_hist_count:fltarr(nt,h_nbins,nfcsts),     $ 
    rad_ps:        fltarr(nt,k_nbins,nfcsts),     $
    hist_bin_val:  fltarr(h_nbins),               $
    hist_bounds:   fltarr(h_nbins+1),             $
    ps_karr:       fltarr(k_nbins),               $
    scales:        fltarr(nscales),               $
    k_nbins:0l,                                   $
    h_nbins:0l,                                   $
    ntresh:0,                                     $         
    tresh_arr:fltarr(ntresh)}

ss.nt        = nt
ss.nexp      = nexp
ss.nfcsts    = nfcsts
ss.nscales   = nscales
ss.missing   = missing
ss.desc      = desc
ss.code      = file_basename(exp_list)
ss.scales    = scales
ss.k_nbins   = k_nbins
ss.h_nbins   = h_nbins
ss.ntresh    = ntresh
ss.tresh_arr = tresh_arr

;for each forecast time
for ff=0, nfcsts-1 do begin
    ;get t0 and tf from forecast time
    this_time_txt = analy_hours_txt[ff]
    yy = long(strmid(this_time_txt,0,4))
    mo = long(strmid(this_time_txt,4,2))
    dd = long(strmid(this_time_txt,6,2))
    hh = long(strmid(this_time_txt,8,2))
    print, yy, mo, dd, hh
    this_time_jul = julday(mo, dd, yy, hh, 0, 0)
    t0 = this_time_jul-prev_time/24d
    tf = this_time_jul+fcst_time/24d
    ;get list of time where scores are desired
    print, 'time_list'
    time_list, t0, tf, model_dt, pr_dt, img_dt,$ ;in
             nearest=1, pr_nearest=0,          $ 
             time=time                           ;out
    print, 'done'

    ss.fcst_time_txt[ff] = this_time_txt
    ss.time_jul[*,ff] = time.t_jul

    ;for each experiments being examined
    for nn=0, nexp-1 do begin  ;for each experiment
        ;read score for this one forecast
        read_score, missing, this_time_txt, long(time.t_cmc),    $
                    fcst_name  = file_basename(exp_list[nn]),          $
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
                    dpdt       = dpdt       
                ;replicate dpdt for compatibility with other scores
                dpdt = rebin(dpdt,n_elements(dpdt),3)
                    ss.fbias[*,*,nn,ff] = fbias
                      ss.pod[*,*,nn,ff] = pod
                      ss.far[*,*,nn,ff] = far
                       ss.sr[*,*,nn,ff] = sr
                      ss.csi[*,*,nn,ff] = csi
                      ss.gss[*,*,nn,ff] = gss
                     ss.lmin[*,*,nn,ff] = lmin*model_res
                     ss.dpdt[*,*,nn,ff] = dpdt
                 ss.tot_corr[*,*,nn,ff] = tot_corr
                  ss.corr_hf[*,*,nn,ff] = corr_hf
                  ss.corr_bf[*,*,nn,ff] = corr_bf
                  ss.corr_lf[*,*,nn,ff] = corr_lf
                ss.hist_nlow[*,nn,ff]   = hist_nlow 
               ss.hist_nhigh[*,nn,ff]   = hist_nhigh
               ss.hist_count[*,*,nn,ff] = hist_count
                       ss.ps[*,*,nn,ff] = ps       
    endfor
    ;get result of power spectrum and histograms for radar data
    read_score, missing, this_time_txt, long(time.t_cmc),                          $
                ;fcst_name  = 'radar_stats',                                $
                fcst_name  = 'radar_12_avg_09_min',                                 $
                ;fcst_name  = 'radar_08_avg_05_min',                                $
                ;fcst_name  = 'radar_02_avg_01_min',                                $
                nscales=nscales, ntresh=ntresh, k_nbins=k_nbins, h_nbins=h_nbins,  $
                hist_bin_val = hist_bin_val ,                                      $
                hist_bounds  = hist_bounds  ,                                      $
                hist_nlow    = hist_nlow ,                                         $
                hist_nhigh   = hist_nhigh,                                         $
                hist_count   = hist_count,                                         $
                ps_karr      = ps_karr   ,                                         $
                ps_arr       = ps       
    ss.rad_hist_nlow[*,ff]    = hist_nlow 
    ss.rad_hist_nhigh[*,ff]   = hist_nhigh
    ss.rad_hist_count[*,*,ff] = hist_count
    ss.rad_ps[*,*,ff]         = ps       

    if ff eq 0 then begin
       ss.hist_bin_val = hist_bin_val
       ss.hist_bounds  = hist_bounds
       ss.ps_karr      = ps_karr    
    endif
endfor

;;plot images for individual forecasts
;for ff=0, nfcsts-1 do begin
;    this_time_txt = analy_hours_txt[ff]
;    scores_one_time, ss, this_time_txt
;endfor

scores_multi_fcsts, ss
    







end
