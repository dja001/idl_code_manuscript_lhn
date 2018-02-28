pro plot_score, ii, nn, exp_struc, missing,     $ ;base info
                yorder, ps,                     $ ;figure info
                multi_forecast=ff,              $ ;index of forecasts when more than one are available
                rad_cov=rad_areal_cov,          $ ;radar areal coverage of precip
                score_name, yr, score,tresh_ind=tresh_ind,yticks=yticks      ;array containing score to plot
    ;plot a given score in a 2D chart

    if tag_exist(exp_struc,'ndom') ne 0 then begin
        if exp_struc.ndom gt 1 then message, 'not compatible with more than one domain'
    endif
    dd=0
    nexp = exp_struc.nexp[dd]

    ;chech that a treshold was provided for tresholded scores
    if n_elements(tresh_ind) eq 0 then message, 'tresh_ind mush be set for plot_score to run properly'

    ;figure position
    y0 = ps.y1 + yorder*(ps.rec_h)
    x0 = ps.x1 
    pos = [x0,y0,x0+nexp*(ps.rec_w+ps.sp_w)-ps.sp_w,y0+.25*ps.rec_h]
    ;pos = [x0,y0,x0+2.*(ps.rec_w+ps.sp_w)-ps.sp_w,y0+.25*ps.rec_h]

    if n_elements(ff) ne 0 then begin
        time_arr = exp_struc.time_jul[*,ff]
    endif else begin
        time_arr = exp_struc.time_jul
    endelse
    t0 = min(time_arr, max=tf)
    ;last_time_to_display = exp_struc.time_jul[ii]   ;time displayed on radar image
    last_time_to_display = tf                       ;entire period
    aa = where(time_arr le last_time_to_display, count)
    if count le 0 then message, 'something wrong going on here'
    time_arr = time_arr[aa]

    ;;xticks for a few hours
    ;;find first integer hour at or after t0
    ;caldat, t0, mo,day,yy,hh,mi,ss
    ;add = 0.
    ;xt0 = t0 + add/1440.
    ;while mi ne 0 do begin
    ;    add ++
    ;    xt0 = t0 + add/1440.
    ;    CALDAT, xt0, mo,dd,yy,hh,mi,ss
    ;endwhile
    ;;make values for xticks
    ;nhours = (tf - t0)*24.
    ;xticks = nhours
    ;xtickinterval = 1./24   ;interval between tick marks
    ;xtickv = dindgen(xticks+1)*xtickinterval + xt0

    ;xticks for 48h forecasts
    xticks = 4
    xtickinterval = 1./2   ;interval between tick marks
    xt0 = t0 + 3d/24d
    xtickv = dindgen(xticks+1)*xtickinterval + xt0
    
    ;plot score     minutes &I
    if yorder eq 0 then begin
        dummy = label_date(date_format="%D%H%I") 
        xtitle = 'time [h]'
    endif else begin
        dummy = label_date(date_format=" ") 
        xtitle = ' '
    endelse

    if n_elements(yticks) eq 0 then yticks=3

    ;plot grey shading
    x0 = pos[0]
    y0 = pos[1]
    sixh = (6d/24d)
    x1 = x0 + (sixh)/(tf-t0)*(pos[2]-pos[0])
    y1 = pos[3]
    loadct, 0, /S
    tv, [242], x0, y0, xs=x1-x0, ys=y1-y0, /normal

    loadct, 40, /s
    plot,[0], /nodata, /noerase,/normal,pos=pos,col=0,                    $
         xs=1, xr=[t0,tf], xtitle=xtitle, xtickformat='label_date', xtickv=xtickv, xticks=xticks, xminor=4,$
         ys=1, yr=yr,      ytitle=' ',    yticklen=.004,                           yticks=yticks,  yminor=1

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


    ;title
    xyouts, pos[0]+.1*ps.sp_w, pos[1]+.01*ps.rec_h,score_name,col=0,/normal, charsize=2.5
    for nn=0,nexp-1 do begin

        ;load color associated with this experiment
        tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]

        ;color legend
        ;xyouts, pos[2]+.1*ps.sp_w, y0+nn*.07*ps.rec_h,exp_struc.code[nn,dd],col=1,/normal, charsize=1.7
        
        if tresh_ind ne missing then begin
            ;this is a tresholded score
            if n_elements(ff) ne 0 then begin
                score_arr = score[aa,tresh_ind,nn,ff]
            endif else begin
                score_arr = score[aa,tresh_ind,nn]
            endelse
            bb = where(score_arr lt -100. or score_arr gt 500.,cbb)
            if cbb ne 0 then score_arr[bb] = !values.f_nan
            oplot, time_arr, score_arr, col=1, th=6.5
            print, score_name, nn, exp_struc.tresh_arr[tresh_ind], max(score_arr,/nan),min(score_arr,/nan)

            ;plot radar qualtities if provided
            if n_elements(rad_areal_cov) ne 0 then begin
                loadct, 0, /s
                oplot, time_arr, rad_areal_cov, col=0, th=6.5
            endif
        endif else begin
            ;this is not a tresholded score
            if n_elements(ff) ne 0 then begin
                score_arr = score[aa,nn,ff]
            endif else begin
                score_arr = score[aa,nn]
            endelse
            bb = where(score_arr lt -100. or score_arr gt 500.,cbb)
            if cbb ne 0 then score_arr[bb] = !values.f_nan
            oplot, time_arr, score_arr, col=1, th=6.5
            print, score_name, nn, max(score_arr,/nan),min(score_arr,/nan)
        endelse
    endfor

    ;horizontal line at zero
    loadct, 0, /s
    if yr[0] lt 0. then oplot, [t0,tf], [0.,0.], col=150, TH=1.

    ;vertical line at current 
    if ii ne missing then begin
        oplot, [exp_struc.time_jul[ii],exp_struc.time_jul[ii]], yr, col=150, TH=3.
    endif



end
