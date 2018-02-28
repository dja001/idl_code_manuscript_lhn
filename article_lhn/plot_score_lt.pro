pro plot_score_lt, nn, ss, time_arr_in, missing,     $ ;base info
                yorder, ps,                     $ ;figure info
                multi_forecast=ff,              $ ;index of forecasts when more than one are available
                score_name, yr, score,tresh_ind=tresh_ind,yticks=yticks,bot=bot      ;array containing score to plot
    ;plot a given score in a 2D chart
    ;the _lt version of this routine plots scores as a function of lead time


    ;chech that a treshold was provided for tresholded scores
    if score_name ne 'Correlation' then begin
        if n_elements(tresh_ind) eq 0 then message, 'tresh_ind mush be set for plot_score to run properly'
    endif

    ;figure position
    y0 = ps.y1 + yorder*(ps.rec_h)
    x0 = ps.x1 
    pos = [x0,y0,x0+2.*(ps.rec_w+ps.sp_w)-ps.sp_w,y0+.25*ps.rec_h]
    ;pos = [x0,y0,x0+2.*(ps.rec_w+ps.sp_w)-ps.sp_w,y0+.25*ps.rec_h]

    ;transform time_arr in hours starting at -3
    time_arr = (time_arr_in/60d) -3.

    ;t0 = min(time_arr, max=tf)
    t0 = -3d
    tf = 9d

    ;xticks for 48h forecasts
    xticks = 4
    xtickv = [0., 12, 24, 36, 48]
    xtickv = [-3., 0., 3, 6,   9]
    
    ;plot score     minutes &I
    if yorder eq 0 then begin
        dummy = label_date(date_format="%D%H%I") 
        xtitle = 'lead time [h]'
    endif else begin
        dummy = label_date(date_format=" ") 
        xtitle = ' '
    endelse

    if n_elements(yticks) eq 0 then yticks=3

    ;plot grey shading
    x0 = pos[0]
    y0 = pos[1]
    sixh = 6d
    x1 = x0 + (sixh)/(tf-t0)*(pos[2]-pos[0])
    y1 = pos[3]
    loadct, 0, /S
    tv, [242], x0, y0, xs=x1-x0, ys=y1-y0, /normal

    xr = [-3., 09]
    if ~keyword_set(bot) then xtickname=replicate(' ',20)

    loadct, 40, /s
    plot,[0], /nodata, /noerase,/normal,pos=pos,col=0,                    $
         xs=1, xr=xr, xtitle=xtitle, xtickv=xtickv, xticks=xticks, xminor=4, xtickname=xtickname, $
         ys=1, yr=yr,      ytitle=' ',    yticklen=.004, yticks=yticks,  yminor=1

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
    for nn=0,ss.nexp-1 do begin

        ;load color associated with this experiment
        tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]

        for ff=0, ss.nfcsts-1 do begin
            ;if score_name ne 'Correlation' then begin
                ;oplot, time_arr, score[*,tresh_ind,nn,ff], col=1, th=0.5
            ;endif else begin
                ;oplot, time_arr, score[*,nn,ff], col=1, th=0.5
            ;endelse
        endfor

        ;compute average
        ;if score_name ne 'Correlation' then begin
            count =    total(finite(reform(score[*,tresh_ind,nn,*])),2)
            avg_data = total(       reform(score[*,tresh_ind,nn,*]), 2,/nan)/count
        ;endif else begin
        ;   count =    total(finite(reform(score[*,nn,*])),2)
        ;   avg_data = total(       reform(score[*,nn,*]), 2,/nan)/count
        ;endelse
        oplot, time_arr, avg_data, col=1, th=6.5
        print, score_name
        maxmin, avg_data, missing=-9999.
    endfor

    ;horizontal line at zero
    loadct, 0, /s
    if yr[0] lt 0. then oplot, [t0,tf], [0.,0.], col=150, TH=1.
     
    print, ''
    print, ''



end
