pro plot_hist, ii, nn, exp_struc, missing,  $ ;base info
               yorder, ps                        ;figure info
    ;plot histogram of each experiment and radar

    if exp_struc.ndom gt 1 then message, 'not compatible with more than one domain'
    dd=0
    nexp = exp_struc.nexp[dd]

    ;figure position
    y0 = ps.y1 + yorder*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 + 1.333*(ps.rec_w + ps.sp_w)
    pos = [x0,y0,x0+.5*ps.rec_w,y0+.45*ps.rec_h]
    check_pos, pos

    nbins = exp_struc.h_nbins
    bins  = findgen(nbins)+1
    xtickv =  findgen(nbins)+1
    xtickname = strarr(nbins/3)
    ;for ss=0, nbins-1,3 do xtickname[nbins-1-ss/3] = strtrim(string(bins[ss/3],format='(i6)'))

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
    loadct, 0, /s
    plot, [0], /nodata, /noerase, /normal, pos=pos, col=0,  $
          xs=1, xrange=[.1,20.], xtitle='precip rate [mm/h]', xlog=1, $;xtickv=xtickv, xticks=30,xminor=1, xtickname=xtickname,  $
          ys=1, yrange=[0.,150], ytitle='count', yticklen=.004, yminor=1;, yticks=4., ylog=0
    ;radar histogram
    oplot, exp_struc.hist_bin_val, reform(exp_struc.rad_hist_count[ii,*]), col=0, thick=4.5;, psym=8, symsize=.3

    for nn=0, nexp-1 do begin
        ;load color associated with this experiment
        tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
        ;plot data
        count_arr = reform(exp_struc.hist_count[ii,*,nn])
        ;normalize distribution
        ;count_arr /= max(count_arr)
        oplot, exp_struc.hist_bin_val, count_arr, col=1, thick=4.5;, psym=8, symsize=.3
        ;color legend
        if nn eq 0 then xyouts, pos[2]+.1*ps.sp_w, y0+nn*.07*ps.rec_h,'control',col=1,/normal, charsize=1.7
        if nn eq 1 then xyouts, pos[2]+.1*ps.sp_w, y0+nn*.07*ps.rec_h,'lhn',col=1,/normal, charsize=1.7
        if nn eq 1 then begin
            loadct, 0, /s
            xyouts, pos[2]+.1*ps.sp_w, y0+2.*.07*ps.rec_h,'radar',col=0,/normal, charsize=1.7
        endif
        ;xyouts, pos[2]+.1*ps.sp_w, y0+nn*.07*ps.rec_h,exp_struc.code[nn,dd],col=1,/normal, charsize=1.7
    endfor
end
