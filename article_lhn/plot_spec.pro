pro plot_spec, ii, nn, exp_struc, missing,  $ ;base info
               yorder, ps                        ;figure info
    ;plot power spectrum of each experiment and radar

    if exp_struc.ndom gt 1 then message, 'not compatible with more than one domain'
    dd=0
    nexp = exp_struc.nexp[dd]

    ;figure position
    y0 = ps.y1 + yorder*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 + .666*(ps.rec_w + ps.sp_w)
    pos = [x0,y0,x0+.5*ps.rec_w,y0+.45*ps.rec_h]

    nscales = exp_struc.k_nbins
    scales  = findgen(nscales)+1
    xtickv =  findgen(nscales)+1
    xtickname = strarr(nscales)
    for ss=0, nscales-1 do xtickname[nscales-1-ss] = strtrim(string(scales[ss],format='(i6)'))

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
          xs=1, xrange=[1.,500], xtitle='Wavenumber', xlog=1, $;xtickv=xtickv, xticks=n_elements(xtickv)-1,xminor=1, xtickname=xtickname,  $
          ys=1, yrange=[1e-9,1e-3], ytitle='power', yticklen=.004, yminor=1, ylog=1

    ;pow spec of radar
        oplot, exp_struc.ps_karr, exp_struc.rad_ps[ii,*], col=0, thick=6.5
    ;pow spec of each experiment
    for nn=0, nexp-1 do begin
        ;load color associated with this experiment
        tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
        ;plot data
        oplot, exp_struc.ps_karr, exp_struc.ps[ii,*,nn], col=1, thick=4.5
        ;color legend
        ;if nn eq 0 then xyouts, pos[2]+.1*ps.sp_w, y0+nn*.07*ps.rec_h,'control',col=1,/normal, charsize=1.7
        ;if nn eq 1 then xyouts, pos[2]+.1*ps.sp_w, y0+nn*.07*ps.rec_h,'lhn',col=1,/normal, charsize=1.7
        ;xyouts, pos[2]+.1*ps.sp_w, y0+nn*.07*ps.rec_h,exp_struc.code[nn,dd],col=1,/normal, charsize=1.7
    endfor
end
