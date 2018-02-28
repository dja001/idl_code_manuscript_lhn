pro rad_power
;plot precip power spectra for model forecasts and a number of radar interpolation method

t0 = JULDAY(07,08,2014,09,10,0)
tf = JULDAY(07,09,2014,23,00,0)
img_dt = 10d
pr_dt =5d

xx0 = 600
xxf = 900
yy0 = 200
yyf = 500
pow_nx = xxf-xx0+1
pow_ny = yyf-yy0+1
print, 'spec x dim',pow_nx
print, 'spec y dim',pow_ny

missing = -9999.

mod_data_dir='/local/raid/armadja/data/lhn_test_outputs/raa/'
r_dir_arr = ['/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v5_kdll_12_avg/',$
             '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v5_kdll_12_avg_15_min/'];,$
            ; '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v4_kdll_12_avg/',$
            ; '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v4_kdll_20_avg/',$
            ; '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v4_kdll_40_avg/']
nr = n_elements(r_dir_arr)

;get number of images to generate
time_list, t0, tf, img_dt, pr_dt, img_dt, $ ;in
               time=time

;time for PR accum computation
match_time, time.pr_cmc,     mod_data_dir, 'PR', $
            file_pr,         avail_time_arr=avail_time_arr_pr, avail_files=avail_files_pr
match_time, time.pr_mdt_cmc,     mod_data_dir, 'PR', $
            file_pr_mdt,     avail_time_arr=avail_time_arr_pr, avail_files=avail_files_pr, reuse=1

;set up image coords setup 
sq_w = 6.
sq_h = 1.*sq_w
sp_w = 2.
pic_h = 17.
pic_w = 2.+(nr+1)*(sq_w+sp_w)+3.6+(sq_w+sp_w)
pal_sp = .3/pic_w
pal_w = .25/pic_w
rec_w = sq_w/pic_w
rec_h = sq_h/pic_h
sp_w = sp_w/pic_w
sp_h = 2.5/pic_h
x1 = 2.2/pic_w 
x2 = x1 + rec_w + sp_w
x3 = x2 + rec_w + sp_w +3.6/pic_w
y1 = 1.5/pic_h
y2 = y1 + rec_h + sp_h


legs, range=[0.,60], color_arr='b_w', over_under='extend', $
      mapping=mapping_ref

base = 100.
range = [base/64.,base/32.,base/16.,base/8.,base/4.,base/2.,base]
legs, range=range, $
      color_arr=['brown','blue','green','orange','red','pink'], $
      excep_val=[missing,0.], excep_col=['grey_230','white'], excep_tol=[1e-3,1e-2], $
      over_high='extend', under_low='white',   $
      mapping=mapping_rr

pic_dir = '~/documents/ps/rad_power/'
if ~file_test(pic_dir) then file_mkdir, pic_dir

k_nbins = 21
max_pow = 1e-2
min_pow = 1e-8

vide = replicate(' ', 30)

;logarithmic color scale
ncol=5
range = 10.^(-reverse(findgen(ncol+1)))
col_num = reverse(findgen(ncol+1)/(ncol)*255.)
col_arr = bytarr(3,2,ncol)
for nn=0, ncol-1 do col_arr[*,*,nn] = [ [replicate(col_num[nn+1],3)] , [replicate(col_num[nn],3)] ]
legs, range=[min_pow,max_pow], color_arr=[[040,040,040],[255,255,255]], over_high='black', under_low='white', $
      excep_val=[missing], excep_col=['blue'], $
      mapping=mapping_bw, map_arr='log'


;image grid for pow spectrum
minw = 1.    ;in fact, pt 0,0 is the zero wave number -> the average
maxw = 300    ;maximum wavenumber (inclusively) to display 
nx = 800
ny = 800
logscale=1
if logscale eq 0 then begin
    plusone=1
    xmin=-.5+plusone
    ymin=-.5+plusone
    xmax=maxw+.5+plusone
    ymax=maxw+.5+plusone
    xx = floor(    rebin(       findgen(nx)/(nx)*(    (xmax)-    (xmin)) +     (xmin),   nx,ny)+.5-plusone)   ;+.5 for matching IDL indexes
    yy = floor(    rebin(rotate(findgen(ny)/(ny)*(    (ymax)-    (ymin)) +     (ymin),1),nx,ny)+.5-plusone)
endif else begin
    plusone=1
    xmin=-.5+plusone
    ymin=-.5+plusone
    xmax=maxw+.5+plusone
    ymax=maxw+.5+plusone
    xx = floor(exp(rebin(       findgen(nx)/(nx)*(alog(xmax)-alog(xmin)) + alog(xmin),   nx,ny))+.5-plusone)   
    yy = floor(exp(rebin(rotate(findgen(ny)/(ny)*(alog(ymax)-alog(ymin)) + alog(ymin),1),nx,ny))+.5-plusone)
    tickv = [1.,10,100]+plusone
    tick_str = strtrim(string(tickv-plusone,format='(i3)'),1)

    xticks = n_elements(tickv)-1
    yticks = n_elements(tickv)-1
endelse
img_ind = yy*pow_nx + xx
img_good = where(xx lt pow_nx and $
                 yy lt pow_ny, ngood, complement=img_bad, ncomplement=nbad)
                       
;initialize accumulation structure
;call to pow_avg just to get k_arr and the number of elements is contains
pow_acc = {rdps_avg:fltarr(k_nbins,nr,time.nt), rdps_high:fltarr(k_nbins,nr,time.nt),rdps_low:fltarr(k_nbins,nr,time.nt),$
            rad_avg:fltarr(k_nbins,nr,time.nt),  rad_high:fltarr(k_nbins,nr,time.nt), rad_low:fltarr(k_nbins,nr,time.nt) }


for tt=0,time.nt-1 do begin

    caldat, time.t_jul[tt], month, day, year, hour, minute, second

    ;get model data
    get_gem_data, file_pr[tt],     var_name='PR', values=pr_t,   cmc_timestamp=time.pr_cmc[tt]
    get_gem_data, file_pr_mdt[tt], var_name='PR', values=pr_mdt, cmc_timestamp=time.pr_mdt_cmc[tt]
    mod_mmh_raw = (pr_t - pr_mdt)*1e3*60./pr_dt    ;conversion to mm/hr
    
    ;plot results for this time
    pic_name = pic_dir+'rad_to_std'+time.t_str[tt]+'.ps'
    ps_start, pic_name, pic_w, pic_h, /white
    !p.charsize=1.2


    ;for each radar data type
    for rr=0, nr-1 do begin

        ;get type and radius from directory name
        type   = strmid(file_basename(r_dir_arr[rr]),21,3)
        radius = strmid(file_basename(r_dir_arr[rr]),18,2)

        ;read radar data
        r_file = r_dir_arr[rr]+string(year, month, day, hour,minute,format='(i4,3i02,"_",i02,".fst")')
        get_gem_data, r_file, var_name='RDBR', values=rad_ref, cmc_timestamp=time.pr_cmc[tt]
        if tt eq 0 then begin
        endif
        get_gem_data, r_file, var_name='RDQI', values=rad_qi , cmc_timestamp=time.pr_cmc[tt]
        aa = where(rad_qi ne 0, naa,complement=bb, ncomplement=nbb)
        rad_mmh = fltarr(size(rad_ref,/dim))
        if naa ne 0 then begin
            rad_mmh[aa] = 10.0^(rad_ref[aa]/16.) / 27.424818   
        endif 

        ;compute model power spectrum   ;redo for each radar product for fair comparison with nodata
        mod_mmh = mod_mmh_raw
        if nbb ne 0 then mod_mmh[bb] = 0.
        dct, mod_mmh[xx0:xxf,yy0:yyf], mod_spec
        maxmin,mod_mmh[xx0:xxf,yy0:yyf], name='Model spec input'
        maxmin,mod_spec, name='Model spec output'

        if nbb ne 0 then rad_mmh[bb] = 0.
        dct, rad_mmh[xx0:xxf,yy0:yyf], rad_spec
        maxmin,rad_mmh[xx0:xxf,yy0:yyf], name='rad spec'
        maxmin,rad_spec, name='rad spec output'

        ;from 2D to 1D power spectrum
        pow_avg, mod_spec, k_nbins, 1., pow_nx, karr, mod_pow_1d
        pow_acc.rdps_avg[*,rr,tt] = mod_pow_1d
        pow_avg, rad_spec, k_nbins, 1., pow_nx, karr, rad_pow_1d, bounds=bounds
         pow_acc.rad_avg[*,rr,tt] = rad_pow_1d

        if rr eq 0 then begin
           ;model spectrum
           x0 = x1
           y0 = y1
           pos = [x0,y0,x0+rec_w,y0+rec_h]
           img_spec = mod_spec[img_ind]
           if nbad ne 0 then img_spec[img_bad] = missing
           legs, data=img_spec, mapping=mapping_bw,  tv_out=pos
           xyouts, x0+.1*rec_w, y0+1.02*rec_h, 'Model spectrum', /normal, Charsize = 2.
           plot, [0],[0], /nodata, /noerase, /normal, pos=pos, $ 
                 xr=[xmin,xmax], xs=1, xlog=logscale, xticks=xticks, xtickv=tickv, xtitle='wavenumber', xtickname=tick_str, $
                 yr=[ymin,ymax], ys=1, ylog=logscale, yticks=yticks, ytickv=tickv, ytitle='wavenumber', ytickname=tick_str
           loadct, 39, /s
           npts = 1000
           ;for nn=0, n_elements(bounds)-1 do begin
           ;    ;conversion to image space
           ;    ra = replicate(bounds[nn], npts)
           ;    az = findgen(npts)/(npts-1)*2.*!pi
           ;    yy = ra*sin(az)+plusone
           ;    xx = ra*cos(az)+plusone
           ;    oplot,xx,yy, col=90, thick=2.5
           ;endfor

           ;model data
           y0 = y2
           pos = [x0,y0,x0+rec_w,y0+rec_h]
           mod_mmh[bb] = missing
           legs, data=mod_mmh[xx0:xxf,yy0:yyf], mapping=mapping_rr,  tv_out=pos
           loadct, 40, /s
           xyouts, x0+.0*rec_w, y0+1.02*rec_h, 'Model precip rate', /normal, Charsize = 2., col=255
           plot, [0], /nodata, /noerase, /normal, pos=pos, $
                 xr=[0,pow_nx-1], xs=1, xminor=1, xtitle='x gridpts', xtickinterval=100, $
                 yr=[0,pow_ny-1], ys=1, yminor=1, ytitle='y gridpts', ytickinterval=100, col=255
        endif

        ;r1 spectrum
        x0 = x1 + (rec_w+sp_w)*(rr+1)
        y0 = y1
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        img_spec = rad_spec[img_ind]
        if nbad ne 0 then img_spec[img_bad] = missing
        legs, data=img_spec, mapping=mapping_bw,  tv_out=pos
        xyouts, x0+.1*rec_w, y0+1.02*rec_h, 'Radar spectrum', /normal, Charsize = 2.
        plot, [0],[0], /nodata, /noerase, /normal, pos=pos, $ 
              xr=[xmin,xmax], xs=1, xlog=1, xticks=2, xtickv=[2.,11,101], xtitle='wavenumber', xtickname=['1','10','100'],  $
              yr=[ymin,ymax], ys=1, ylog=1, yticks=2, ytickv=[2.,11,101], ytitle=' ',          ytickname=['1','10','100']
        loadct, 40, /s
        npts = 1000
        ;for nn=0, n_elements(bounds)-1 do begin
        ;    ;conversion to image space
        ;    ra = replicate(bounds[nn], npts)
        ;    az = findgen(npts)/(npts-1)*2.*!pi
        ;    yy = ra*sin(az)+plusone
        ;    xx = ra*cos(az)+plusone
        ;    oplot,xx,yy, col=90, thick=2.5
        ;endfor

        ;r1 data
        y0 = y2
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        rad_mmh[bb] = missing
        legs, data=rad_mmh[xx0:xxf,yy0:yyf], mapping=mapping_rr,  tv_out=pos
        loadct, 40, /s
        xyouts, x0+.0*rec_w, y0+1.02*rec_h, 'Radar precip rate', /normal, Charsize = 2., col=255
        plot, [0], /nodata, /noerase, /normal, pos=pos, $
              xr=[0,pow_nx-1], xs=1, xminor=1, xtitle='x gridpts', xtickinterval=100, $
              yr=[0,pow_ny-1], ys=1, yminor=1, ytitle=' ', ytickinterval=100, col=255

    endfor
    ;plot spectrum palette
    x0 = x0 + rec_w + pal_sp
    y0 = y1
    pos = [x0,y0,x0+pal_w,y0+rec_h]
    legs, mapping=mapping_bw, palette=pos, units='power', yticks=6
    ;plot rain rates palette
    y0 = y2
    pos = [x0,y0,x0+pal_w,y0+rec_h]
    legs, mapping=mapping_rr, palette=pos, pal_prop='equal',units='mm/h', ytickformat='(f6.1)'


    ;plot power spectrum
    x0 = x1 + (rec_w+sp_w)*(nr+1)+3.6/pic_w
    y0 = y1
    pos = [x0,y0,x0+rec_w,y0+rec_h]
    print, pos
    loadct, 40, /s
    plot, [0,1],[0,1],pos=pos, /nodata, /noerase, xstyle=1, ystyle=1, $
        title='2d spectral density', $
        xr = [minw, maxw], xtitle=['wavenumber'], /xlog, $
        yr = [min_pow, max_pow], ytitle=['normalized power'], yminor=1, yticklen=.01, /ylog, col=0

    loadct, 0, /s
    ;oplot, karr, karr^(-1.), col=220
    oplot, karr, karr^(-2.), col=230
    xyouts,20,3e-3,'k!u-2', /data
    oplot, karr, karr^(-3.), col=230
    xyouts,7,3e-3,'k!u-3', /data
    ;loadct, 40, /s
    ;oplot, karr, karr^(-5./3), col=120

    loadct, 40, /s
    ;model
    oplot, karr, pow_acc.rdps_avg[*,0,tt], psym=8, col=93, symsize=.5
    oplot, karr, pow_acc.rdps_avg[*,0,tt], col=93, th=.8
    xyouts, pos[0]+1./pic_w, pos[1]+.2*rec_h, 'RDPS', col=93, charsize=2., /normal

    ;radar
    minc = 180
    maxc = 230
    for rr=0.,nr-1 do begin
        if nr ne 1 then this_c = (rr/(nr-1))*(maxc-minc)+minc else this_c = 210
        oplot, karr, pow_acc.rad_avg[*,rr,tt], psym=8, col=this_c, symsize=.5
        oplot, karr, pow_acc.rad_avg[*,rr,tt], col=this_c, th=.8

        ;;averages
        ;loadct, 0, /s
        ;if tt gt 0 then begin
        ;    avg_rad = total(reform(pow_acc.rad_avg[*,rr,0:tt]),2)/(tt+1.)
        ;    oplot, karr, avg_rad, psym=8, col=200, symsize=.5
        ;    oplot, karr, avg_rad, col=200, th=.8
        ;    print, 'avg rad:'
        ;    print, avg_rad
        ;endif
    endfor
    xyouts, pos[0]+1./pic_w, pos[1]+.1*rec_h, 'radar', col=210, charsize=2., /normal

    ;;average model
    ;loadct, 0, /s
    ;if tt gt 0 then begin
    ;    avg_mod = total(reform(pow_acc.rdps_avg[*,0,0:tt]),2)/(tt+1.)
    ;    oplot, karr, avg_mod, psym=8, col=100, symsize=.5
    ;    oplot, karr, avg_mod, col=100, th=.8
    ;    print, 'avg mod:'
    ;    print, avg_mod
    ;endif


    print, 'karr'
    print, karr
    print, 'rad_pow:'
    print, rad_pow_1d
    print, 'mod_pow:'
    print, mod_pow_1d


    ps_close, pic_name, /del_ps, font='lmroman', /v, /gif, density=400, geometry='50%', num_cpus=20
    ;ps_close, pic_name, /del_ps, font='lmroman', /v, /pdf
endfor


;image containing only average spectra

;set up image coords setup 
sq_w = 6.
sq_h = 1.*sq_w
sp_w = 2.
pic_h = 10.
pic_w = 10.
rec_w = sq_w/pic_w
rec_h = sq_h/pic_h
sp_w = sp_w/pic_w
sp_h = 2.5/pic_h
x1 = 2.2/pic_w 
y1 = 1.5/pic_h
pic_name = pic_dir+'rad_to_std_avg.ps'
ps_start, pic_name, pic_w, pic_h, /white
!p.charsize=1.0

    ;plot power spectrum
    x0 = x1 
    y0 = y1
    pos = [x0,y0,x0+rec_w,y0+rec_h]
    print, pos
    loadct, 40, /s
    plot, [0,1],[0,1],pos=pos, /nodata, /noerase, xstyle=1, ystyle=1, $
        title='2d spectral density', $
        xr = [minw, maxw], xtitle=['wavenumber'], /xlog, $
        yr = [min_pow, max_pow], ytitle=['normalized power'], yminor=1, yticklen=.01, /ylog, col=0
    loadct, 40, /s
    ;radar
    minc = 180
    maxc = 230
    for rr=0.,nr-1 do begin
        avg_rad = total(reform(pow_acc.rad_avg[*,rr,*]),2)/(time.nt)
        this_c = (rr/(nr-1))*(maxc-minc)+minc
        oplot, karr, avg_rad, psym=8, col=this_c, symsize=.5
        oplot, karr, avg_rad, col=this_c, th=.8
        print, 'avg rad:'
        print, avg_rad
    endfor
    xyouts, pos[0]+1./pic_w, pos[1]+.1*rec_h, 'radar', col=210, charsize=2., /normal

    ;average model
    minc = 43
    maxc = 93
    for rr=0.,nr-1 do begin
        avg_mod = total(reform(pow_acc.rdps_avg[*,rr,*]),2)/(time.nt)
        this_c = (rr/(nr-1))*(maxc-minc)+minc
        oplot, karr, avg_mod, psym=8, col=this_c, symsize=.5
        oplot, karr, avg_mod, col=this_c, th=.8
        print, 'avg mod:'
        print, avg_mod
    endfor
    xyouts, pos[0]+1./pic_w, pos[1]+.3*rec_h, 'RDPS', col=93, charsize=2., /normal


ps_close, pic_name, /del_ps, font='lmroman', /v, /pdf




end
