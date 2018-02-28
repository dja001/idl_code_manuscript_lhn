pro radar_img, nn, str
;procedure making the image

;load libraries and data structure
;RESTORE, str_savename
;load rmn lib
LOAD_RMN_LIB
SPAWN,'. ssmuse-sh -d cmdn/201502/06/base'

pic_dir = '/local/raid/armadja/data/radar_nat_composites/figures/'
;pic_dir = '/fs/cetus/fs3/mrb/arma/armadja/data/idl_figures/'

;set up image coords setup 
    ;ratio = 0.5 ;to use with newb projection
    ;pic_h = 18.5
 ratio = .8
 ;pic_h = 26.  ;  .8 ratio   
 pic_h = 12.  ;  .8 ratio 1 row
pic_w = 25.    ;two panels
;pic_w = 17.5   ;one panel
pal_sp = .9/pic_w
pal_w = .25/pic_w
off = .1/pic_h
sq_sz = 9.
rec_w = sq_sz/pic_w
rec_h = ratio*sq_sz/pic_h
sp_w = 2.5/pic_w
sp_h = 2./pic_h
x1 = 1.5/pic_w 
x2 = x1 + rec_w + .4*sp_w
x3 = x2 + rec_w + .4*sp_w
x4 = x3 + rec_w + sp_w
x5 = x4 + rec_w + sp_w
y1 = 1.2/pic_h
y2 = y1 + rec_h+sp_h
y3 = y2 + rec_h+sp_h


;range for color pics
range = [0., 50]
;make mapping structures
;legs, range=range, n_col=6, over_high='extend', under_low='white', $
      ;excep_val=[str.missing,-4000.], excep_col=['grey_175','black'], excep_tol=[1e-3,1e-3], $
      ;mapping=mapping
legs, range=range, color_arr=['brown','blue','green','orange','red'], over_high='extend', under_low='white', dark_pos='high',$ 
;legs, range=range, color_arr=['blue'], over_high='extend', under_low='white', dark_pos='high',$ 
      excep_val=[str.missing,4000.], excep_col=[[169,222,255],[215,000,013]], excep_tol=[1e-3,1e-3], $
      mapping=mapping_db

;legs, range=[0.,60.], color_arr='b_w', excep_val=[str.missing,10.], excep_col=['brown','blue'], dark_pos='high', $
legs, range=[0.,1.], color_arr=['red','brown','purple','blue','green'], excep_val=[str.missing], excep_col=['brown'], $
      over_under='exact',mapping=mapping_bw

legs, range=[0.,1.], color_arr=[[0,0,0],[255,255,255]], $
      excep_val=str.missing,excep_col='white', $
      mapping=mapping_cf

;legs, range=[0.,20.], color_arr=['blue'], excep_val=[str.missing], excep_col=['brown'], $
      ;over_under='exact',mapping=mapping_rr

base = 20.
range = [base/64.,base/32.,base/16.,base/8.,base/4.,base/2.,base]
legs, range=range, $
      color_arr=['brown','blue','green','orange','red','pink'], $
      excep_val=[str.missing,0.], excep_col=['grey_230','white'], excep_tol=[1e-3,1e-2], $
      over_high='extend', under_low='white',   $
      mapping=mapping_rr


;figure name
caldat, str.time_arr[nn], month,day,year,hour,minute,sec
;;only images for hours
;if (minute mod tres) ne 0 then continue
date_str = string([year,month,day,hour,minute], format='(i4,4i02)')
pic_name = pic_dir+date_str+'_compare.ps'

;make jhuapl time string
print, 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
;ddmo = jd2date(str.time_arr[nn], form='w$ n$ ')        ;there appears to be a bug  in 2016 08 01 before 12:00
months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
ddmo = strmid(whatdayisit(str.time_arr[nn]),0,3)+' '+months[month-1]+' '
jhu_time_str = ddmo+string(day,hour,minute,sec,year,format='(i02," ",i02,":",i02,":",i02," ",i4)')
print, jhu_time_str
print, 'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB'

;radar V7 h5 file
day_dir    = string(year,month,day, format='(i4,"/",i02,"/",i02,"/")')
file_bname = string([year,month,day,hour,minute], format='("qcomp_v7_",i4,2i02,"T",2i02,"Z.h5")')
h5_file = str.h5_data_dir+day_dir+file_bname

;10km radar on RDPS grid file
file_bname = string([year,month,day,hour,minute], format='(i4,3i02,"_",i02,".fst")')
fst_file = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_12_avg_09_min/'+day_dir+file_bname

;;radar v5 h5 file
;file_bname = string([year,month,day,hour,minute], format='("qcomp_v5_",i4,2i02,"T",2i02,"Z.h5")')
;h5_file_v5 = str.h5_data_dir+day_dir+file_bname


;;get cloud fraction field
;if str.files_cf_13[nn] eq 'not_avail' then begin
;    proj_cf_13 = replicate(-1.,size(str.cf13_proj_ind,/dim))
;endif else begin
;    get_gem_data, str.files_cf_13[nn],var_name='CF',values=cf_13
;    apply_proj, cf_13, str.cf13_proj_ind, proj_cf_13, missing=str.missing
;endelse
;if str.files_cf_15[nn] eq 'not_avail' then begin
;    proj_cf_15 = replicate(-1.,size(str.cf15_proj_ind,/dim))
;endif else begin
;    get_gem_data, str.files_cf_15[nn],var_name='CF',values=cf_15
;    apply_proj, cf_15, str.cf15_proj_ind, proj_cf_15, missing=str.missing
;endelse
;;make a composite cloud product
;comp_cf = replicate(str.missing,size(proj_cf_13,/dim))
;cf_13_true = proj_cf_13 ge 0.
;cf_15_true = proj_cf_15 ge 0.
;aa = where(cf_13_true and cf_15_true, naa)
;if naa ne 0 then begin
;    for kk=0,naa-1 do comp_cf[aa[kk]] = max([proj_cf_13[aa[kk]],proj_cf_15[aa[kk]]])
;endif 
;aa = where(cf_13_true and ~cf_15_true, naa)
;if naa ne 0 then comp_cf[aa] = proj_cf_13[aa] 
;aa = where(~cf_13_true and cf_15_true, naa)
;if naa ne 0 then comp_cf[aa] = proj_cf_15[aa] 
;legs, data=comp_cf, mapping=mapping_cf, img_out=cf_rgb


;;find and load nearest GPM data 
;gpm_data_dir = '/local/raid/armadja/data/GPM_precip/'
;gpm_files = file_search(gpm_data_dir+'*',count=n_gpm)
;if n_gpm eq 0 then message, 'no gpm files found'
;aa = sort(gpm_files)
;gpm_files = gpm_files[aa]
;gpm_file = 'not_avail'
;for cc=0, n_gpm-1 do begin
;    gpm_yy = strmid(file_basename(gpm_files[cc]),21,4)
;    gpm_mo = strmid(file_basename(gpm_files[cc]),25,2)
;    gpm_dd = strmid(file_basename(gpm_files[cc]),27,2)
;    gpm_mm = strmid(file_basename(gpm_files[cc]),46,4)
;    gpm_time = julday(gpm_mo, gpm_dd,gpm_yy,  0, gpm_mm,00)
;    if gpm_time ge str.time_arr[nn] then begin
;        if gpm_time-str.time_arr[nn] le .5d/24. then begin
;            gpm_file = gpm_files[cc]
;            break
;        endif
;    endif 
;endfor
;if gpm_file ne 'not_avail' then begin
;    nc_id = ncdf_open(gpm_file)
;    ncdf_varget, nc_id, 'precipitationCal', gpm_data
;    ncdf_close, nc_id
;    gpm_data = rotate(gpm_data,4)
;endif else begin
;    gpm_data = replicate(str.missing, 837,493)
;endelse






;make rgb map of day and night
day_night_rgb = bytarr(str.pnx,str.pny,3)
sunaltazi, jhu_time_str, str.dev_lon, str.dev_lat, azi, alt

aa = where(alt ge 0., naa,complement=bb,ncomplement=nbb)
;;dark
;day_rgb   = [255,179,000]
;night_rgb = [189,187,240]
;pale
day_rgb   = [255,255,225]
night_rgb = [225,225,255]
is_day   = bytarr(str.pnx,str.pny)
is_night = bytarr(str.pnx,str.pny)
if naa ne 0 then   is_day[aa] = 1.
if nbb ne 0 then is_night[bb] = 1.
for kk=0,2 do begin
    day_night_rgb[*,*,kk] = is_day*day_rgb[kk] + is_night*night_rgb[kk]
endfor

;plot image
ps_start, pic_name, pic_w, pic_h, charsize=1.2

    fracx = 0.05
    fracy = 1.15


    ;plot white bkg img
    loadct, 0, /s
    tv, [255], 0,0,xs=1,ys=1,/normal

    ;title, labels and stuff
    loadct,40,/s
    tit_date_str = string([year, month, day, hour, minute], format='(i4,"-",i02,"-",i02,"  ",i02,":",i02," UTC")')

    ;day night pic
    offset = 0.;-3./pic_w
    y0 = y1+1./pic_h
    pos = [x1+rec_w-1./pic_w+offset,y0+rec_h+1./pic_h,x2+1./pic_w+offset,y0+rec_h+2./pic_h]
    ;plot blended image
    loadct, 0, /s
    dn_rgb = fltarr(2,1,3)
    for kk=0,2 do dn_rgb[*,0,kk] = [day_rgb[kk],night_rgb[kk]]
    tv, dn_rgb, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3
    xyouts, pos[0]+.1/pic_w,   pos[1]+.4/pic_h,       "day",   charsize=1.4, /normal
    xyouts, pos[0]+(pos[2]-pos[0])/2.+.1/pic_w, pos[1]+.4/pic_h, "night", charsize=1.4, /normal
    plots, [pos[0],pos[2],pos[2],pos[0],pos[0]],[pos[1],pos[1],pos[3],pos[3],pos[1]], th=.5, /normal

    ;;label for no cloud echoes
    ;pos = [x1+rec_w-1./pic_w+offset,y0+rec_h+2./pic_h,x2+1./pic_w+offset,y0+rec_h+3./pic_h]
    ;tv, [[[201]],[[000]],[[013]]], pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3
    ;xyouts, pos[0]+.1/pic_w,   pos[1]+.4/pic_h,       "Radar > 0 dBZ w/o clouds",   charsize=.9, /normal



    ;;plot rhi-------------------------------
    ;show_arrow=1
    ;arrow_col=[000,134,000]
    ;radar_info, rinfo, id='CAWSO'
    ;;plot radar position
    ;;plots, [rinfo.lon], [rinfo.lat], psym=8, col=0, symsize=.3
    ;want_az = 240d
    ;xflip=0
    ;if want_az gt 180 and want_az lt 360 then xflip=1
    ;x0 = x1
    ;y0 = y3
    ;pos = [x0,y0,x0+2.*rec_w+sp_w,y0+rec_h]
    ;get_and_plot_h5_rhi, rinfo=rinfo, want_az=want_az, $
    ;mapping=mapping, jul_time=str.time_arr[nn], pos=pos, missing=str.missing, /topo, $
    ;zrange=[0.,10.], xflip=xflip, dataset='data2'


    ;10km radar on rdps grid image---------------------------------------------------------------------------
    get_gem_data, fst_file, var_name='RDPR', values=urp_mm_h
        ;urp_dbz = replicate(str.missing, size(urp_mm_h,/dim))
        ;aa = where(urp_mm_h gt 0., naa)
        ;if naa ne 0 then urp_dbz[aa] = 10.*alog10((200.*urp_mm_h[aa]^1.6)/10.)

    ;apply projection
    apply_proj, urp_mm_h,     str.proj_ind, proj_urp, missing=str.missing

    ;;flag echoes where no clouds present
    ;aa = where(proj_urp gt 0. and comp_cf lt 1e-3, naa)
    ;if naa ne 0 then proj_urp[aa] = 4000.
    
    x0 = x2
    y0 = y1
    pos = [x0,y0,x0+rec_w,y0+rec_h]
    legs, data=proj_urp, mapping=mapping_rr, img_out=urp_rgb

    ;;blend counds and day_night
    ;alpha = fltarr(str.pnx,str.pny)
    ;aa = where(comp_cf gt 1e-3, naa)
    ;if naa ne 0 then alpha[aa] = 1.
    ;img_mix, dn_cf_rgb, day_night_rgb, cf_rgb, alpha
        dn_cf_rgb = day_night_rgb
    ;add radar data
    alpha = fltarr(str.pnx,str.pny)
    aa = where(proj_urp gt 0., naa)
    if naa ne 0 then alpha[aa] = 1.
    ;aa = where(proj_urp eq str.missing , naa)
    ;if naa ne 0 then alpha[aa] = 0.
    img_mix, urp_blended_rgb, dn_cf_rgb, urp_rgb, alpha

    ;plot blended image
    loadct, 0, /s
    tv, urp_blended_rgb, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;title & legend
    ;xyouts, x0+fracx*rec_w,   y0+fracy*rec_h, "URP composites", charsize=1.8, /normal
    xyouts, x0+fracx*rec_w,   y0+fracy*rec_h, ' 10km composites', charsize=1.8, /normal
    ;xyouts, x0+.2*rec_w,   y0+rec_h+1./pic_h, tit_date_str, charsize=1.8, /normal
    ;xyouts, x0+ .45*rec_w, y0-.1 *rec_h, "[deg W]", charsize=.9, /normal
    ;xyouts, x0- .07*rec_w, y0+.45*rec_h, "[deg N]", charsize=.9, /normal, orientation=90.

    ;plot map details
    maps, pos=pos, /map, /grid, loc=str.loc
        ;color palette
        pos = [x0+rec_w+pal_sp,y0-off,x0+rec_w+pal_sp+pal_w,y0+rec_h+off]
        ;legs, mapping=mapping_db, palette=pos, units='dBZ'
        legs, mapping=mapping_rr, palette=pos, units='mm/hr', pal_prop='equal'
        ;pos = [x0+rec_w+3.*pal_sp+pal_w,y0-off,x0+rec_w+3.*pal_sp+2.*pal_w,y0+rec_h+off]
        ;legs, mapping=mapping_cf, palette=pos, units='Cloud fraction'




    ;;10km radar on rdps grid quality index---------------------------------------------------------------------------
    ;get_gem_data, str.li[nn], var_name='RDQI', values=urp_qi

    ;;apply projection
    ;apply_proj, urp_qi,     str.proj_ind, proj_urp, missing=str.missing

    ;;;flag echoes where no clouds present
    ;;aa = where(proj_urp gt 0. and comp_cf lt 1e-3, naa)
    ;;if naa ne 0 then proj_urp[aa] = 4000.
    ;
    ;x0 = x2
    ;y0 = y1
    ;pos = [x0,y0,x0+rec_w,y0+rec_h]
    ;legs, data=proj_urp, mapping=mapping_bw, img_out=urp_rgb

    ;;;blend counds and day_night
    ;;alpha = fltarr(str.pnx,str.pny)
    ;;aa = where(comp_cf gt 1e-3, naa)
    ;;if naa ne 0 then alpha[aa] = 1.
    ;;img_mix, dn_cf_rgb, day_night_rgb, cf_rgb, alpha
    ;    dn_cf_rgb = day_night_rgb
    ;;add radar data
    ;alpha = fltarr(str.pnx,str.pny)
    ;aa = where(proj_urp gt 0., naa)
    ;if naa ne 0 then alpha[aa] = 1.
    ;;aa = where(proj_urp eq str.missing , naa)
    ;;if naa ne 0 then alpha[aa] = 0.
    ;img_mix, urp_blended_rgb, dn_cf_rgb, urp_rgb, alpha

    ;;plot blended image
    ;loadct, 0, /s
    ;tv, urp_blended_rgb, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;;title & legend
    ;;xyouts, x0+fracx*rec_w,   y0+fracy*rec_h, "URP composites", charsize=1.8, /normal
    ;xyouts, x0+fracx*rec_w,   y0+fracy*rec_h, 'V6 10km composites', charsize=1.8, /normal
    ;;xyouts, x0+.2*rec_w,   y0+rec_h+1./pic_h, tit_date_str, charsize=1.8, /normal
    ;;xyouts, x0+ .45*rec_w, y0-.1 *rec_h, "[deg W]", charsize=.9, /normal
    ;;xyouts, x0- .07*rec_w, y0+.45*rec_h, "[deg N]", charsize=.9, /normal, orientation=90.

    ;;plot map details
    ;maps, pos=pos, /map, /grid, loc=str.loc
    ;    ;color palette
    ;    pos = [x0+rec_w+pal_sp,y0-off,x0+rec_w+pal_sp+pal_w,y0+rec_h+off]
    ;    legs, mapping=mapping_bw, palette=pos, units='unitless'
    ;    ;pos = [x0+rec_w+3.*pal_sp+pal_w,y0-off,x0+rec_w+3.*pal_sp+2.*pal_w,y0+rec_h+off]
    ;    ;legs, mapping=mapping_cf, palette=pos, units='Cloud fraction'


    ;;urp image---------------------------------------------------------------------------
    ;dum = str.li[nn]
    ;    ;;faking v6 files from urp names
    ;    ;aa = strpos(dum, 'v6')
    ;    ;v5_file = strmid(dum,0,aa)+'v5'+strmid(dum,aa+2,strlen(dum)-1-aa+2)
    ;    ;get_gem_data, str.li[nn], var_name='RDPR', values=urp_dbz
    ;get_gem_data, str.li[nn], var_name='RDBR', values=urp_dbz

    ;;apply projection
    ;apply_proj, urp_dbz,     str.proj_ind, proj_urp, missing=str.missing

    ;;;flag echoes where no clouds present
    ;;aa = where(proj_urp gt 0. and comp_cf lt 1e-3, naa)
    ;;if naa ne 0 then proj_urp[aa] = 4000.
    ;
    ;x0 = x1
    ;y0 = y1
    ;pos = [x0,y0,x0+rec_w,y0+rec_h]
    ;legs, data=proj_urp, mapping=mapping_db, img_out=urp_rgb

    ;;;blend counds and day_night
    ;;alpha = fltarr(str.pnx,str.pny)
    ;;aa = where(comp_cf gt 1e-3, naa)
    ;;if naa ne 0 then alpha[aa] = 1.
    ;;img_mix, dn_cf_rgb, day_night_rgb, cf_rgb, alpha
    ;    dn_cf_rgb = day_night_rgb
    ;;add radar data
    ;alpha = fltarr(str.pnx,str.pny)
    ;aa = where(proj_urp gt 0., naa)
    ;if naa ne 0 then alpha[aa] = 1.
    ;;aa = where(proj_urp eq str.missing , naa)
    ;;if naa ne 0 then alpha[aa] = 0.
    ;img_mix, urp_blended_rgb, dn_cf_rgb, urp_rgb, alpha

    ;;plot blended image
    ;loadct, 0, /s
    ;tv, urp_blended_rgb, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;;title & legend
    ;xyouts, x0+fracx*rec_w,   y0+fracy*rec_h, "URP composites", charsize=1.8, /normal
    ;;xyouts, x0+fracx*rec_w,   y0+fracy*rec_h, 'IMERG', charsize=1.8, /normal
    ;xyouts, x0+fracx*rec_w,      y0+rec_h+2./pic_h, tit_date_str, charsize=1.8, /normal
    ;;xyouts, x0+ .45*rec_w, y0-.1 *rec_h, "[deg W]", charsize=.9, /normal
    ;;xyouts, x0- .07*rec_w, y0+.45*rec_h, "[deg N]", charsize=.9, /normal, orientation=90.

    ;;plot map details
    ;maps, pos=pos, /map, /grid, loc=str.loc
    ;    ;;color palette
    ;    pos = [x0+rec_w+pal_sp,y0-off,x0+rec_w+pal_sp+pal_w,y0+rec_h+off]
    ;    legs, mapping=mapping_db, palette=pos, units='dBZ'
    ;    ;pos = [x0+rec_w+3.*pal_sp+pal_w,y0-off,x0+rec_w+3.*pal_sp+2.*pal_w,y0+rec_h+off]
    ;    ;legs, mapping=mapping_cf, palette=pos, units='Cloud fraction'
    ;    ;pos = [x0+rec_w+pal_sp,y0-off,x0+rec_w+pal_sp+pal_w,y0+rec_h+off]
    ;    ;legs, mapping=mapping_rr, palette=pos, units='mm/hr', pal_prop='equal'



    ;h5 image with processing---------------------------------------------------------------------------
    print, h5_file
    read_h5, file=h5_file, ref=h5_dbz, /qced, missing=str.missing, tot_qi=tot_qi

    ;transform to mm/hr
    h5_mmh = fltarr(size(h5_dbz,/dim))
    aa = where(h5_dbz ne str.missing, naa)
    if naa ne 0 then h5_mmh[aa] = 10d^(h5_dbz[aa]/16d) / 27.424818   ;precip rate in mm/hr

    ;apply projection
    ;apply_proj, h5_dbz,   str.h5_proj_ind, proj_h5, missing=str.missing
    apply_proj, h5_mmh,   str.h5_proj_ind, proj_h5, missing=str.missing
    apply_proj, tot_qi,   str.h5_proj_ind, proj_qi, missing=str.missing

    ;;flag echoes where no clouds present
    ;aa = where(proj_h5 gt 0. and comp_cf lt 1e-3, naa)
    ;if naa ne 0 then proj_h5[aa] = 4000.
    
    ;precip from h5_file
    x0 = x1
    y0 = y1
    pos = [x0,y0,x0+rec_w,y0+rec_h]
    legs, data=proj_h5, mapping=mapping_rr, img_out=h5_rgb

    ;;blend clouds and day_night
    ;alpha = fltarr(str.pnx,str.pny)
    ;aa = where(comp_cf gt 1e-3, naa)
    ;if naa ne 0 then alpha[aa] = 1.
    ;img_mix, dn_cf_rgb, day_night_rgb, cf_rgb, alpha
        dn_cf_rgb = day_night_rgb
    ;add radar data
    alpha = fltarr(str.pnx,str.pny)
    ;aa = where(proj_h5 gt 0.31, naa)
    ;if naa ne 0 then alpha[aa] = 1.
    help, proj_qi, alpha
    aa = where(proj_qi gt 0. , naa)
    if naa ne 0 then alpha[aa] = 1.
    img_mix, h5_blended_rgb, dn_cf_rgb, h5_rgb, alpha

    ;plot blended image
    loadct, 0, /s
    tv, h5_blended_rgb, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;title & legend
    offset = 1.5/pic_w
    xyouts, x0+fracx*rec_w+offset,   y0+fracy*rec_h, "QCed composites", charsize=1.8, /normal
    xyouts, x0+fracx*rec_w+1./pic_w,   y0+rec_h+2.8/pic_h, tit_date_str, charsize=1.4, /normal
    ;xyouts, x0+ .45*rec_w, y0-.2 *rec_h, "[deg W]", charsize=1.1, /normal

    ;plot map details
    maps, pos=pos, /map, /grid, loc=str.loc


    ;;h5 image WITHOUT processing---------------------------------------------------------------------------
    ;print, h5_file
    ;read_h5, file=h5_file, ref=h5_dbz, /nonqced, missing=str.missing, tot_qi=tot_qi

    ;;;transform to mm/hr
    ;;h5_mmh = fltarr(size(h5_dbz,/dim))
    ;;aa = where(h5_dbz ne str.missing, naa)
    ;;if naa ne 0 then h5_mmh[aa] = 10d^(h5_dbz[aa]/16d) / 27.424818   ;precip rate in mm/hr

    ;;apply projection
    ;apply_proj, tot_qi,   str.h5_proj_ind, proj_h5, missing=str.missing
    ;;apply_proj, h5_mmh,   str.h5_proj_ind, proj_h5, missing=str.missing

    ;;;flag echoes where no clouds present
    ;;aa = where(proj_h5 gt 0. and comp_cf lt 1e-3, naa)
    ;;if naa ne 0 then proj_h5[aa] = 4000.
    ;
    ;;precip from h5_file
    ;x0 = x2
    ;y0 = y1
    ;pos = [x0,y0,x0+rec_w,y0+rec_h]
    ;legs, data=proj_h5, mapping=mapping_cf, img_out=h5_rgb

    ;;;blend clouds and day_night
    ;;alpha = fltarr(str.pnx,str.pny)
    ;;aa = where(comp_cf gt 1e-3, naa)
    ;;if naa ne 0 then alpha[aa] = 1.
    ;;img_mix, dn_cf_rgb, day_night_rgb, cf_rgb, alpha
    ;    dn_cf_rgb = day_night_rgb
    ;;add radar data
    ;alpha = fltarr(str.pnx,str.pny)
    ;aa = where(proj_h5 gt 0.31, naa)
    ;if naa ne 0 then alpha[aa] = 1.
    ;aa = where(proj_h5 eq str.missing , naa)
    ;if naa ne 0 then alpha[aa] = 0.
    ;img_mix, h5_blended_rgb, dn_cf_rgb, h5_rgb, alpha

    ;;plot blended image
    ;loadct, 0, /s
    ;tv, h5_rgb, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;;title & legend
    ;offset = 0.;-2./pic_w
    ;;xyouts, x0+fracx*rec_w+offset,   y0+fracy*rec_h, "non QCed composites", charsize=1.8, /normal
    ;xyouts, x0+fracx*rec_w+offset,   y0+fracy*rec_h, "Quality index", charsize=1.8, /normal
    ;;xyouts, x0+.2*rec_w+offset,   y0+1.1*rec_h, tit_date_str, charsize=1.4, /normal
    ;;xyouts, x0+ .45*rec_w, y0-.2 *rec_h, "[deg W]", charsize=1.1, /normal

    ;;plot map details
    ;maps, pos=pos, /map, /grid, loc=str.loc

    ;    ;color palette
    ;    pos = [x0+rec_w+pal_sp,y0-off,x0+rec_w+pal_sp+pal_w,y0+rec_h+off]
    ;    ;legs, mapping=mapping_db, palette=pos, units='dBZ'
    ;    legs, mapping=mapping_cf, palette=pos, units='Quality index [unitless]'
    ;    ;legs, mapping=mapping_rr, palette=pos, units='mm/hr', pal_prop='equal'
    ;    ;pos = [x0+rec_w+3.*pal_sp+pal_w,y0-off,x0+rec_w+3.*pal_sp+2.*pal_w,y0+rec_h+off]
    ;    ;legs, mapping=mapping_cf, palette=pos, units='Cloud fraction'






    ;plot all radars
    radar_info, allr
    for ii=0, n_elements(allr)-1 do begin
        pp = convert_coord([allr[ii].lon,allr[ii].lat], /data, /to_normal)
        if pp[0] gt pos[0] and pp[0] lt pos[2] and pp[1] gt pos[1] and pp[1] lt pos[3] then begin
            plots, [allr[ii].lon], [allr[ii].lat], psym=8, col=0, symsize=.2
            xyouts, allr[ii].lon,   allr[ii].lat, allr[ii].id, charsize=.3
        endif
    endfor



    ;;plot rhi-------------------------------
    ;if keyword_set(show_arrow) then rhi_arrow, rinfo, 250d, want_az, col=arrow_col, th=5.

    ;    ;color palette
    ;    pos = [x0+rec_w+pal_sp,y0-off,x0+rec_w+pal_sp+pal_w,y0+rec_h+off]
    ;    legs, mapping=mapping, palette=pos, units='dBZ'
    ;    pos = [x0+rec_w+pal_sp,y0-off,x0+rec_w+pal_sp+pal_w,y0+rec_h+off]
    ;    legs, mapping=mapping_rr, palette=pos, units='mm/hr', pal_prop='equal'








ps_close, pic_name, /jpg, /del_ps, density=400, geometry='55%', font='lmroman', num_cpus=10, /verbose
;ps_close, pic_name, /gif, /del_ps, density=400, geometry='55%', font='lmroman', num_cpus=10, /verbose
;ps_close, pic_name, /pdf, /del_ps, font='lmroman', num_cpus=10, /verbose


end





pro anim_composite, serial=serial
;animation of different continental composites takes in standard files

    ;;--1--
    ;t0 = julday(07,07,2014,08,00,00)
    ;tf = julday(07,09,2014,00,00,00)

    ;t0 = julday(07,31,2016,00,00,00)
        t0 = julday(08,08,2016,00,00,00)
    ;tf = julday(09,03,2016,00,00,00)
        tf = julday(08,09,2016,00,00,00)

tres = 10       ;time resolution of movie in minutes
missing = -999.
;pic_dir       = '/users/dor/arma/dja/documents/ps/composite/jul_2014/'
;pic_dir_tmp   = '/users/dor/arma/dja/documents/ps/composite/tmp/'
;pic_dir_probs = '/users/dor/arma/dja/documents/ps/composite/probs/'

;--2--
loc='can_us_radars'
;loc='erie'
;loc='nat_enkf'
;loc='missouri'
;loc='newb'
;loc='gaspesia'
;loc='bc_alberta'
;loc='prairies'
;loc='south_quebec'
;loc='south_quebec_small'
    ;maps, loc=loc, /test, rec_w=12., rec_h=6. 
    ;stop
ind_savename = '~/documents/idl_sav_files/anim_composite_'+loc+'.sav'
h5_ind_savename = '~/documents/idl_sav_files/anim_composite_h5_'+loc+'.sav'
regen = 1


;--3--
;special='terrain_echoes_over_northern_BC'
;special='intensity_change_over_prairies'
;special='data_gap_sth_quebec'
;special='weird_echoes_southern_bc'
;special='storm_regeneration_over_mtl'

;clean dir of previous files
;SPAWN, 'rm -f '+pic_dir_tmp+'*'

;data_dir = '/cnfs/ops/production/radar/urp/NowCast/PRECIPET/RPN-STND/'
h5_data_dir = '/fs/cetus/fs3/mrb/armp/armpdbm/output/composites/'

    ;;;URP composites
    ;data_dir = '/local/raid/armadja/data/radar_nat_composites/reduced_rdata/'
    ;product = '*.stnd'
;10 km composites from H5 data
data_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_kdll_12_avg_09_min/'
product = '*.fst'

;;for URP composites
;li = file_search(data_dir+product, count=nf)
;if nf eq 0 then begin
;    message, 'no files found'
;endif
;time_arr = dblarr(nf)
;for i=0, nf-1 do begin
;    date_str = strmid(file_basename(li[i]), 0, 13)
;    year   = fix(strmid(date_str,0,4))
;    month  = fix(strmid(date_str,4,2))
;    day    = fix(strmid(date_str,6,2))
;    hour   = fix(strmid(date_str,8,2))
;    minute = fix(strmid(date_str,11,2))
;    time_arr[i] = julday(month,day,year,hour,minute,00)
;    ;;for debug
;    ;print, date_str
;    ;print, month, day, year, hour, minute
;    ;print, ' '
;endfor
;;sort in order of time
;ss = sort(time_arr)
;time_arr = time_arr[ss]
;li = li[ss]



;for h5 data
count = 1l
time_acc = t0
this_time = t0
while this_time le tf do begin
    this_time  = t0 + count*tres/1440. +1e-5
    time_acc   = [time_acc,  this_time]
    caldat, this_time, month, day, year, hour, minute, second
    count++
endwhile
li_acc = replicate('not_avail',count)




;get the lat/lon grid of urp radar composites
    sample_file = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_12_avg_09_min/2014/07/07/2014070700_00.fst'
    var = 'RDPR'
;sample_file = '/local/raid/armadja/data/radar_nat_composites/dearchived/2016070100_00ref_4km.stnd'
;var = 'RDBR'
call_gdll, sample_file, var, mat_lat, mat_lon
sz = size(mat_lat,/dim)
urp_nx = sz[0]
urp_ny = sz[1]

    ;;instead of URP make projection indices for GMP data
    ;sample_gpm_file = '/local/raid/armadja/data/GPM_precip/3B-HHR.MS.MRG.3IMERG.20140707-S060000-E062959.0360.V04A.HDF5.nc'
    ;nc_id = ncdf_open(sample_gpm_file)
    ;ncdf_varget, nc_id, 'lat', mat_lat
    ;ncdf_varget, nc_id, 'lon', mat_lon
    ;ncdf_close, nc_id
    ;;make data 2D
    ;urp_nx = n_elements(mat_lon)
    ;urp_ny = n_elements(mat_lat)
    ;mat_lon = rebin(mat_lon,urp_nx,urp_ny)
    ;mat_lat = rebin(rotate(mat_lat,1),urp_nx,urp_ny)







;get the lat/lon grid of baltrad composites
read_h5, lats=h5_lats, lons=h5_lons
sz = size(h5_lats,/dim)
h5_nx = sz[0]
h5_ny = sz[1]


;determine latitudes and longitudes on device used for projection
set_plot, 'Z'
; projection image size in pixels
sz_zbuf = [800,800]     ;data square smaller than this because of boundaries
device, set_resolution=sz_zbuf, set_pixel_depth=24
device, decomposed=0
maps, pos=[0.,0.,1.,1.], /map, /grid, loc=loc
dev_lat_lon, sz_zbuf, dev_lat, dev_lon
device, z_buffering = 0
sz = size(dev_lat,/dim)
pnx = sz[0]
pny = sz[1]

;projection for urp
if ~file_test(ind_savename) or regen eq 1 then begin
    kdll, mat_lat, mat_lon, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
          proj_ind                                                         ;output
    save, proj_ind, filename=ind_savename, /compress
endif
restore, ind_savename, /verbose








;;projection for baltrad
if ~file_test(h5_ind_savename) or regen eq 1 then begin
    kdll, h5_lats, h5_lons, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
          h5_proj_ind                                                      ;output
    save, h5_proj_ind, filename=h5_ind_savename, /compress
endif
restore, h5_ind_savename, /verbose




;;for URP
;;make list of times where we want images
;count=0
;for nn=0, nf-1 do begin
;    if ((time_arr[nn] ge t0) and (time_arr[nn] le tf)) then begin
;
;        if count eq 0 then begin
;            time_acc = time_arr[nn]
;            li_acc   = li[nn]
;        endif else begin
;            time_acc = [time_acc, time_arr[nn]]
;            li_acc   = [li_acc,   li[nn]]
;        endelse
;        count ++
;    endif
;endfor


;;for cloud fraction 
;cloud_dir = '/users/dor/arma/bha/fs/cmcgoes/operation.satellites.cmcgoes.goes13/'
;jul_to_cmc, time_acc, time_cmc
;match_time, time_cmc, cloud_dir, 'CF',/nearest, $
;            files_cf_13
;files_cf_13[0] = files_cf_13[2]
;files_cf_13[1] = files_cf_13[2]
;cloud_dir = '/users/dor/arma/bha/fs/cmcgoes/operation.satellites.cmcgoes.goes15/'
;jul_to_cmc, time_acc, time_cmc
;match_time, time_cmc, cloud_dir, 'CF',/nearest, $
;            files_cf_15
;;compute projection indexes for CF13
;aa = where(files_cf_13 ne 'not_avail')
;get_gem_data, files_cf_13[aa[0]],var_name='CF',lat=lats_cf, lon=lons_cf
;print, 'kdll'
;kdll, lats_cf, lons_cf, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
;      cf13_proj_ind                                                    ;output
;aa = where(files_cf_15 ne 'not_avail')
;get_gem_data, files_cf_15[aa[0]],var_name='CF',lat=lats_cf, lon=lons_cf
;print, 'kdll'
;kdll, lats_cf, lons_cf, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
;      cf15_proj_ind                                                    ;output


;structure to be passed to individual processes
str = {time_arr:dblarr(count),li:strarr(count),pnx:0l,pny:0l,h5_nx:0l,h5_ny:0l,$
       dev_lon:fltarr(pnx,pny),dev_lat:fltarr(pnx,pny),      $
       proj_ind:fltarr(pnx,pny),h5_proj_ind:fltarr(pnx,pny), cf13_proj_ind:fltarr(pnx,pny), cf15_proj_ind:fltarr(pnx,pny), $
       h5_lats:fltarr(h5_nx,h5_ny),h5_lons:fltarr(h5_nx,h5_ny), $
       h5_data_dir:'',missing:0.,loc:'',$
       files_cf_13:strarr(count),files_cf_15:strarr(count)}
str.time_arr = time_acc
str.li       = li_acc
str.pnx = pnx
str.pny = pny
str.h5_nx = h5_nx
str.h5_ny = h5_ny
str.dev_lon = dev_lon
str.dev_lat = dev_lat
str.proj_ind = proj_ind
str.h5_proj_ind = h5_proj_ind
;str.cf13_proj_ind = cf13_proj_ind
;str.cf15_proj_ind = cf15_proj_ind
str.h5_data_dir=h5_data_dir
str.h5_lats = h5_lats
str.h5_lons = h5_lons
str.missing=missing
str.loc=loc
;str.files_cf_13=files_cf_13
;str.files_cf_15=files_cf_15
str_savename = '~/documents/idl_sav_files/anim_compsite.sav'
;save, str, filename=str_savename


;make the images
n_task = count
n_cpu = 30

if keyword_set(serial) then begin
    ;serially
    ;for i=n_task-68, n_task-1 do begin
    for i=0, n_task-1 do begin
        radar_img,i,str
        if i ge 0 then stop
    endfor
endif else begin
    ;in parallel
    print, 'computing in parallel...'
    ; initialize and get the process manager
    cpu_pm=get_cpu_process_manager(n_cpu)
    ; setup by setting to all slaves the same working directory, idl path and compiling the same routines as the current idl session     
    cpu_pm->setup
    ; send one unit of work to each session
    for i=0, min([n_cpu-1,n_task-1]) do task_id=cpu_pm->call_procedure('radar_img',i,str)
    ; if necessary, loop over getting new work requests until there is no more work to be done
    if n_task - n_cpu ge 1 then begin
        for i=n_cpu, n_task-1 do begin
          ; wait until one of the processes finish
          task_id=cpu_pm->waitend()
          ; receive result and save it
          dummy=cpu_pm->getresult(task_id)
          ; send a new work unit
          task_id=cpu_pm->call_procedure('radar_img',i,str)
        endfor
    endif
    ; there is no more work to be done, so receive all the outstanding results from the slaves
    for i=0, n_cpu-1 do begin
      task_id=cpu_pm->waitend()
      dummy=cpu_pm->getresult(task_id)
    endfor
    print, 'done'
endelse



;caldat, t0, month,day,year,hour,minute,sec
;date_str = string([year,month,day,hour,minute], format='(i4,4i02)')
;
;WAIT, 15 ;wait for all images to be converted
;
;IF N_ELEMENTS(special) NE 0 THEN BEGIN
;    print, 'movie:  '+pic_dir_probs+date_str+'_'+special+'.gif'
;    SPAWN, 'convert -delay 10   -loop 0   '+pic_dir_tmp+'*.gif '+pic_dir_probs+date_str+'_'+special+'.gif'
;ENDIF ELSE BEGIN
;    print, 'movie:  '+pic_dir+date_str+'.gif'
;    SPAWN, 'convert -delay 10   -loop 0   '+pic_dir_tmp+'*.gif '+pic_dir+date_str+'.gif'
;ENDELSE



;mencoder "mf://*.jpg" -mf fps=12 -o ~/public_html/movies/test.avi -ovc lavc -lavcopts vcodec=msmpeg4v2:vbitrate=7000

END
