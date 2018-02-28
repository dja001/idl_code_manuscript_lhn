pro plot_rdbr, ii, nn, exp_struc, missing, data_dir, rdt,  $ ;base info
               yorder, ps,nowcast=nowcast,                 $ ;figure info
               rad_rdbr, rad_qi                              ;output data

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

    time_str = exp_struc.time_str[ii,nn,0]
    yy = strmid(time_str, 0,4)
    mo = strmid(time_str, 5,2)
    dd = strmid(time_str, 8,2)
    hh = strmid(time_str,12,2)
    case rdt of
        5: begin
               mi = strmid(time_str,15,2)
           end
        10:begin
               mi = strmid(time_str,15,1)+'0'
           end
        else: message, 'rdt can only be 5 or 10'
    endcase
    rad_jul_date = julday(long(mo),long(dd),long(yy),long(hh),long(mi))
    jul_to_cmc, rad_jul_date, rad_cmc_time
    rad_file = data_dir+'/'+yy+'/'+mo+'/'+dd+'/'+yy+mo+dd+hh+'_'+mi+'.fst'

    ;nat_domain
    ;get_data
    print, rad_file
    get_gem_data, rad_file, var_name='RDPR',values=rad_rdbr, cmc_timestamp=rad_cmc_time, is_there=is_there_pr
    get_gem_data, rad_file, var_name='RDQI',values=rad_qi,   cmc_timestamp=rad_cmc_time, is_there=is_there_qi
    if is_there_pr eq 0 then begin
        rad_rdbr = replicate(missing, exp_struc.nx[0], exp_struc.ny[0])
    endif 
    if is_there_qi eq 0 then begin
        rad_qi = replicate(missing, exp_struc.nx[0], exp_struc.ny[0])
    endif 
    ;else begin
        ;rad_rdbr = 10.0^(rad_rdbr/16.) / 27.424818   
    ;endelse

    maxmin, rad_rdbr, name='rdpr', missing=-999.
    on_model_grid, rad_rdbr, exp_struc.nx[0], exp_struc.ny[0], missing
    on_model_grid, rad_qi  , exp_struc.nx[0], exp_struc.ny[0], missing


        ;make jhuapl time string
        ddmo = jd2date(exp_struc.time_jul[ii,nn,0], form='w$ n$ ')
        jhu_time_str = ddmo+string(dd,hh,mi,0,yy,format='(i02," ",i02,":",i02,":",i02," ",i4)')
        ;day/night figure
        ;make rgb map of day and night
        sunaltazi, jhu_time_str, exp_struc.dev_lon, exp_struc.dev_lat, azi, alt
        aa = where(alt ge 0., naa,complement=bb,ncomplement=nbb)
        ;;dark
        ;day_rgb   = [255,179,000]
        ;night_rgb = [189,187,240]
        ;pale
        day_rgb   = [255,255,225]
        night_rgb = [225,225,255]
        sz_zbuf = size(exp_struc.dev_lat,/dim)
        day_night_rgb = bytarr([sz_zbuf,3])
        is_day        = bytarr(sz_zbuf)
        is_night      = bytarr(sz_zbuf)
        if naa ne 0 then   is_day[aa] = 1.
        if nbb ne 0 then is_night[bb] = 1.
        for kk=0,2 do begin
            day_night_rgb[*,*,kk] = is_day*day_rgb[kk] + is_night*night_rgb[kk]
        endfor


    base = 20.
    range = [0.1,base/32.,base/16.,base/8.,base/4.,base/2.,base]
    legs, range=range, $
          color_arr=['brown','blue','green','orange','red','pink'], $
          ;color_arr=['blue','green','orange'], $
          excep_val=[missing,0.], excep_col=['grey_230','white'], excep_tol=[1e-3,1e-4], $
          over_high='extend', under_low='white',   $
          mapping=mapping_rr

    legs, range=[0,60], $
          color_arr=['brown','blue','green','orange','red','pink'], $
          ;color_arr=['blue','green','orange'], $
          excep_val=[missing,-3000.,0.], excep_col=['grey_230','brown','white'], excep_tol=[1e-3,1.,1e-2], $
          over_high='extend', under_low='white',   $
          mapping=mapping_db

    ;project data
    apply_proj, rad_rdbr, exp_struc.proj_ind[*,*,0], proj_rdbr, missing=missing

    ;apply color mapping
    legs, data=proj_rdbr,     mapping=mapping_rr,    img_out=img_rdbr

    ;mix day/night and radar data
    ;add radar data
    alpha = fltarr(sz_zbuf)
    aa = where(proj_rdbr gt 0., naa)
    if naa ne 0 then alpha[aa] = 1.
    img_mix, blended_rgb, day_night_rgb, img_rdbr, alpha
        ;image without day/night
        blended_rgb = img_rdbr

    ;figure position
    y0 = ps.y1 + yorder*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 + nn*(ps.rec_w + ps.sp_w) ;      -ps.rec_w/2.-ps.sp_w/2.
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]

    ;plot images
    loadct,0,/s
    tv, blended_rgb, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;overlay grid
    maps, pos=pos, /grid, /map, loc=exp_struc.loc

    ;;plot border of verification domain
    ;imin = exp_struc.verif_domain[0]
    ;jmin = exp_struc.verif_domain[1]
    ;imax = exp_struc.verif_domain[2]
    ;jmax = exp_struc.verif_domain[3]
    ;plot_border, mat_lat=exp_struc.lat[imin:imax-1,jmin:jmax-1], mat_lon=exp_struc.lon[imin:imax-1,jmin:jmax-1]

    ;;titles
    ;type = ''
    ;if strmatch(data_dir,'*r_data*')   then type = 'Radar'
    ;if strmatch(data_dir,'*nowcasts*') then type = 'Nowcast'
    ;xyouts, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.90*(pos[3]-pos[1]),type+' r = f(z)',/normal,charsize=2.7, col=0

    ;color rectangle for title
    sx0 = pos[0]+.02*(pos[2]-pos[0]) 
    sy0 = pos[1]+.82*(pos[3]-pos[1]) 
    spos = [sx0,sy0,sx0+ps.rec_w/2.,sy0+1.2/ps.pic_h]
    if keyword_set(nowcast) then begin
        tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
        title='Nowcast'
        col_num = 1
    endif else begin
        title='Radar'
        loadct, 0, /s
        col_num = 255
    endelse
    tv,[col_num], spos[0], spos[1], xs=spos[2]-spos[0], ys=spos[3]-spos[1], /normal
    ;title text
    loadct, 40, /s
    xyouts, sx0+.2/ps.pic_w, sy0+.2/ps.pic_h, title,  /normal,charsize=2.7, col=0

    ;palette
    nexp = exp_struc.nexp
    if nn eq nexp-1 then begin
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        legs, mapping=mapping_rr, palette=pos, pal_prop='equal',units='mm/h', ytickformat='(f6.1)'
        ;legs, mapping=mapping_db, palette=pos, units='dbz'
    endif
end
