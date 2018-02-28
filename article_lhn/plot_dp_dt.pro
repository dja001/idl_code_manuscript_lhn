PRO plot_dp_dt, ii, nn, exp_struc, missing, $;base info
                yorder, ps,                 $;image position info
                cc_lat_arr, cc_lon_arr,     $;cross section position
                dp_dt, pr_acc                ;data output

    min_acc = .05

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

    ;calculate dp0/dt
    dp_dt = fltarr(exp_struc.maxnum,exp_struc.maxnum,exp_struc.ndom)+missing
    for dd=0, exp_struc.ndom-1 do begin
        nx = exp_struc.nx[dd]
        ny = exp_struc.ny[dd]

        if (exp_struc.file_t[ii,nn,dd] ne 'not_avail') and (exp_struc.file_mdt[ii,nn,dd] ne 'not_avail') then begin
            ;pressure at time t
            get_gem_data, exp_struc.file_t[ii,nn,dd],  VAR_NAME='P0', values=p0,      cmc_timestamp=exp_struc.time_t[ii,nn,dd]
            on_model_grid, p0, nx, ny, missing
            ;pressure at time mdt
            get_gem_data, exp_struc.file_mdt[ii,nn,dd], VAR_NAME='P0', values=p0_mdt, cmc_timestamp=exp_struc.time_mdt[ii,nn,dd]
            on_model_grid, p0_mdt, nx, ny, missing
            ;surface pressute tendencies
            dp_dt[0:nx-1,0:ny-1,dd] = (p0 - p0_mdt)/exp_struc.model_dt[dd]
        endif
    endfor

    ;calculate accumuated PR
    pr_acc = fltarr(exp_struc.maxnum,exp_struc.maxnum,exp_struc.ndom)+missing
    for dd=0, exp_struc.ndom-1 do begin
        nx = exp_struc.nx[dd]
        ny = exp_struc.ny[dd]
        if (exp_struc.file_pr[ii,nn,dd] ne 'not_avail') and (exp_struc.file_pr_mdt[ii,nn,dd] ne 'not_avail') then begin
            get_gem_data, exp_struc.file_pr[ii,nn,dd], var_name='PR', values=pr, cmc_timestamp=exp_struc.time_pr[ii,nn,dd]
            on_model_grid, pr, nx, ny, missing

            ;in a cycled forecast, PR need not be substracted five minutes after the hour
            ;this function should no be enabled for continuous forecasts
            cmd='r.date -V '+ string(exp_struc.time_pr_mdt[ii,nn,dd], format='(i12)')
            spawn, cmd, time
            minute = strmid(time, 10,2)
            read_pr = 1
            if (minute eq '00') then begin    ;continuous forecasts
                case nn of
                    0 : print, 'pr as usual '
                    1 : print, 'pr as usual '
                    2 : print, 'pr as usual '
                    3 : print, 'pr as usual '
                    else: begin
                              print, 'use 0 for pr_mdt'
                              pr_mdt = replicate(0.,nx,ny)
                              read_pr = 0
                          end
                endcase
            endif 

            ;;for forecasts with -ve time, skip 0h since counter are reset at this moment
            ;if ii eq 37 then begin
            ;    print, 'use 0 for pr_mdt'
            ;    pr_mdt = replicate(0.,nx,ny)
            ;    read_pr = 0
            ;endif

            if read_pr eq 1 then begin
                get_gem_data, exp_struc.file_pr_mdt[ii,nn,dd], var_name='PR', values=pr_mdt, cmc_timestamp=exp_struc.time_pr_mdt[ii,nn,dd]
                on_model_grid, pr_mdt, nx, ny, missing
            endif
        
            ;precip accumulation
            pr_acc[0:nx-1,0:ny-1,dd] = (pr - pr_mdt)*1e3*60./exp_struc.pr_dt    ;conversion to mm/hr

        endif
    endfor

    base = 20.
    range = [0.1,base/32.,base/16.,base/8.,base/4.,base/2.,base]
    legs, range=range, $
          color_arr=['brown','blue','green','orange','red','pink'], $
          ;color_arr=['blue','green','orange'], $
          excep_val=[missing,-3000.,0.], excep_col=['grey_230','brown','white'], excep_tol=[1e-3,1.,1e-4], $
          over_high='extend', under_low='white',   $
          mapping=mapping_accum

    black_white = [ [[000,000,000],[255,255,255]],[[000,000,000],[255,255,255]] ]
    legs, range=[-.03,.03], $
          color_arr=black_white,dark_pos=['low','high'], $
          excep_val=missing, excep_col='white',     $
          over_high='extend', under_low='extend',      $
          mapping=mapping_p0tend


    for dd=0, exp_struc.ndom-1 do begin
        nx = exp_struc.nx[dd]
        ny = exp_struc.ny[dd]
        ;make image for this domain
        apply_proj,  dp_dt[0:nx-1,0:ny-1,dd], exp_struc.proj_ind[*,*,dd], proj_dp_dt,  missing=missing
        apply_proj, pr_acc[0:nx-1,0:ny-1,dd], exp_struc.proj_ind[*,*,dd], proj_pr_acc, missing=missing
        ;apply color mapping
        legs, data=proj_dp_dt,  mapping=mapping_p0tend, img_out=rgb_dp_dt
        legs, data=proj_pr_acc, mapping=mapping_accum,  img_out=rgb_pr_acc
        ;mix the two images
        ;mm1 = 0.
        ;mm2 = .2
        ;LINMAP, proj_pr_acc, alpha, [mm1,mm2],[0.,1.]
        alpha = fltarr(size(proj_pr_acc,/dim))
        aa = where(proj_pr_acc gt .1, naa)
        if naa ne 0 then alpha[aa] = 1.
        IMG_MIX, blended_img, rgb_dp_dt, rgb_pr_acc, alpha
        if dd eq 0 then begin
            ;save image 
           composite = blended_img
           ;REMOVE DPDT FROM MIX
                composite = rgb_pr_acc
        endif else begin
            ;add smaller domain over previously computed image
            alpha=fltarr(size(proj_dp_dt,/dim))
            aa = where((proj_dp_dt ne missing), caa)
            if caa ne 0 then alpha[aa] = 1.
            img_mix, blended_2dom, composite, blended_img, alpha
            composite = blended_2dom
        endelse
    endfor
    
    ;finally plot image
    y0 = ps.y1 + yorder*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 + nn*(ps.rec_w + ps.sp_w)
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;image
    loadct,0,/s
    tv, composite, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3
    ;titles

    ;overlay domain border
    maps, pos=pos, /grid, /map, loc=exp_struc.loc
    for dd=0, exp_struc.ndom-1 do begin
        nx = exp_struc.nx[dd]
        ny = exp_struc.ny[dd]
        PLOT_BORDER, MAT_LAT=exp_struc.lat[0:nx-1,0:ny-1,dd], MAT_LON=exp_struc.lon[0:nx-1,0:ny-1,dd]
    endfor

    ;color rectangle for title
    sx0 = pos[0]+.02*(pos[2]-pos[0]) 
    sy0 = pos[1]+.82*(pos[3]-pos[1]) 
    spos = [sx0,sy0,sx0+ps.rec_w/2.,sy0+1.2/ps.pic_h]
    tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
    tv,[1], spos[0], spos[1], xs=spos[2]-spos[0], ys=spos[3]-spos[1], /normal
    ;title text
    loadct, 40, /s
    xyouts, sx0+.2/ps.pic_w, sy0+.2/ps.pic_h, exp_struc.desc[nn],  /normal,charsize=2.7, col=0
    
    ;;plot location of cross section
    loadct,40,/s
    if n_elements(cc_lat_arr) ne 0 then plots, cc_lon_arr, cc_lat_arr, th=5., col=210

    ;palettes
    IF nn EQ exp_struc.nexp-1 THEN BEGIN
        ;x0 = x0 + ps.rec_w + ps.pal_sp
        ;pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        ;LEGS, MAPPING=mapping_accum, PALETTE=pos, PAL_PROP='equal', UNITS='mm/h', YTICKFORMAT='(f6.1)'
        ;x0 = x0 + ps.pal_w+2.*ps.pal_sp
        ;pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        ;LEGS, MAPPING=mapping_p0tend, PALETTE=pos, UNITS='Pa/min'
    ENDIF
END
