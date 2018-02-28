PRO meopar_east_domain
;rdps domain with topography

pic_name = '~/documents/ps/meopar_east_domains.ps'


;define figure size and position of plot in figure
pic_w = 15.       
pic_h = 2.*7.5
rec_w = 2.*6./pic_w  
rec_h = 2.*6./pic_h  
sp_w = 2./pic_w      
x1 = 1.2/pic_w       
y1 = 1.2/pic_h       
pal_sp = 1./pic_w    
pal_w = .25/pic_w    
missing = -9999.

pos = [x1, y1, x1+rec_w, y1+rec_h]     

;open figure
    
;load map
loadct, 0, /s
loc = 'meopar_east'
maps, pos=pos, loc=loc, /grid, col=0


legs, range=[0,2500], col='brown', dark_pos='low', over_high='extend', $
      excep_val=[missing,0.],excep_col=[[255,255,255],[000,024,159]], excep_tol=[1.,1.], $
      mapping=topo_map


r_file='/local/raid/armadja/data/domains/meopar_east_weiguang.fst'

;;latitudes and longitude of device 
;sz_zbuf = [1200,1200]     ;data square smaller than this because of boundaries
;;determine latitudes and longitudes on device used for projection
;set_plot, 'Z'
;; projection image size in pixels
;device, set_resolution=sz_zbuf, set_pixel_depth=24
;device, decomposed=0
;;map projection
;sub_domain = [0,0.,1.,1.]
;maps, pos=sub_domain, /map, /grid, loc=loc
;dev_lat_lon,sz_zbuf,dev_lat, dev_lon
;device, z_buffering = 0

ps_start, pic_name, pic_w, pic_h

    ;;lat lon and ME of source data
    ;get_gem_data, r_file, lat=r_lat, lon=r_lon, values=r_me, var_name='ME'

    ;;nearest neighbor with kdll
    ;kdll, r_lat, r_lon, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
    ;      r_proj_ind                                                   ;output

    ;;project
    ;apply_proj, r_me, r_proj_ind, proj_r_me, missing=missing

    ;;make image
    ;legs, data=proj_r_me, mapping=topo_map, img_out=r_img


    ;;plot image
    ;loadct,0,/s
    ;tv, blended_img, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    maps, pos=pos, /map, /grid, loc=loc


    loadct,0,/s
    plot_border, file=r_file, var='P0', col=00, TH=3.

    ;plot all radars
    radar_info, allr
    for ii=0, n_elements(allr)-1 do begin
        pp = convert_coord([allr[ii].lon,allr[ii].lat], /data, /to_normal)
        if pp[0] gt pos[0] and pp[0] lt pos[2] and pp[1] gt pos[1] and pp[1] lt pos[3] then begin
            plots, [allr[ii].lon], [allr[ii].lat], psym=8, col=0, symsize=.5
            xyouts, allr[ii].lon,   allr[ii].lat, allr[ii].id, charsize=.5

            ;make circle array
            nn=400
            azs = findgen(nn)/(nn-1)*360.
            lats = fltarr(nn)
            lons = fltarr(nn)
            for jj=0l, nn-1 do begin
                ;can or us radar?
                letter = strmid(allr[ii].id,0,1)
                if letter eq 'C' then begin
                    range_km = 128.
                    col = 210
                endif else begin
                    range_km = 250.
                    col = 80
                endelse
                lat_lon_range, allr[ii].lat, allr[ii].lon, range_km, azs[jj], lat_o, lon_o
                lats[jj] = lat_o
                lons[jj] = lon_o
            endfor
            loadct, 40, /s
            plots, lons, lats, col=col, th=1.

        endif
    endfor






;close img
ps_close, pic_name, /v, /pdf, /del_ps, font='lmroman'    

END 

