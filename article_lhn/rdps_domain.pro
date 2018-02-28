PRO rdps_domain
;rdps domain with topography

pic_name = '~/documents/ps/rdps_domains.ps'


;define figure size and position of plot in figure
pic_w = 2.*12.       
pic_h = 2.*8.        
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
loc = 'renkf'
maps, pos=pos, loc=loc, /grid, col=0

;;  red  ora  yel lblu dblu black
;;   0    1   2   3    4    5
;r = [249,238,244,008, 008, 000]
;g = [087,150,061,109, 061, 000]
;b = [056,075,094,167, 119, 000]
;tvlct, r,g,b

legs, range=[0,2500], col='brown', dark_pos='low', over_high='extend', $
      excep_val=[missing,0.],excep_col=[[255,255,255],[000,024,159]], excep_tol=[1.,1.], $
      mapping=topo_map


r_file='/local/raid/armadja/data/domains/RDPS_10km_yin.fst'
g_file='/local/raid/armadja/data/domains/GDPS.fst'

;latitudes and longitude of device 
sz_zbuf = [1200,1200]     ;data square smaller than this because of boundaries
;determine latitudes and longitudes on device used for projection
set_plot, 'Z'
; projection image size in pixels
device, set_resolution=sz_zbuf, set_pixel_depth=24
device, decomposed=0
;map projection
sub_domain = [0,0.,1.,1.]
maps, pos=sub_domain, /map, /grid, loc=loc
dev_lat_lon,sz_zbuf,dev_lat, dev_lon
device, z_buffering = 0

ps_start, pic_name, pic_w, pic_h

    ;lat lon and ME of source data
    get_gem_data, r_file, lat=r_lat, lon=r_lon, values=r_me, var_name='ME'
    get_gem_data, g_file, lat=g_lat, lon=g_lon, values=g_me, var_name='ME'

    ;;set all low values to zero
    ;aa = where(r_me lt 10., naa)
    ;if naa ne 0 then r_me[aa] = 0.

    ;nearest neighbor with kdll
    kdll, r_lat, r_lon, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
          r_proj_ind                                                   ;output
    kdll, g_lat, g_lon, dev_lat, dev_lon, missing=missing, /nearest, /noextend, $ ;input
          g_proj_ind                                                              ;output

    ;fix bug with nan search result
    ind_0 = r_proj_ind[0,0]
    aa = where(r_proj_ind eq ind_0, naa)
    if naa ne 0 then r_proj_ind[aa] = missing
    ind_0 = g_proj_ind[0,0]
    aa = where(g_proj_ind eq ind_0, naa)
    if naa ne 0 then g_proj_ind[aa] = missing

    ;project
    apply_proj, r_me, r_proj_ind, proj_r_me, missing=missing
    apply_proj, g_me, g_proj_ind, proj_g_me, missing=missing

    ;make image
    legs, data=proj_r_me, mapping=topo_map, img_out=r_img
    legs, data=proj_g_me, mapping=topo_map, img_out=g_img

    ;blend image
    alpha = fltarr(size(r_proj_ind,/dim))
    aa = where(r_proj_ind ne missing, naa)
    if naa ne 0 then alpha[aa] = 1.
    img_mix, blended_img, g_img, r_img, alpha

    ;plot image
    loadct,0,/s
    tv, blended_img, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    maps, pos=pos, /map, /grid, loc=loc


    loadct,0,/s
    plot_border, file=r_file, var='ME', col=250






;close img
ps_close, pic_name, /v, /pdf, /del_ps, font='lmroman'    

END 

