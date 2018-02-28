pro plot_tlhs, ii, jj, exp_str, nat_str,missing,                         $ ;base info
               ps,loc,      $ ;image position info
               exp_rdqi, nat_rdqi, lat_arr, lon_arr                        ;data input
    x0 = ps.x1
    y0 = ps.y1
    blue_orange = [ [013, 013, 134],$      ;dark blue
                    [000, 081, 192],$
                    [000, 126, 237],$
                    [000, 169, 191],$
                    [153, 216, 224],$
                    [204, 249, 255],$      ;pale bue
                    [255, 255, 169],$      ;pale orange
                    [255, 205, 124],$
                    [255, 159, 071],$
                    [255, 119, 051],$
                    [164, 053, 000],$
                    [104, 010, 000] ]      ;dark orange
    legs, range=[-2.,2.], $
          color_arr=blue_orange, solid='supplied',$
          mapping=mapping_rdqi, $
          over_under='extend' , $
          excep_val=[missing,0.], excep_col=['white','white']

    ;project data
    apply_proj, exp_rdqi, exp_str[jj].proj_ind, exp_proj_rdqi, missing=missing
    aa = where(nat_rdqi ne missing,caa)
    if caa ne 0 then nat_rdqi[aa] = nat_rdqi[aa]*1000.
    apply_proj, nat_rdqi, nat_str[jj].proj_ind, nat_proj_rdqi, missing=missing

    ;apply color mapping
    legs, data=exp_proj_rdqi,     mapping=mapping_rdqi,    img_out=exp_img_rdqi
    legs, data=nat_proj_rdqi,     mapping=mapping_rdqi,    img_out=nat_img_rdqi

    ;blend the two images
    alpha=fltarr(size(exp_proj_rdqi,/dim))
    aa = where((exp_proj_rdqi ne missing), caa)
    if caa ne 0 then alpha[aa] = 1.
    img_mix, rgb_blended_2dom, nat_img_rdqi, exp_img_rdqi, alpha

    ;figure position
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]

    ;plot images
    loadct,0,/s
    tv, rgb_blended_2dom, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;titles
    xyouts, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'total moisture added/removed',/normal,charsize=1.7, col=0
    ;overlay grid
    maps, pos=pos, /grid, /map, loc=loc
    plot_border, mat_lat=nat_str[jj].lat, mat_lon=nat_str[jj].lon
    ;plot_border, mat_lat=exp_str[jj].lat, mat_lon=exp_str[jj].lon

    ;palette
    nexp = n_elements(exp_str)
    if jj eq nexp-1 then begin
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        legs, mapping=mapping_rdqi, palette=pos, units='g'
    endif
end
