
pro plot_kf, ii, jj, exp_str, nat_str,missing,                         $ ;base info
               ps,loc,      $ ;image position info
               exp_rdqi, nat_rdqi, lat_arr, lon_arr                        ;data input
    x0 = ps.x1
    y0 = ps.y1

    base = 10.
    range = [base/64.,base/32.,base/16.,base/8.,base/4.,base/2.,base]
    legs, range=range, $
          color_arr=['brown','blue','green','orange','red','pink'], $
          ;color_arr=['blue','green','orange'], $
          excep_val=[missing,-3000.,0.], excep_col=['grey_230','brown','white'], excep_tol=[1e-3,1.,1e-2], $
          over_high='extend', under_low='white',   $
          mapping=mapping_rr

    ;project data
    apply_proj, exp_rdqi, exp_str[jj].proj_ind, exp_proj_rdqi, missing=missing
    apply_proj, nat_rdqi, nat_str[jj].proj_ind, nat_proj_rdqi, missing=missing

    ;apply color mapping
    legs, data=exp_proj_rdqi,     mapping=mapping_rr,    img_out=exp_img_rdqi
    legs, data=nat_proj_rdqi,     mapping=mapping_rr,    img_out=nat_img_rdqi

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
    xyouts, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'lhn decision tree',/normal,charsize=1.7, col=0
    ;overlay grid
    maps, pos=pos, /grid, /map, loc=loc
    plot_border, mat_lat=nat_str[jj].lat, mat_lon=nat_str[jj].lon
    ;plot_border, mat_lat=exp_str[jj].lat, mat_lon=exp_str[jj].lon

    ;palette
    nexp = n_elements(exp_str)
    if jj eq nexp-1 then begin
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        legs, mapping=mapping_rr, palette=pos, pal_prop='equal'
    endif
end
