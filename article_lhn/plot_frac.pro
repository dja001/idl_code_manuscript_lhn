pro plot_frac, ii, nn, exp_struc, missing,     $ ;base info
               yorder, ps,                     $ ;figure info
               tresh_ind, field              ;input data


    tresh  = exp_struc.tresh_arr[tresh_ind]
    this_l = exp_struc.lmin[ii,tresh_ind,nn]
    if ~finite(this_l) then this_l = 500.
    this_l = 5.;/= 10.   ;in pixels instead of km

    field_true  = field  ge tresh
    field_n = fltarr(size(field_true,/dim))+missing
    ;compute Lmin for different tresholds
    imin = 250L
    imax = 580L
    jmin = 40L
    jmax = 350L
    nx = imax - imin
    ny = jmax - jmin

    ;make index for this circle
    sq_nx=2L*this_l+1L
    xx = rebin(findgen(sq_nx),sq_nx,sq_nx) - this_l
    yy = rotate(xx,1)
    aa = where(sqrt(xx^2.+yy^2.) le this_l, num_pts)
    for i=imin, imax-1 do begin
        for j=jmin, jmax-1 do begin
            field_n[i,j] = mean( field_true[i+xx[aa],j+yy[aa]])
        endfor
    endfor

    legs, range=[0,1.], $
          color_arr=['b_w'], $
          excep_val=[missing], excep_col=['blue'],  $
          mapping=mapping_frac

    ;project data
    APPLY_PROJ, field_n, exp_struc.proj_ind[*,*,0], proj_frac, MISSING=missing

    ;apply color mapping
    LEGS, DATA=proj_frac,     MAPPING=mapping_frac,    IMG_OUT=img_frac

    ;figure position
    y0 = ps.y1 + yorder*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 + nn*(ps.rec_w + ps.sp_w)
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]

    ;plot images
    loadct,0,/s
    tv, img_frac, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;titles
    xyouts, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'r = f(z)',/normal,charsize=1.7, col=0
    ;overlay grid
    maps, pos=pos, /grid, /map, loc=exp_struc.loc
    nx = exp_struc.nx
    ny = exp_struc.ny
    plot_border, mat_lat=exp_struc.lat[0:nx-1,0:ny-1,0], mat_lon=exp_struc.lon[0:nx-1,0:ny-1,0]

    ;palette
    nexp = exp_struc.nexp
    if nn eq nexp-1 then begin
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        legs, mapping=mapping_frac, palette=pos, units='unitless'
    endif
end

