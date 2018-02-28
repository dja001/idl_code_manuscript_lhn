pro fss_example
;this program plots out images for explaining the fss score and the Lmin metric 

;read data
nc_file = '/local/drive2/arma/armadja/data/lhn_test_outputs/pr_data_for_score_examples.nc'
nc_id  = ncdf_open(nc_file, /write)
ncdf_varget, nc_id, 'lat', lat
ncdf_varget, nc_id, 'lon', lon
ncdf_varget, nc_id, 'radar', radar
ncdf_varget, nc_id, 'control', control
ncdf_varget, nc_id, 'lhn', lhn
ncdf_varget, nc_id, 'lhnnow', lhnnow
ncdf_varget, nc_id, 'nowcast', nowcast
ncdf_close, nc_id                                      ;close file

;determine nx and ny from latitude matrix
sz = SIZE(lat, /DIM)
dom_nx = sz[0]
dom_ny = sz[1]

;generate projection indices

;get projection indices for the different domains
res = 10.
loc = 'mid_us'
proj_file = '~/documents/idl_sav_files/fss_example.sav'
    ;sz_zbuf = [801,801] ;size of z buffer
    ;;Z buffer stuff to set projection indices
    ;SET_PLOT, 'Z'
    ;; projection image size in pixels
    ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
    ;DEVICE, DECOMPOSED=0
    ;;map projection
    ;sub_domain=[0,0.,1.,1.]
    ;MAPS, POS=sub_domain, LATS=lat, LONS=lon, MISSING=missing, PROJ_IND=proj_ind, LOC=loc
    ;    ;mis_proj_up
    ;    arr_lat=lat[*,dom_ny-1]
    ;    arr_lon=lon[*,dom_ny-1]
    ;    nn=5
    ;    mis_lon=REBIN(arr_lon,dom_nx,nn)
    ;    mis_lat=FLTARR(dom_nx,nn)
    ;    FOR ii=0D,nn-1 DO mis_lat[*,ii] = arr_lat + .5*ii
    ;    MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_up, LOC=loc
    ;    missing_up = WHERE(proj_mis_up NE missing, nmis)
    ;    IF nmis NE 0 THEN proj_ind[missing_up]=missing
    ;    ;mis_proj_down
    ;    arr_lat=lat[*,0]
    ;    arr_lon=lon[*,0]
    ;    nn=5
    ;    mis_lon=REBIN(arr_lon,dom_nx,nn)
    ;    mis_lat=FLTARR(dom_nx,nn)
    ;    FOR ii=nn-1,0,-1 DO mis_lat[*,ii] = arr_lat - .5*ii
    ;    MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_do, LOC=loc
    ;    missing_do = WHERE(proj_mis_do NE missing, nmis)
    ;    IF nmis NE 0 THEN proj_ind[missing_do]=missing
    ;DEVICE, Z_BUFFERING = 0
    ;;save projection indices
    ;SAVE, proj_ind, FILENAME=proj_file, /COMPRESS
;restore previously generated indices
RESTORE, proj_file;, /VERBOSE


;set up image coords setup 
ps = {pic_w:0.,pic_h:0.,sq_w:0.,sq_h:0.,pal_sp:0.,pal_w:0.,rec_w:0.,rec_h:0.,sp_w:0.,sp_h:0.,$
         x1:0.,   x2:0.,  x3:0.,  x4:0.,    x5:0.,   x6:0.,   x7:0.,   x8:0.,  x9:0.,        $
         y1:0.,   y2:0.,  y3:0.,  y4:0.,    y5:0.,   y6:0.,   y7:0.,   y8:0.,  y9:0.}
ps.sq_w = 10.
ps.sq_h = 10.
ps.sq_h = .8*ps.sq_h
ps.pic_h = 24.
ps.pic_w  = 30.
ps.pal_sp = 1.3/ps.pic_w
ps.pal_w  = .25/ps.pic_w
ps.rec_w  = ps.sq_w/ps.pic_w
ps.rec_h  = ps.sq_h/ps.pic_h
ps.sp_w   = 2./ps.pic_w
ps.sp_h   = 2./ps.pic_h
ps.x1     = 2.5/ps.pic_w 
ps.x2     = ps.x1 + ps.rec_w + 2.*ps.sp_w
ps.y1     = 2./ps.pic_h
!p.charsize = 6.7
missing = -9999.

;palettes
base = 20.
range = [base/64.,base/32.,base/16.,base/8.,base/4.,base/2.,base]
LEGS, RANGE=range, $
      COLOR_ARR=['brown','blue','green','orange','red','pink'], $
      EXCEP_VAL=[missing,0.], EXCEP_COL=['grey_230','white'], EXCEP_TOL=[1e-3,1e-2], $
      OVER_HIGH='extend', UNDER_LOW='white',   $
      MAPPING=mapping_rr
LEGS, RANGE=[0,1], $
      COLOR_ARR=['blue'], $
      EXCEP_VAL=[missing,0.], EXCEP_COL=['grey_230','white'], $
      MAPPING=mapping_01

;treshold-----------------------------------------------------
base = 10.
tresh_arr = [base/32.,base/16.,base/8.,base/4.,base/2.,base]
this_tresh = tresh_arr[1]

;ones within verification region
imin = 550 
jmin = 220 
imax = 855
jmax = 450
verif_true = bytarr(dom_nx,dom_ny)
verif_true[imin:imax-1,jmin:jmax-1] = 1

;tresholded fields--------------------------------------------
forecast = lhnnow
fcst_true  = forecast ge this_tresh
refer_true = radar ge this_tresh

;first image is that of the threshold fields
pic_name = '~/documents/ps/fss_example.ps'
PS_START, pic_name, ps.pic_w, ps.pic_h

    ;;raw forecast field
    ;y0 = ps.y1 + 1*(ps.rec_h + ps.sp_h)
    ;x0 = ps.x1 + 0*(ps.rec_w + ps.sp_w)
    ;pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;;project data
    ;APPLY_PROJ, forecast, proj_ind, proj_data, MISSING=missing
    ;;apply color mapping and plot image
    ;LEGS, DATA=proj_data, MAPPING=mapping_rr, TV_OUT=pos
    ;;titles
    ;XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'forecast',/NORMAL,CHARSIZE=1.7, COL=0
    ;;overlay grid
    ;MAPS, POS=pos, /GRID, /MAP, LOC=loc
    ;PLOT_BORDER, MAT_LAT=lat, MAT_LON=lon
    ;PLOT_BORDER, MAT_LAT=lat[imin:imax-1,jmin:jmax-1], MAT_LON=lon[imin:imax-1,jmin:jmax-1]
    ;    x0 = x0 + ps.rec_w + ps.pal_sp
    ;    pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
    ;    LEGS, MAPPING=mapping_rr, PALETTE=pos, PAL_PROP='equal',UNITS='mm/h', YTICKFORMAT='(f6.1)'

    ;;radar field
    ;y0 = ps.y1 + 0*(ps.rec_h + ps.sp_h)
    ;x0 = ps.x1 + 0*(ps.rec_w + ps.sp_w)
    ;pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;;project data
    ;APPLY_PROJ, radar, proj_ind, proj_data, MISSING=missing
    ;;apply color mapping and plot image
    ;LEGS, DATA=proj_data, MAPPING=mapping_rr, TV_OUT=pos
    ;;titles
    ;XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'radar',/NORMAL,CHARSIZE=1.7, COL=0
    ;;overlay grid
    ;MAPS, POS=pos, /GRID, /MAP, LOC=loc
    ;PLOT_BORDER, MAT_LAT=lat, MAT_LON=lon
    ;PLOT_BORDER, MAT_LAT=lat[imin:imax-1,jmin:jmax-1], MAT_LON=lon[imin:imax-1,jmin:jmax-1]
    ;    x0 = x0 + ps.rec_w + ps.pal_sp
    ;    pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
    ;    LEGS, MAPPING=mapping_rr, PALETTE=pos, PAL_PROP='equal',UNITS='mm/h', YTICKFORMAT='(f6.1)'

    ;tresholded forecast
    y0 = ps.y1 + 1*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;project data
    APPLY_PROJ, fcst_true, proj_ind, proj_data, MISSING=missing
    ;apply color mapping and plot image
    LEGS, DATA=proj_data, MAPPING=mapping_01, TV_OUT=pos
    ;titles
    XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'radar > tresh',/NORMAL,CHARSIZE=1.7, COL=0
    ;overlay grid
    MAPS, POS=pos, /GRID, /MAP, LOC=loc
    PLOT_BORDER, MAT_LAT=lat, MAT_LON=lon
    PLOT_BORDER, MAT_LAT=lat[imin:imax-1,jmin:jmax-1], MAT_LON=lon[imin:imax-1,jmin:jmax-1]
        ;x0 = x0 + ps.rec_w + ps.pal_sp
        ;pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        ;LEGS, MAPPING=mapping_rr, PALETTE=pos, PAL_PROP='equal',UNITS='mm/h', YTICKFORMAT='(f6.1)'

    ;tresholded radar
    y0 = ps.y1 + 0*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;project data
    APPLY_PROJ, refer_true, proj_ind, proj_data, MISSING=missing
    ;apply color mapping and plot image
    LEGS, DATA=proj_data, MAPPING=mapping_01, TV_OUT=pos
    ;titles
    XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'fcst > tresh',/NORMAL,CHARSIZE=1.7, COL=0
    ;overlay grid
    MAPS, POS=pos, /GRID, /MAP, LOC=loc
    PLOT_BORDER, MAT_LAT=lat, MAT_LON=lon
    PLOT_BORDER, MAT_LAT=lat[imin:imax-1,jmin:jmax-1], MAT_LON=lon[imin:imax-1,jmin:jmax-1]
        ;x0 = x0 + ps.rec_w + ps.pal_sp
        ;pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        ;LEGS, MAPPING=mapping_rr, PALETTE=pos, PAL_PROP='equal',UNITS='mm/h', YTICKFORMAT='(f6.1)'


;compute Lmin for different tresholds
lmin = 1L    ;minumum radius in pixels must be an odd number
lmax = 50L   ;maximum radius in pixels
ver_nx = imax - imin
ver_ny = jmax - jmin

fss_target = .5 + float(mean(refer_true[imin:imax-1,jmin:jmax-1]))/2.
this_l = lmin
this_fss = 0
;while this_fss le fss_target do begin
while this_l le lmax do begin
    ;this_l = 17

    ;make index for this circle
    sq_nx=2L*this_l+1L
    xx = rebin(findgen(sq_nx),sq_nx,sq_nx) - this_l
    yy = rotate(xx,1)
    aa = where(sqrt(xx^2.+yy^2.) le this_l, num_pts)
    model_n = fltarr(dom_nx, dom_ny)
    radar_n = fltarr(dom_nx, dom_ny)
    for ii=imin, imax-1 do begin
        for jj=jmin, jmax-1 do begin
            model_n[ii,jj] = mean( fcst_true[ii+xx[aa],jj+yy[aa]])
            radar_n[ii,jj] = mean(refer_true[ii+xx[aa],jj+yy[aa]])
        endfor
    endfor
    mse     = total((radar_n[imin:imax-1,jmin:jmax-1] - model_n[imin:imax-1,jmin:jmax-1])^2.)/(ver_nx*ver_ny)
    mse_ref = (total(radar_n[imin:imax-1,jmin:jmax-1]^2.) + total(model_n[imin:imax-1,jmin:jmax-1]^2.))/(ver_nx*ver_ny)
    this_fss = 1 - mse/mse_ref

    l_txt = string(this_l*res, format='(i5)')
    fss_txt = string(this_fss, format='(f5.3)')

    ;;fraction model
    ;y0 = ps.y1 + 1*(ps.rec_h + ps.sp_h)
    ;x0 = ps.x2 ;+ (this_l+0)*(ps.rec_w + ps.sp_w)
    ;pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;;project data
    ;APPLY_PROJ, model_n, proj_ind, proj_data, MISSING=missing
    ;;apply color mapping and plot image
    ;LEGS, DATA=proj_data, MAPPING=mapping_01, TV_OUT=pos
    ;;titles
    ;XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'L   = '+l_txt+'km',/NORMAL,CHARSIZE=1.7, COL=0
    ;XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+1.1*(pos[3]-pos[1]),'FSS = '+fss_txt,/NORMAL,CHARSIZE=1.7, COL=0
    ;;overlay grid
    ;MAPS, POS=pos, /GRID, /MAP, LOC=loc
    ;    x0 = x0 + ps.rec_w + ps.pal_sp
    ;    pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
    ;    LEGS, MAPPING=mapping_01, PALETTE=pos, UNITS='unitless'

    ;;fraction radar
    ;y0 = ps.y1 + 0*(ps.rec_h + ps.sp_h)
    ;x0 = ps.x2 ;+ (this_l+0)*(ps.rec_w + ps.sp_w)
    ;pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]
    ;;project data
    ;APPLY_PROJ, radar_n, proj_ind, proj_data, MISSING=missing
    ;;apply color mapping and plot image
    ;LEGS, DATA=proj_data, MAPPING=mapping_01, TV_OUT=pos
    ;;titles
    ;XYOUTS, pos[0]+.02*(pos[2]-pos[0]),pos[1]+.93*(pos[3]-pos[1]),'L = '+l_txt+'km',/NORMAL,CHARSIZE=1.7, COL=0
    ;;overlay grid
    ;MAPS, POS=pos, /GRID, /MAP, LOC=loc
    ;    x0 = x0 + ps.rec_w + ps.pal_sp
    ;    pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
    ;    LEGS, MAPPING=mapping_01, PALETTE=pos, UNITS='unitless'


    ;if this_l gt 15 then break

    this_l ++
    print, this_tresh, this_l, mse, mse_ref, this_fss, fss_target

    ;no not exceed boundary of domain
    if this_l gt lmax then begin
        this_l = !values.f_nan
        break
    endif
endwhile

PS_CLOSE, pic_name,  /DEL_PS, FONT='lmroman', /V, /PDF







end





















;saving the data 
;put in lines 923 of anim_pandom then 

;        ;save data in ncdf for python experiment
;        nc_file = '/local/drive2/arma/armadja/data/lhn_test_outputs/pr_data_for_score_examples.nc'
;        if nn eq 0 then nc_id  = ncdf_create(nc_file, /clobber)              ;create file
;        if nn eq 0 then begin 
;            xdim  = ncdf_dimdef(nc_id, 'x' , nx)             ;define dimensions
;            ydim  = ncdf_dimdef(nc_id, 'y' , ny) 
;            lat_id= ncdf_vardef(nc_id, 'lat',  [xdim, ydim])  
;                    ncdf_attput,nc_id, lat_id, "description","latitude"
;                    ncdf_attput,nc_id, lat_id, "units", "deg"
;            lon_id= ncdf_vardef(nc_id, 'lon',  [xdim, ydim])  
;                    ncdf_attput,nc_id, lon_id, "description","longitude"
;                    ncdf_attput,nc_id, lon_id, "units", "deg"
;            ra_id = ncdf_vardef(nc_id, 'radar',  [xdim, ydim])  
;                    ncdf_attput,nc_id, ra_id, "description","precip rate"
;                    ncdf_attput,nc_id, ra_id, "units", "mm/h"
;            co_id = ncdf_vardef(nc_id, 'control',  [xdim, ydim])  
;                    ncdf_attput,nc_id, co_id, "description","precip rate"
;                    ncdf_attput,nc_id, co_id, "units", "mm/h"
;            lhn_id= ncdf_vardef(nc_id, 'lhn',  [xdim, ydim])  
;                    ncdf_attput,nc_id, lhn_id, "description","precip rate"
;                    ncdf_attput,nc_id, lhn_id, "units", "mm/h"
;            lno_id= ncdf_vardef(nc_id, 'lhnnow',  [xdim, ydim])  
;                    ncdf_attput,nc_id, lno_id, "description","precip rate"
;                    ncdf_attput,nc_id, lno_id, "units", "mm/h"
;            now_id= ncdf_vardef(nc_id, 'nowcast',  [xdim, ydim])  
;                    ncdf_attput,nc_id, now_id, "description","precip rate"
;                    ncdf_attput,nc_id, now_id, "units", "mm/h"
;            ncdf_control, nc_id, /endef                            ;close definition mode
;            ncdf_varput, nc_id, lat_id, exp_struc.lat[0:nx-1,0:ny-1]
;            ncdf_varput, nc_id, lon_id, exp_struc.lon[0:nx-1,0:ny-1]
;        endif
;        if nn gt 0 then nc_id  = ncdf_open(nc_file, /write)
;        if nn eq 0 then varid = ncdf_varid(nc_id,'control') 
;        if nn eq 1 then varid = ncdf_varid(nc_id,'lhn') 
;        if nn eq 2 then varid = ncdf_varid(nc_id,'lhnnow') 
;        if nn eq 3 then varid = ncdf_varid(nc_id,'nowcast') 
;        ncdf_varput, nc_id, varid, pr_acc[0:nx-1,0:ny-1]
;        ncdf_close, nc_id                                      ;close file



;and on line 1528
;    nc_file = '/local/drive2/arma/armadja/data/lhn_test_outputs/pr_data_for_score_examples.nc'
;    if nn eq 2 then begin
;        nc_id  = ncdf_open(nc_file, /write)
;        varid = ncdf_varid(nc_id,'radar') 
;        ncdf_varput, nc_id, varid, radar_mmh
;        ncdf_close, nc_id                                      ;close file
;    endif


