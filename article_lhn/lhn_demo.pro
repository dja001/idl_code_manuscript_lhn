PRO LHN_DEMO
;demo showing a profile and scales profile for LHN

;read model precip rates & LHN 
file = '/local/drive2/arma/armadja/data/lhn_demo_output/2015052500_006'
date_jul   = JULDAY(05,25,2015,06,00,00)
date_jul_m = JULDAY(05,25,2015,05,55,00)
JUL_to_cmc, date_jul, date_cmc
JUL_to_cmc, date_jul_m, date_cmc_m
missing = -9999.

GET_GEM_DATA, file, LAT=nat_lat, LON=nat_lon, GETVAR='RT'
GET_GEM_DATA, file, VAR_NAME='RT', VALUES=rt_val, CMC_TIMESTAMP=date_cmc
GET_GEM_DATA, file, VAR_NAME='PR', VALUES=pr_t, CMC_TIMESTAMP=date_cmc
GET_GEM_DATA, file, VAR_NAME='PR', VALUES=pr_t_m, CMC_TIMESTAMP=date_cmc_m
dpr_val = (pr_t - pr_t_m)*1e3
;dpr_val[200:400,200:400] = dpr_val[200:400,200:400] *0. + 3.
rt_val *= 1e3
MAXMIN,rt_val, NAME='RT'
MAXMIN,dpr_val, NAME='dpr'
;GET_GEM_DATA, file, VAR_NAME='T3', VALUES=t3_val, CMC_TIMESTAMP=date_cmc
GET_GEM_DATA, file, VAR_NAME='TA', VALUES=ta_val, CMC_TIMESTAMP=date_cmc, P_FROM_VAR=ta_pres
ta_val *= 60.
help, rt_val, ta_val

;determine nx and ny from latitude matrix
sz = SIZE(nat_lat, /DIM)
nat_nx = sz[0]
nat_ny = sz[1]

;get radar data
radar_file_t = '/local/drive2/arma/armadja/data/radar_nat_composites/raw_data/operation.radar.Composite-USACDN-4km.precipet.std-rpn/2015052506_00ref_4km.stnd'
;read radar data
GET_GEM_DATA, radar_file_t,   VAR_NAME='RRAI', VALUES=radar_r,    CMC_TIMESTAMP=date_cmc
radar_r /= 12.
;get lat/lon of radar grid
GET_GEM_DATA, radar_file_t, GETVAR='RRAI', LAT=rad_lat, LON=rad_lon
;size of radar output
sz = SIZE(radar_r,/DIM)
rad_nx = sz[0]
rad_ny = sz[1]

loc = 'nat_enkf'
;loc = 'south_quebec'
nat_ind = '~/documents/idl_sav_files/lhn_demo.sav'

print, 'Nat domain ','nx ', nat_nx, ' ny ', nat_ny
    ;;Z buffer stuff to set projection indices
    ;SET_PLOT, 'Z'
    ;; projection image size in pixels
    ;sz_zbuf = [1200,768]     ;data square smaller than this because of boundaries
    ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
    ;DEVICE, DECOMPOSED=0
    ;;map projection
    ;sub_domain = [0,0.,1.,1.]
    ;    MAPS, POS=sub_domain, LATS=nat_lat, LONS=nat_lon, MISSING=missing, PROJ_IND=proj_ind, LOC=loc
    ;        ;mis_proj_up
    ;        arr_lat=nat_lat[*,nat_ny-1]
    ;        arr_lon=nat_lon[*,nat_ny-1]
    ;        nn=5
    ;        mis_lon=REBIN(arr_lon,nat_nx,nn)
    ;        mis_lat=FLTARR(nat_nx,nn)
    ;        FOR ii=0D,nn-1 DO mis_lat[*,ii] = arr_lat + .5*ii
    ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_up, LOC=loc
    ;        missing_up = WHERE(proj_mis_up NE missing, nmis)
    ;        IF nmis NE 0 THEN proj_ind[missing_up]=missing
    ;        ;mis_proj_down
    ;        arr_lat=nat_lat[*,0]
    ;        arr_lon=nat_lon[*,0]
    ;        nn=5
    ;        mis_lon=REBIN(arr_lon,nat_nx,nn)
    ;        mis_lat=FLTARR(nat_nx,nn)
    ;        FOR ii=nn-1,0,-1 DO mis_lat[*,ii] = arr_lat - .5*ii
    ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_do, LOC=loc
    ;        missing_do = WHERE(proj_mis_do NE missing, nmis)
    ;        IF nmis NE 0 THEN proj_ind[missing_do]=missing
    ;    ;save projection indices
    ;    SAVE, proj_ind, FILENAME=nat_ind, /COMPRESS
    ;DEVICE, Z_BUFFERING = 0
RESTORE, nat_ind;, /VERBOSE

rad_ind = '~/documents/idl_sav_files/lhn_demo_rad.sav'
    ;;Z buffer stuff to set projection indices
    ;SET_PLOT, 'Z'
    ;; projection image size in pixels
    ;sz_zbuf = [1200,768]     ;data square smaller than this because of boundaries
    ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
    ;DEVICE, DECOMPOSED=0
    ;;map projection
    ;sub_domain = [0,0.,1.,1.]
    ;    MAPS, POS=sub_domain, LATS=rad_lat, LONS=rad_lon, MISSING=missing, PROJ_IND=rad_proj_ind, LOC=loc
    ;    ;save projection indices
    ;    SAVE, rad_proj_ind, FILENAME=rad_ind, /COMPRESS
    ;DEVICE, Z_BUFFERING = 0
RESTORE, rad_ind;, /VERBOSE


;pt_lon = -98.
;pt_lat = 46.
pt_lon = -98.
pt_lat = 46.5

aa = MIN(((nat_lon-pt_lon)^2. + (nat_lat-pt_lat)^2.),mpos)
my = FLOOR(mpos/nat_nx)
mx = mpos - my*nat_nx

ta_vect   = REFORM(ta_val[mx,my,*])
pres_vect = REFORM(ta_pres[mx,my,*])

print, 'model rain rate', dpr_val[mx,my], 'mm in 5 minutes'


aa = MIN(((rad_lon-pt_lon)^2. + (rad_lat-pt_lat)^2.),mpos)
rmy = FLOOR(mpos/rad_nx)
rmx = mpos - rmy*rad_nx
print, 'radar rain rate', radar_r[rmx,rmy], 'mm in 5 minutes'




; Display the data.
;setup image
pic_w = 42.
pic_h = 12. 
pal_sp = .9/pic_w
pal_w = .2/pic_w
rec_w = 12.5/pic_w
rec_h = 8./pic_h
sq_h = 8./pic_h
sq_w = 8./pic_w
sp_w = 2./pic_w
sq = 1./pic_h
x0 = .99/pic_w
x1 = x0 + rec_w + sp_w
x2 = x1 + rec_w + 2.*sp_w
x3 = x2 + rec_w + sp_w
y0 = .99/pic_h
y1 = y0 + rec_h

LEGS, RANGE=[0.,1.], $
      N_COL=6, $
      EXCEP_VAL=[missing,0.], EXCEP_COL=['grey_220','white'],   $
      OVER_HIGH='extend', UNDER_LOW='white', $
      MAPPING=mapping_rt

pic_name = '~/documents/ps/lhn_demo.ps'
PS_START, pic_name, pic_w, pic_h

;plot 
pos = [x0, y0, x0+rec_w, y1]
XYOUTS, x0+.3*rec_w, y1+0.1*rec_h, 'Model precip rate', /NORMAL, CHARSIZE=1.5
APPLY_PROJ, dpr_val,  proj_ind, proj_rt, MISSING=missing
;apply color mapping
LEGS, DATA=proj_rt, MAPPING=mapping_rt, TV_OUT=pos
;overlay grid
LOADCT,0,/S
MAPS, POS=pos, /GRID, /MAP, LOC=loc, COL=0
PLOT_BORDER, MAT_LAT=nat_lat, MAT_LON=nat_lon, COL=0
;pt
LOADCT, 40, /S
PLOTS, [nat_lon[mx,my]], [nat_lat[mx,my]], PSYM=7, COL=210, SYMSIZE=.5


;plot 
pos = [x1, y0, x1+rec_w, y1]
XYOUTS, x0+.3*rec_w, y1+0.1*rec_h, 'Model precip rate', /NORMAL, CHARSIZE=1.5
APPLY_PROJ, radar_r,  rad_proj_ind, proj_rad, MISSING=missing
;apply color mapping
LEGS, DATA=proj_rad, MAPPING=mapping_rt, TV_OUT=pos
;overlay grid
LOADCT,0,/S
MAPS, POS=pos, /GRID, /MAP, LOC=loc, COL=0
;pt
LOADCT, 40, /S
PLOTS, [nat_lon[mx,my]], [nat_lat[mx,my]], PSYM=7, COL=210, SYMSIZE=.5



    ;palette
    pos = [x1+rec_w+pal_sp, y0, x1+rec_w+pal_sp+pal_w, y1]
    LEGS, MAPPING=mapping_rt, PALETTE=pos, UNITS='mm/5min'





;LHN profile plot
pos = [x2, y0, x2+sq_w, y1]
print, pos
LOADCT, 40, /S
PLOT, [0], [0], /NODATA, /NOERASE, /NORMAL, POS=pos, $
    XS=1, XR=[-.005, 0.09], XTITLE='Heating rate [K/min]', XMINOR=1, $
    YS=1, YR=[1000.,100.], YTITLE='Height [hPa]', /YLOG, YTICKLEN=0.04
OPLOT, [0.,0.], [1000.,100.], COL=0, TH=.5
OPLOT, ta_vect, pres_vect, COL=210







PS_CLOSE, pic_name, /PDF, /DEL_PS, FONT='lmroman', /VERBOSE










END
