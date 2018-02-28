PRO ODIM_READ_TEST
;experiment with reading hdf files



file = '/users/dor/arma/dja/desktop/qcomp_20150522T0020Z.h5'
;H5_LIST, file  ;;requires idl 8.3
;Result = H5_BROWSER(file)
;stop

file_id = H5F_OPEN(file)

; Open the image dataset within the file.
dataset_id1 = H5D_OPEN(file_id,'/dataset1/data1/data')
att_id1 = H5A_OPEN_NAME(dataset_id1,'CLASS')

dataset_id2 = H5D_OPEN(file_id,'/dataset1/data1/quality6/data')

;dataset_id2 = H5G_OPEN(file_id,'/dataset1/data1/what')
;att_id2 = H5A_OPEN_NAME(dataset_id2,'gain')
;help, dataset_id2

; Read data
r_data  = H5D_READ(dataset_id1)
r_qi    = H5D_READ(dataset_id2)
;flip vertically
r_data  = ROTATE(r_data,7)
r_qi    = ROTATE(r_qi  ,7)


;att =  H5A_READ(att_id1)
;help, att
;
;att2 =  H5A_READ(att_id2)
;help, att2
;
;; Open up the dataspace 
;dataspace_id = H5D_GET_SPACE(dataset_id1)
;help, dataspace_id
;; Retrieve the dimensions 
;dimensions = H5S_GET_SIMPLE_EXTENT_DIMS(dataspace_id)
;print, dimensions

; Close all our identifiers so we don't leak resources.
;
;H5S_CLOSE, dataspace_id
H5D_CLOSE, dataset_id1
H5D_CLOSE, dataset_id2
H5F_CLOSE, file_id

;apply transformation to radar data
nodata = 255
nodat_pos = WHERE(r_data EQ nodata, caa)
;scale and offset
scale = .5
offset = -32.
r_data = r_data*scale + offset
IF caa NE 0 THEN r_data[nodat_pos] = nodata

;apply transformation to qi 
nodata = 255
nodat_pos = WHERE(r_qi EQ nodata, caa)
r_qi = FLOAT(r_qi)/254.
IF caa NE 0 THEN r_qi[nodat_pos] = nodata


print, 'for data1'
help, r_data
MAXMIN, r_data, NAME='data1'

print, 'for qi'
help, r_qi
MAXMIN, r_qi, NAME='qi'


loc = 'lhn_rdps'
;loc = 'south_quebec'
rad_ind = '~/documents/idl_sav_files/odim_read_test.sav'

;;get lat lon from standard file
;;nat_files = FILE_SEARCH('/local/drive2/arma/armadja/data/recycle_outputs/naa/*', COUNT=n_nat)
;;IF n_nat EQ 0 THEN MESSAGE, 'No files found in: /local/drive2/arma/armadja/data/recycle_outputs/naa/*'
dom_file='/local/drive2/arma/armadja/data/domains/radar_continental_2.5km.fst'
GET_GEM_DATA, dom_file, LAT=rad_lat, LON=rad_lon, GETVAR='MSKC'
;determine nx and ny from latitude matrix

    


sz = SIZE(rad_lat, /DIM)
rad_nx = sz[0]
rad_ny = sz[1]
print, 'Nat domain ','nx ', rad_nx, ' ny ', rad_ny
    ;Z buffer stuff to set projection indices
    SET_PLOT, 'Z'
    ; projection image size in pixels
    sz_zbuf = [1200,768]     ;data square smaller than this because of boundaries
    DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
    DEVICE, DECOMPOSED=0
    ;map projection
    sub_domain = [0,0.,1.,1.]
        ;MAPS, POS=sub_domain, LATS=rad_lat, LONS=rad_lon, MISSING=missing, PROJ_IND=proj_ind, LOC=loc
        ;    ;mis_proj_up
        ;    arr_lat=rad_lat[*,rad_ny-1]
        ;    arr_lon=rad_lon[*,rad_ny-1]
        ;    nn=5
        ;    mis_lon=REBIN(arr_lon,rad_nx,nn)
        ;    mis_lat=FLTARR(rad_nx,nn)
        ;    FOR ii=0D,nn-1 DO mis_lat[*,ii] = arr_lat + .5*ii
        ;    MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_up, LOC=loc
        ;    missing_up = WHERE(proj_mis_up NE missing, nmis)
        ;    IF nmis NE 0 THEN proj_ind[missing_up]=missing
        ;    ;mis_proj_down
        ;    arr_lat=rad_lat[*,0]
        ;    arr_lon=rad_lon[*,0]
        ;    nn=5
        ;    mis_lon=REBIN(arr_lon,rad_nx,nn)
        ;    mis_lat=FLTARR(rad_nx,nn)
        ;    FOR ii=nn-1,0,-1 DO mis_lat[*,ii] = arr_lat - .5*ii
        ;    MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_do, LOC=loc
        ;    missing_do = WHERE(proj_mis_do NE missing, nmis)
        ;    IF nmis NE 0 THEN proj_ind[missing_do]=missing
        ;;save projection indices
        ;SAVE, proj_ind, FILENAME=rad_ind, /COMPRESS
    DEVICE, Z_BUFFERING = 0
RESTORE, rad_ind;, /VERBOSE








; Display the data.
;setup image
pic_w = 32.
pic_h = 12. 
pal_sp = .9/pic_w
pal_w = .2/pic_w
rec_w = 12.5/pic_w
rec_h = 8./pic_h
sq = 1./pic_h
x0 = .99/pic_w
x1 = x0 + rec_w
x2 = x1 + 3./pic_w
x3 = x2 + rec_w
y0 = .99/pic_h
y1 = y0 + rec_h

missing = 255
LEGS, RANGE=[0.,60.], $
      N_COL=6, $
      EXCEP_VAL=missing, EXCEP_COL='white',   $
      OVER_HIGH='extend', UNDER_LOW='extend', $
      MAPPING=mapping_ref
LEGS, RANGE=[0.,1.], $
      COLOR_ARR=[[255,255,255],[000, 000, 000]], DARK_POS='low', $
      EXCEP_VAL=[missing,0.], EXCEP_COL=['white','white'],   $
      OVER_UNDER='exact', $
      MAPPING=mapping_qi

pic_name = '~/documents/ps/odim_read_test.ps'
PS_START, pic_name, pic_w, pic_h

;plot odim data
pos = [x0, y0, x1, y1]
XYOUTS, x0+.2*rec_w, y1+0.1*rec_h, 'QCed reflectivity composite', /NORMAL, CHARSIZE=1.5
APPLY_PROJ, r_data,  proj_ind, proj_r_data, MISSING=missing
;apply color mapping
LEGS, DATA=proj_r_data, MAPPING=mapping_ref, TV_OUT=pos
;overlay grid
LOADCT,0,/S
MAPS, POS=pos, /GRID, /MAP, LOC=loc, COL=0
PLOT_BORDER, MAT_LAT=rad_lat, MAT_LON=rad_lon, COL=0, TH=.5
    ;palette
    pos = [x1+pal_sp, y0, x1+pal_sp+pal_w, y1]
    LEGS, MAPPING=mapping_ref, PALETTE=pos, UNITS='dBZ'

;plot odim data2
pos = [x2, y0, x3, y1]
XYOUTS, x2+.3*rec_w, y1+0.1*rec_h, 'Quality index', /NORMAL, CHARSIZE=1.5
APPLY_PROJ, r_qi,  proj_ind, proj_r_qi, MISSING=missing
;apply color mapping
LEGS, DATA=proj_r_qi, MAPPING=mapping_qi, TV_OUT=pos
;overlay grid
LOADCT,0,/S
MAPS, POS=pos, /GRID, /MAP, LOC=loc, COL=0
PLOT_BORDER, MAT_LAT=rad_lat, MAT_LON=rad_lon, COL=0, TH=.5
    ;palette
    pos = [x3+pal_sp, y0, x3+pal_sp+pal_w, y1]
    LEGS, MAPPING=mapping_qi, PALETTE=pos, UNITS='[unitless]'

;;plot mcgill data
;pos = [x2, y0, x3, y1]
;APPLY_PROJ, ref_cart,  proj_r_ind, proj_mcgill_data, MISSING=missing
;;apply color mapping
;LEGS, DATA=proj_mcgill_data, MAPPING=mapping_ref, TV_OUT=pos
;;overlay grid
;LOADCT,0,/S
;MAPS, POS=pos, /GRID, /MAP, LOC=loc, COL=0
;;PLOT_BORDER, MAT_LAT=nat_lat, MAT_LON=nat_lon
;PLOT_BORDER, MAT_LAT=nat_lat, MAT_LON=nat_lon, COL=0
;    ;palette
;    pos = [x1+pal_sp, y0, x1+pal_sp+pal_w, y1]
;    LEGS, MAPPING=mapping_ref, PALETTE=pos, UNITS='dBZ'

PS_CLOSE, pic_name, /PDF, /DEL_PS, FONT='lmroman', /VERBOSE










END
