PRO PLOT_RR, model_r, radar_r, rr_diff, missing, loc

    ;projection of model data on common grid
    mod_ind='~/documents/idl_sav_files/lhn_mod.sav'
        ;;Z buffer stuff to set projection indices
        ;SET_PLOT, 'Z'
        ;; projection image size in pixels
        ;sz_zbuf = [800,800]     ;data square smaller than this because of boundaries
        ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
        ;DEVICE, DECOMPOSED=0
        ;;map projection
        ;sub_domain = [0,0.,1.,1.]
        ;    MAPS, POS=sub_domain, LATS=mod_lat, LONS=mod_lon, MISSING=missing, PROJ_IND=proj_ind_mod, LOC=loc
        ;        ;mis_proj_up
        ;        arr_lat=mod_lat[*,mod_ny-1]
        ;        arr_lon=mod_lon[*,mod_ny-1]
        ;        nn=5
        ;        mis_lon=REBIN(arr_lon,mod_nx,nn)
        ;        mis_lat=FLTARR(mod_nx,nn)
        ;        FOR ii=0D,nn-1 DO mis_lat[*,ii] = arr_lat + .5*ii
        ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_up, LOC=loc
        ;        missing_up = WHERE(proj_mis_up NE missing, nmis)
        ;        IF nmis NE 0 THEN proj_ind_mod[missing_up]=missing
        ;        ;mis_proj_down
        ;        arr_lat=mod_lat[*,0]
        ;        arr_lon=mod_lon[*,0]
        ;        nn=5
        ;        mis_lon=REBIN(arr_lon,mod_nx,nn)
        ;        mis_lat=FLTARR(mod_nx,nn)
        ;        FOR ii=nn-1,0,-1 DO mis_lat[*,ii] = arr_lat - .5*ii
        ;        MAPS, POS=sub_domain, LATS=mis_lat, LONS=mis_lon, MISSING=missing, PROJ_IND=proj_mis_do, LOC=loc
        ;        missing_do = WHERE(proj_mis_do NE missing, nmis)
        ;        IF nmis NE 0 THEN proj_ind_mod[missing_do]=missing
        ;    ;save projection indices
        ;    help, proj_ind_mod
        ;    SAVE, proj_ind_mod, FILENAME=mod_ind, /COMPRESS
        ;DEVICE, Z_BUFFERING = 0
    RESTORE, mod_ind;, /VERBOSE
    
    ;projection of radar data on common grid
    rad_ind='~/documents/idl_sav_files/lhn_rad.sav'
        ;;Z buffer stuff to set projection indices
        ;SET_PLOT, 'Z'
        ;; projection image size in pixels
        ;sz_zbuf = [800,800]     ;data square smaller than this because of boundaries
        ;DEVICE, SET_RESOLUTION=sz_zbuf, SET_PIXEL_DEPTH=24
        ;DEVICE, DECOMPOSED=0
        ;;map projection
        ;sub_domain = [0,0.,1.,1.]
        ;    MAPS, POS=sub_domain, LATS=rad_lat, LONS=rad_lon, MISSING=missing, PROJ_IND=proj_ind_rad, LOC=loc
        ;    ;save projection indices
        ;    help, proj_ind_rad
        ;    SAVE, proj_ind_rad, FILENAME=rad_ind, /COMPRESS
        ;DEVICE, Z_BUFFERING = 0
    RESTORE, rad_ind;, /VERBOSE

    ;compute precip diff and factor for profile adjustment
    APPLY_PROJ, model_r, proj_ind_mod, proj_model_r, MISSING=missing
    APPLY_PROJ, radar_r, proj_ind_rad, proj_radar_r, MISSING=missing
    APPLY_PROJ, rr_diff, proj_ind_mod, proj_rr_diff, MISSING=missing

    ;color mapping for rr
    base = 5.
    range = [base/32.,base/16.,base/8.,base/4.,base/2.,base]
    LEGS, RANGE=range, $
          COLOR_ARR=['blue','green','orange','red','pink'], $
          EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-2], $
          OVER_HIGH='extend', UNDER_LOW='white',   $
          MAPPING=mapping_rr
    ;color mapping for difference
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
     LEGS, RANGE=[-1.,1.]*200., $
           COLOR_ARR=blue_orange, SOLID='supplied', $ 
           EXCEP_VAL=[missing,-3000.,0.], EXCEP_COL=['grey_230','brown','white'], EXCEP_TOL=[1e-3,1.,1e-3], $
           OVER_HIGH='extend', UNDER_LOW='extend', $
           MAPPING=mapping_diff

    ;set up image coords setup 
    sq_w = 10.
    sq_h = .8*sq_w
    pic_h = 15.
    pic_w = 15.*3
    pal_sp = 1.3/pic_w
    pal_w = .25/pic_w
    rec_w = sq_w/pic_w
    rec_h = sq_h/pic_h
    sp_w = 2./pic_w
    sp_h = 2./pic_h
    x1 = 2.2/pic_w 
    x2 = x1 + rec_w + sp_w
    x3 = x2 + rec_w + sp_w + pal_w + sp_w
    y1 = 1./pic_h
    y2 = y1 + rec_h + sp_h
    
    y0 = y1
    pic_dir = '~/documents/ps/'
    pic_name = pic_dir+'lhn.ps'
    PS_START, pic_name, pic_w, pic_h
        ;plot model rr
        x0 = x1
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;plot image
        LEGS, DATA=proj_model_r, MAPPING=mapping_rr,  TV_OUT=pos
        ;title
        XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'Model',/NORMAL,CHARSIZE=2., COL=0
        ;overlay grid
        MAPS, POS=pos, /GRID, /MAP, LOC=loc
        ;PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon

        ;plot radar rr
        x0 = x2
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;plot image
        LEGS, DATA=proj_radar_r, MAPPING=mapping_rr,  TV_OUT=pos
        ;title
        XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'Radar',/NORMAL,CHARSIZE=2., COL=0
        ;overlay grid
        MAPS, POS=pos, /GRID, /MAP, LOC=loc
        ;PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon

        ;plot palette
        x0 = x0 + rec_w + sp_w
        pos = [x0,y0,x0+pal_w,y0+rec_h]
        ;plot image
        LEGS, MAPPING=mapping_rr, PALETTE=pos, UNITS='mm/h', PAL_PROP='equal'

        ;plot diff
        x0 = x3
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;plot image
        LEGS, DATA=proj_rr_diff, MAPPING=mapping_rr,  TV_OUT=pos
        ;title
        XYOUTS, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'Radar',/NORMAL,CHARSIZE=2., COL=0
        ;overlay grid
        MAPS, POS=pos, /GRID, /MAP, LOC=loc
        ;PLOT_BORDER, MAT_LAT=nat_str[jj].lat, MAT_LON=nat_str[jj].lon

        ;plot palette
        x0 = x0 + rec_w + sp_w
        pos = [x0,y0,x0+pal_w,y0+rec_h]
        ;plot image
        LEGS, MAPPING=mapping_rr, PALETTE=pos, UNITS='mm/h';, PAL_PROP='equal'



    PS_CLOSE, pic_name, /DEL_PS, FONT='lmroman', /V, /PDF

END


PRO LHN_IN_IDL

;read parameter file
model_file_t  ='/local/drive2/arma/armadja/data/recycle_outputs/nad/2015052200_087'
time_t_cmc   = 402458300L
model_file_mdt='/local/drive2/arma/armadja/data/recycle_outputs/nad/2015052200_087'
time_mdt_cmc = 402458224L
model_dt = 5.       ;model dt in minutes

radar_file_t = '/local/drive2/arma/armadja/data/radar_nat_composites/raw_data/operation.radar.Composite-USACDN-4km.precipet.std-rpn/2015052515_00ref_4km.stnd'

;internal variables & parameters
missing=-999.
loc = 'nat_enkf'


;read radar data
GET_GEM_DATA, radar_file_t,   VAR_NAME='RRAI', VALUES=radar_r,    CMC_TIMESTAMP=time_t_cmc
;get lat/lon of radar grid
GET_GEM_DATA, radar_file_t, GETVAR='RRAI', LAT=rad_lat, LON=rad_lon
;size of radar output
sz = SIZE(radar_r,/DIM)
rad_nx = sz[0]
rad_ny = sz[1]

;read model output
;get 3D profiles of T and HU
GET_GEM_DATA, model_file_t,   VAR_NAME='TT', VALUES=model_tt,     CMC_TIMESTAMP=time_t_cmc
GET_GEM_DATA, model_file_t,   VAR_NAME='HU', VALUES=model_hu,     CMC_TIMESTAMP=time_t_cmc
;size of model output
sz = SIZE(model_tt,/DIM)
mod_nx = sz[0]
mod_ny = sz[1]
mod_nz = sz[2]
;get lat/lon of model grid
GET_GEM_DATA, model_file_t, GETVAR='TT', LAT=mod_lat, LON=mod_lon
;read model precip
GET_GEM_DATA, model_file_t,   VAR_NAME='PR', VALUES=model_pr_t,   CMC_TIMESTAMP=time_t_cmc
ON_MODEL_GRID, model_pr_t,   mod_nx, mod_ny, missing
GET_GEM_DATA, model_file_mdt, VAR_NAME='PR', VALUES=model_pr_mdt, CMC_TIMESTAMP=time_mdt_cmc
ON_MODEL_GRID, model_pr_mdt, mod_nx, mod_ny, missing
;compute precip rate
model_r = (model_pr_t - model_pr_mdt) / model_dt
;convert to mm/h
model_r = model_r * 1e3 * 60.       ;*1000 = mm/minute   *60. = mm/hour

;distance of radar grid points to nearest radar
dist_savename='~/documents/idl_sav_files/lhn_dist.sav'
    ;R_POS, r_pos    ;load a structure containing lat lon of all north-american radars
    ;n_rad = N_ELEMENTS(r_pos)
    ;;for every point in the radar grid
    ;d_to_nearest_rad = REPLICATE(missing,rad_nx,rad_ny)
    ;;FOR ii=0, rad_nx-1 DO BEGIN
    ;;    FOR jj=0, rad_ny-1 DO BEGIN
    ;FOR ii=400, 800 DO BEGIN
    ;    FOR jj=400, 600 DO BEGIN
    ;        r_dist = FLTARR(n_rad)
    ;        FOR nn=0, n_rad-1 DO BEGIN
    ;            r_dist[nn] = MAP_2POINTS(rad_lon[ii,jj],rad_lat[ii,jj],r_pos[nn].lon,r_pos[nn].lat,/METERS) / 1e3  ;/1000 for answer in km
    ;        ENDFOR
    ;        d_to_nearest_rad[ii,jj] = MIN(r_dist)
    ;    ENDFOR
    ;    print, ii, ' of ', rad_nx-1
    ;ENDFOR
    ;SAVE, d_to_nearest_rad, FILENAME=dist_savename
RESTORE, dist_savename;, /VERBOSE

;define projection of radar point to model grid
near_savename='~/documents/idl_sav_files/lhn_near.sav'
    ;mod_ind = REPLICATE(LONG(missing),rad_nx,rad_ny)
    ;FOR ii=0, rad_nx-1 DO BEGIN
    ;    FOR jj=0, rad_ny-1 DO BEGIN
    ;;FOR ii=800, 850 DO BEGIN
    ;;    FOR jj=850, 866 DO BEGIN
    ;        IF (d_to_nearest_rad[ii,jj] GT 0.) AND (d_to_nearest_rad[ii,jj] LE 200.) THEN BEGIN
    ;            ;point is within 200km of a radar
    ;            this_dist = (mod_lat - rad_lat[ii,jj])^2. + (mod_lon - rad_lon[ii,jj])^2.
    ;            aa = MIN(this_dist,min_pos)
    ;            mod_ind[ii,jj] = min_pos
    ;        ENDIF ELSE BEGIN
    ;            ;point is more than 200 km away from nearest radar
    ;            mod_ind[ii,jj] = missing
    ;        ENDELSE
    ;    ENDFOR
    ;    print, ii, ' of ', rad_nx-1
    ;ENDFOR
    ;SAVE, mod_ind, FILENAME=near_savename
RESTORE, near_savename;, /VERBOSE

;replace no data with 0. 
aa = WHERE(radar_r EQ missing, caa)
IF caa NE 0 THEN radar_r[aa] = 0.

;apply the projection for radar data points
print, 'projecting radar data onto model grid'
res   = FLTARR(mod_nx,mod_ny)   
count = LONARR(mod_nx,mod_ny)
FOR ii=0, rad_nx-1 DO BEGIN
    FOR jj=0, rad_ny-1 DO BEGIN
        IF mod_ind[ii,jj] NE missing THEN BEGIN
            res[mod_ind[ii,jj]] += radar_r[ii,jj]
            count[mod_ind[ii,jj]] += 1
        ENDIF 
    ENDFOR
    print, ii, ' of ', rad_nx-1
ENDFOR
print, 'done'
aa = WHERE(count GT 0, caa, COMPLEMENT=bb, NCOMPLEMENT=cbb)
IF caa NE 0 THEN res[aa] = res[aa] / count[aa]      ;res is radar data composited onto the model grid
IF cbb NE 0 THEN res[bb] = missing

PLOT_RR, model_r, radar_r, res, missing, loc











;modulate adjustments with various restrictions

;modulate profiles

;compute increments and make IAU








END
