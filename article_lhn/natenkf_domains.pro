PRO NATENKF_DOMAINS
;make arrays for plotting different domains

pic_name = '~/documents/ps/natenkf_domains.ps'


;define figure size and position of plot in figure
pic_w = 2.*12.       
pic_h = 2.*8.        
rec_w = 2.*8./pic_w  
rec_h = 2.*6./pic_h  
sp_w = 2./pic_w      
x1 = 1.2/pic_w       
y1 = 1.2/pic_h       
pal_sp = 1./pic_w    
pal_w = .25/pic_w    

pos = [x1, y1, x1+rec_w, y1+rec_h]     

;open figure
PS_START, pic_name, pic_w, pic_h
    
    ;load map
    LOADCT, 0, /S
    MAPS, POS=pos, LOC='nat_enkf', /GRID, COL=0

    ;  red  ora  yel lblu dblu black
    ;   0    1   2   3    4    5
    r = [249,238,244,008, 008, 000]
    g = [087,150,061,109, 061, 000]
    b = [056,075,094,167, 119, 000]
    TVLCT, r,g,b

    ;;plot extended reg enkf
    ;print, 'extended reg'
    ;ext_renkf='/local/raid/armadja/data/domains/extended_regenkf.fst'
    ;PLOT_BORDER, FILE=ext_renkf, VAR='MSKC', COL=3
    ;XYOUTS, pos[0]+.2*rec_w,pos[1]+.15*rec_h, 'extention for radar DA', /NORMAL, COL=3, CHARSIZE=1.2
    ;print, ''

    ;plot joel's 10km map
    print, 'renkf'
    joels_10km='/local/raid/armadja/data/domains/10km_joel.fst'
    PLOT_BORDER, FILE=joels_10km, VAR='MSKC', COL=4
    XYOUTS, pos[0]+.2*rec_w,pos[1]+.95*rec_h, 'REnKF 10 km', /NORMAL, COL=4, CHARSIZE=1.2
    print, ''

    ;plot hrdps
    print, 'hrdps'
    hrdps='/local/raid/armadja/data/domains/hrdps.fst'
    PLOT_BORDER, FILE=hrdps, VAR='MSKC', COL=1, MAT_LAT=hrdps_lat, MAT_LON=hrdps_lon 
    XYOUTS, pos[0]+.2*rec_w,pos[1]+.85*rec_h, 'HRDPS', /NORMAL, COL=1, CHARSIZE=1.2
    print, ''

    ;hrenkf
    print, 'hrenkf'
    hrenkf='/local/raid/armadja/data/domains/hrenkf.fst'
    PLOT_BORDER, FILE=hrenkf, VAR='ME', COL=2
    XYOUTS, pos[0]+.2*rec_w,pos[1]+.55*rec_h, 'HREnKF', /NORMAL, COL=2, CHARSIZE=1.2
    print, ''


    ;plot extended panam
    print, 'panam'
    ext_panam= '/local/raid/armadja/data/domains/extended_panam.fst'
    PLOT_BORDER, FILE=ext_panam, VAR='ME', COL=0
    XYOUTS, pos[0]+.6*rec_w,pos[1]+.55*rec_h, 'Pan-Am', /NORMAL, COL=0, CHARSIZE=1.2
    print, ''

    ;plot meopar_east
    print, 'meopar east'
    east_file= '/local/raid/armadja/data/domains/meopar_east.nc'
    nc_id = NCDF_OPEN(east_file)
    NCDF_VARGET, nc_id, 'nav_lat', mat_lat
    NCDF_VARGET, nc_id, 'nav_lon', mat_lon
    NCDF_CLOSE, nc_id
    help, mat_lat
    PLOT_BORDER, MAT_LAT=mat_lat, MAT_LON=mat_lon, COL=5
    XYOUTS, pos[0]+.75*rec_w,pos[1]+.8*rec_h, 'MEOPAR east', /NORMAL, COL=5, CHARSIZE=1.0
    print, ''

    ;plot Seung-Jong meopar east
    print, 'meopar east 2.5 km'
    hrdps='/users/dor/arma/gr7/data/cetus2/work/grid_definition/grid_meopar_east_2.5km_N53'
    PLOT_BORDER, FILE=hrdps, VAR='MSKC', COL=2
    XYOUTS, pos[0]+.75*rec_w,pos[1]+.9*rec_h, 'East 2.5 km', /NORMAL, COL=2, CHARSIZE=1.2
    print, ''

    print, 'meopar east 1 km'
    hrdps='/users/dor/arma/gr7/data/cetus2/work/grid_definition/grid_meopar_east_1km_N53'
    PLOT_BORDER, FILE=hrdps, VAR='MSKC', COL=1
    XYOUTS, pos[0]+.75*rec_w,pos[1]+.9*rec_h, 'East 1 km', /NORMAL, COL=1, CHARSIZE=1.2
    print, ''

    ;plot meopar_west
    print, 'meopar west'
    west_file= '/local/raid/armadja/data/domains/meopar_west.nc'
    nc_id = NCDF_OPEN(west_file)
    NCDF_VARGET, nc_id, 'nav_lat', mat_lat
    NCDF_VARGET, nc_id, 'nav_lon', mat_lon
    NCDF_CLOSE, nc_id
    help, mat_lat
    PLOT_BORDER, MAT_LAT=mat_lat, MAT_LON=mat_lon, COL=5
    XYOUTS, pos[0]+.1*rec_w,pos[1]+.35*rec_h, 'MEOPAR west', /NORMAL, COL=5, CHARSIZE=1.0
    print, ''


    ;plot domain for bernat
    LOADCT,40,/S
        ;first domain
        ;PLOTS,[-77.1410],[43.1758], PSYM=8, COL=80
        ;PLOTS,[-70.7338],[47.6724], PSYM=8, COL=80
        ;xx0=1676
        ;xxf=2100
        ;yy0=326
        ;yyf=750
    ;second domain
    ;PLOTS,[-107.931],[48.0321], PSYM=8, COL=80
    ;PLOTS,[-100.893],[52.5287], PSYM=8, COL=80

    ;1961 2494 328 1011
    ;1878 2530 335 1161 ;extended domain for meopar exchange
    xx0=1878-1
    xxf=2530-1
    yy0=335-1
    yyf=1161-1
    help, hrdps_lat
    sub_lat = hrdps_lat[xx0:xxf,yy0:yyf]
    help, sub_lat
    sub_lon = hrdps_lon[xx0:xxf,yy0:yyf]
    PLOT_BORDER, MAT_LAT=sub_lat, MAT_LON=sub_lon, COL=80



    ;;extended 2.5 grid for radar composites
    ;radar_grid = '/local/raid/armadja/data/domains/radar_continental_2.5km.fst'
    ;PLOT_BORDER, FILE=radar_grid, VAR='MSKC', COL=40



;close img
PS_CLOSE, pic_name, /V, /PDF, /DEL_PS, FONT='lmroman'    


    ;GET_GEM_DATA, radar_grid, LAT=mat_lat, LON=mat_lon, GETVAR='MSKC'
    ;sz = SIZE(mat_lat, /DIM)
    ;nx = sz[0]
    ;ny = sz[1]
    ;;save ncdf in matrix
    ;nc_name='/users/dor/arma/dja/desktop/lat_lon_continental_radar_domain_2.5.nc'
    ;f_id   = NCDF_CREATE(nc_name, /CLOBBER)               ;create file
    ;xdim   = NCDF_DIMDEF(f_id, 'x', nx)                   ;define dimensions
    ;ydim   = NCDF_DIMDEF(f_id, 'y', ny) 
    ;lat_id = NCDF_VARDEF(f_id, 'latitude',  [xdim, ydim])  ;define variables
    ;NCDF_ATTPUT, f_id, lat_id, 'description', 'Latitude'
    ;NCDF_ATTPUT, f_id, lat_id, 'units', 'degrees'
    ;lon_id = NCDF_VARDEF(f_id, 'longitude', [xdim, ydim]) 
    ;NCDF_ATTPUT, f_id, lon_id, 'description', 'Longitude'
    ;NCDF_ATTPUT, f_id, lon_id, 'units', 'degrees [-180, 180]'
    ;NCDF_CONTROL, f_id, /endef                            ;close definition mode
    ;NCDF_VARPUT, f_id, lat_id, mat_lat                    ;put variables in file
    ;NCDF_VARPUT, f_id, lon_id, mat_lon
    ;NCDF_CLOSE, f_id                                      ;close file

END 

