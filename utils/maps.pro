pro maps, hrenkf_west=hrenkf_west,                             $     ;preset projections
          hrenkf_rot=hrenkf_rot,                               $
          renkf=renkf,                                         $
          can_us_radars=can_us_radars,                         $ 
          vancouver=vancouver,                                 $
          wrfens=wrfens,                                       $
          loc=loc,                                             $
          grid=grid, map=map,                                  $     ;keyword to plot latlon grid + topo
          pos=pos, lats=lats, lons=lons, missing=missing,      $     ;input data for the generation of indices
          col=col,charsize=charsize,                           $
          proj_ind=proj_ind, sz_zbuf=sz_zbuf,                  $      ;output indices
          test=test,ratio=ratio,rec_w=rec_w,rec_h=rec_h               ;generate a figure with domain begin defined

    ; this routine can be used to generate projection indices or just to output map features
    ; the keyword test is used to make figure with map details

    ;if sz_zbuf not provided, assume 800x800
    if n_elements(sz_zbuf) eq 0 then begin
        sz_zbuf = [800,800]
    endif

    ;put something if proj is not defined
    if n_elements(loc) eq 0 then begin
        loc = 'undefined'
    endif

    ;by default plot boundary in black
    if n_elements(col) eq 0 then begin
        loadct, 40, /s
        col=255
    endif

    if keyword_set(test) then begin
        ;specify that we want grid and map
        grid=1
        map=1
        ;set up image coords setup 
        pic_h = 12.
        pic_w  = 15.
        sq_sz = 10.
        if n_elements(ratio) eq 0 then ratio = .8   ;aspect ratio of image
        if n_elements(rec_w) eq 0 then begin
            rec_w  = sq_sz/pic_w
        endif else begin
            rec_w = rec_w/pic_w
        endelse
        if n_elements(rec_h) eq 0 then begin
            rec_h  =  ratio*sq_sz/pic_h
        endif else begin
            rec_h  =  rec_h/pic_h
        endelse
        x0     = 1./pic_w 
        y0     = 1./pic_h
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;first image is that of the threshold fields
        pic_name = '~/documents/ps/maps.ps'
        ps_start, pic_name, pic_w, pic_h
        loadct, 40, /s
        col=0
    endif

    ;make sure position is loaded
    if n_elements(pos) eq 0 then begin
        message, 'you must specify a position'
    endif

    ;directory where shapefiles are 
    can_shape_dir = '/data/ords/arma/armadja/shapefiles/'

    ;load preset projections 
    loaded = 0  ;flag to make sure one projection was loaded

    if loc eq 'franktown' then begin
        ;domain that covers all of hrenkf domain
        limit=[44,-77.6,46.2,-74.6]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            draw_shapefile, can_shape_dir+'CAN_adm3.shp', th=.1, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=1, latdel=1, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'south_quebec' then begin
        ;domain that covers all of hrenkf domain
        limit=[41,-84,50,-64]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /usa, /hires, mlinethick=.5, col=col
            draw_shapefile, can_shape_dir+'CAN_adm2.shp', th=.1, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.5, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=10, latdel=5, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'south_quebec_small' then begin
        ;domain that covers all of hrenkf domain
        limit=[43,-78,48,-70]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /usa, /hires, mlinethick=.5, col=col
            draw_shapefile, can_shape_dir+'CAN_adm2.shp', th=.1, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.5, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=3, latdel=3, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'prairies' then begin
        ;domain that covers all of hrenkf domain
        limit=[45,-115,57,-85]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /usa, /hires, mlinethick=.5, col=col
            ;draw_shapefile, can_shape_dir+'CAN_adm2.shp', th=.1, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.5, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=10, latdel=5, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'bc_alberta' then begin
        ;domain that covers all of hrenkf domain
        limit=[45,-132,57,-110]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /usa, /hires, mlinethick=.5, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.5, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=10, latdel=5, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if keyword_set(hrenkf_west) or loc eq 'hrenkf_west' then begin
        ;domain that covers all of hrenkf domain
        limit=[41.7834,-137.207,55.2531,-118.122]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /coasts, /hires, /rivers, mlinethick=.5, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=10, latdel=10, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'renkf_clip' then begin
        ;domain that covers all of hrenkf domain
        limit=[38.7834,-138.207,58.2531,-115.122]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /coasts, /hires, /rivers, mlinethick=.5, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=5, latdel=5, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if keyword_set(hrenkf_rot) or loc eq 'hrenkf_rot' then begin
        ;domain that covers all of hrenkf domain
        ;same as hrenkf_west but rotated
        limit=[44.7834,-137.207,55.2531,-118.122]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 29.5
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /coasts, /hires, /rivers, mlinethick=.5, col=col
        endif
        if keyword_set(grid) then begin
            ;grid
            map_grid, londel=5, latdel=5, glinestyle=2., label=1, glinethick=.5, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if keyword_set(renkf) or loc eq 'renkf' then begin
        ;regional renkf domain
        limit = [-10,-180,90,180]
        p0_lat = 55.
        p0_lon = -120.
        p0_rot = 23.
        map_set, p0_lat, p0_lon, p0_rot, /orthographic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5,/countries, /continents, /usa, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            map_grid, londel=20, latdel=20, glinestyle=1., label=1, glinethick=.5, charsize=.9, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'world' then begin
        map_set, 0, 180, 0, /mercator, $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, mlinethick=.5,/countries, /continents, col=col
            map_grid, glinestyle=1., label=1, glinethick=.5, charsize=.9, /box_axes, col=col
        endif
        loaded = 1
    endif
    if keyword_set(can_us_radars) or loc eq 'can_us_radars' then begin
        ;for us/can radar composite
        limit = [25., -121, 60, -71]
        p0_lat = 43.7 
        p0_lon = -88.
        p0_rot = -10.
        std_prll = [30, 55.]
            ;test artefact south hemisphere
            ;limit = [-70., 60, -5, 80]
            ;p0_lat = -40 
            ;p0_lon = 90.
            ;p0_rot = 0.
            ;std_prll = [-30, -55.]
        map_set, p0_lat, p0_lon, p0_rot, /albers, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_grid, londel=20, latdel=20, glinestyle=1., label=1, glinethick=.5, charsize=.9, /box_axes, col=col
        endif
        if keyword_set(map) then begin
            ;plot map details
                map_continents, /hires, mlinethick=.1, /continents, col=col
            map_continents, /hires, mlinethick=.1, /usa, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
        endif
        loaded = 1
    endif
    if loc eq 'north_am' then begin
        ;for us/can radar composite
        limit = [15., -141, 80, -61]
        p0_lat = 43.7 
        p0_lon = -88.
        p0_rot = -10.
        std_prll = [30, 55.]
        map_set, p0_lat, p0_lon, p0_rot, /albers, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_grid, londel=20, latdel=20, glinestyle=1., label=1, glinethick=.5, charsize=.9, /box_axes, col=col
        endif
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.1,/countries, /continents, /usa, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
        endif
        loaded = 1
    endif
    if loc eq 'north_west_bc' then begin
        ;for us/can radar composite
        limit = [50., -150, 70, -111]
        p0_lat = 43.7 
        p0_lon = -88.
        p0_rot = -10.
        std_prll = [30, 55.]
        map_set, p0_lat, p0_lon, p0_rot, /albers, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_grid, londel=20, latdel=20, glinestyle=1., label=1, glinethick=.5, charsize=.9, /box_axes, col=col
        endif
        if keyword_set(map) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.1,/countries, /continents, /usa, col=col
            draw_shapefile, can_shape_dir+'CAN_adm2.shp', th=.1, col=col
        endif
        loaded = 1
    endif
    if keyword_set(vancouver) or loc eq 'vancouver' then begin
        ;for radars in the vancouver area
        limit = [46.7300,     -126.025,      51.3259,     -119.15]
        p0_lat = (limit[0]+limit[2])/2.
        p0_lon = (limit[1]+limit[3])/2.
        p0_rot = 0.
        map_set, p0_lat, p0_lon, p0_rot, /conic, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /coasts, /hires, /rivers, mlinethick=2., col=col
            ;grid
            map_grid, londel=5, latdel=3, glinestyle=2., label=1, glinethick=2, charsize=1., /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'nat_enkf' then begin
        ;regional renkf domain
        limit = [28,-134.5,65.5,-63]
        p0_lat = 51.6
        p0_lon = -97.
        p0_rot = -14.4
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            col=120
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            col=0
            map_grid, londel=20, latdel=10, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'lhn_rdps' then begin
        ;regional renkf domain
        limit = [22,-144.5,70.5,-72]
        p0_lat = 51.6
        p0_lon = -97.
        p0_rot = -14.4
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            ;loadct, 0
            ;col=120
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            col=0
            map_grid, londel=20, latdel=10, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'pan_enkf' then begin
        ;pan_am domain
        limit = [38.5,-87,49.5,-73]
        p0_lat = 45
        p0_lon = -81
        p0_rot = 0.
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            map_grid, londel=5, latdel=5, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'mid_us' then begin
        ;pan_am domain
        limit = [31.5,-100,50.5,-73]
        p0_lat = 35
        p0_lon = -91
        p0_rot = 0.
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            loadct, 0, /s
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=120
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=120
            map_grid, londel=10, latdel=10, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=0
        endif
        loaded = 1
    endif
    if loc eq 'mid_us_closeup' then begin
        ;pan_am domain
        limit = [35,-100,50.5,-78]
        p0_lat = 45
        p0_lon = -91
        p0_rot = 0.
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            loadct, 0, /s
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=120
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=120
            map_grid, londel=10, latdel=10, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=0
        endif
        loaded = 1
    endif
    if loc eq 'tiny_mid_us' then begin
        ;pan_am domain
        limit = [40.,-85,46,-76]
        p0_lat = 43.85
        p0_lon = -80.75
        p0_rot = 0.
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            map_grid, londel=5, latdel=5, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'missouri' then begin
        ;pan_am domain
        limit = [36.,-96,42,-87]
        p0_lat = 38.159
        p0_lon = -92.28
        p0_rot = 0.
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            map_grid, londel=5, latdel=5, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'newb' then begin
        ;pan_am domain
        limit = [44.,-69,49.9,-52]
        p0_lat = 47.91
        p0_lon = -59.34
        p0_rot = 0.
        std_prll = [45, 50.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            ;map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.5, col=col
            map_grid, londel=5, latdel=5, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'gaspesia' then begin
        ;pan_am domain
        limit = [48.,-70,50.,-64]
        p0_lat = 48.1
        p0_lon = -67.
        p0_rot = 0.
        std_prll = [45, 50.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            ;map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.5, col=col
            map_grid, londel=4, latdel=2, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'erie' then begin
        ;pan_am domain
        limit = [40.,-85,45.,-77]
        p0_lat = 43
        p0_lon = -81.
        p0_rot = 0.
        std_prll = [45, 50.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.5, col=col
            map_grid, londel=4, latdel=2, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'meopar_east' then begin
        limit = [34.0,-75,55.0,-50]
        p0_lat =  40.55
        p0_lon = -64.76
        p0_rot = 0.
        std_prll = [20, 40.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            map_grid, londel=5, latdel=5, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'meopar_east_rot' then begin
        limit = [33.2,-62.1,57,-62]
        p0_lat =  45.
        p0_lon = -64
        p0_rot = -38.7
        std_prll = [40, 50.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            map_grid, londel=5, latdel=5, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif
    if loc eq 'test' then begin
        ;pan_am domain
        limit = [3.,-70,5,-68]
        p0_lat = 2.7
        p0_lon = -68.19
        p0_rot = 0.
        std_prll = [10, -10.]
        map_set, p0_lat, p0_lon, p0_rot,  /lambert, standard_parallels=std_prll, limit=limit,  $
                 pos=pos, /noborder, /noerase
        if keyword_set(grid) then begin
            ;plot map details
            map_continents, /hires, mlinethick=.5, /usa,/continents, col=col, /countries
            draw_shapefile, can_shape_dir+'CAN_adm1.shp', th=.1, col=col
            map_grid, londel=2, latdel=2, glinestyle=1., label=1, glinethick=.5, charsize=charsize, /box_axes, col=col
        endif
        loaded = 1
    endif

    if loaded ne 1 then begin
        message, 'you must specify at least one projection'
    endif

    if arg_present(proj_ind) then begin
        if (n_elements(lats) eq 0) or (n_elements(lons) eq 0) then begin
            message, 'you must specify lats and lons keywords for projection indices'
        endif
        if n_elements(missing) eq 0 then begin
          missing=-9999.  
        endif
        ;dimentions
        dims = size(lats, /dim)
        nx = dims[0]
        ny = dims[1]
        ;projection of i and j indexes
        print, 'projecting indices....'
        ii = rebin(          lindgen(nx) , nx, ny)
        proj_ii=round(map_patch(ii,lons,lats,missing=missing,/triangulate))
        print, 'done 1 of 2'
        jj = rebin(transpose(lindgen(ny)), nx, ny)
        proj_jj=round(map_patch(jj,lons,lats,missing=missing,/triangulate))
        print, 'done 2 of 2'
        ;adjust size when map_patch returns arrays with different dimensions (hrrr...) 
        sz_patched = size(proj_ii,/dim)
        if (sz_patched[0] ne sz_zbuf[0]) or (sz_patched[1] ne sz_zbuf[1]) then begin
            print, 'size from map patch not the same as the size of zbuf'
            print, 'sz_zbuf',    sz_zbuf
            print, 'sz_patched', sz_patched
            xdiff = sz_zbuf[0] - sz_patched[0]
            ydiff = sz_zbuf[1] - sz_patched[1]
            if xdiff ge 0 and ydiff ge 0 then begin
                tmpii = replicate(long(missing),sz_zbuf)
                tmpjj = replicate(long(missing),sz_zbuf)
                tmpii[0:sz_patched[0]-1,0:sz_patched[1]-1] = proj_ii
                tmpjj[0:sz_patched[0]-1,0:sz_patched[1]-1] = proj_jj
                proj_ii = tmpii
                proj_jj = tmpjj
            endif else begin
                message, 'case where map_patch returns larger grid is not yet implemented'
            endelse
        endif
        ;indices in 2d form
        proj_ind = proj_ii + proj_jj*nx
        ;mask of points where projection not available
        aa = where((proj_ii eq missing) or (proj_jj eq missing), naa)
        if naa gt 0 then proj_ind[aa]=missing
        ;indices in 2d form
        proj_ind = proj_ii + proj_jj*nx
        ;mask of points where projection not available
        aa = where((proj_ii eq missing) or (proj_jj eq missing), naa)
        if naa gt 0 then proj_ind[aa]=missing
    endif

    if keyword_set(test) then begin
        ;plot lines for testing image distance proportions
        lat_lon_range, p0_lat, p0_lon, 200d, 0., lat_o, lon_o
        plots, [p0_lon,lon_o], [p0_lat,lat_o], col=120
        lat_lon_range, p0_lat, p0_lon, 200d, 90., lat_o, lon_o
        plots, [p0_lon,lon_o], [p0_lat,lat_o], col=120

        ps_close, pic_name,  /del_ps, font='lmroman', /v, /pdf
    endif

END
