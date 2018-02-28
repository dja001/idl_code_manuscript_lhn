pro get_and_plot_h5_rhi, rinfo=rinfo, want_az=want_az, $
    mapping=mapping, jul_time=jul_time, pos=pos, missing=missing, topo=topo, $
    xrange=xrange, zrange=zrange, xflip=xflip, $
    dataset=dataset
;read h5 volume scans and display a rhi at pos=pos

base_dir = '/fs/cetus/fs3/mrb/armp/armpdbm/output/'

;default value for missing
if n_elements(missing) eq 0 then missing = -9999.
ground=-4000.

;desired time 
if n_elements(jul_time) eq 0 then message, 'please specify jul_time'

;desired azimuth
if n_elements(want_az) eq 0 then  message, 'please specify the desired azimuth for rhi'

;desired radar
if n_elements(rinfo) eq 0 then   message, 'please specify the desired radar info for rhi'
r_name = strlowcase(rinfo.id)
hrad = (rinfo.alt + rinfo.tower_h)/1000.    ;radar height in km

;desired position of image
if n_elements(pos) ne 4 then      message, 'please specify the desired position for rhi image'

;desired position of image
if n_elements(mapping) eq 0 then  message, 'please specify a color mapping'

if keyword_set(topo) then begin
    topo_file='/fs/cetus/fs3/mrb/arma/armadja/shared/constants/hrdps_topo.fst'
    ;topo_file='/fs/cetus/fs2/mrb/arma/armagr7/geophy_files/geophy_meopar_east_1.0km_N53/geophy_east_1p0km_1480x1926_SRTM_JPL_mar_2017.fst'
    get_gem_data, topo_file, lat=hrdps_lat, lon=hrdps_lon, var_name='ME', values=me
    ;aa = where(hrdps_lat gt 48 and hrdps_lat lt 51 and hrdps_lon gt -70 and hrdps_lon lt -50., naa)
    ;aa = where(hrdps_lat gt 50 and hrdps_lat lt 57 and hrdps_lon gt -130 and hrdps_lon lt -110., naa)
    aa = where(hrdps_lat gt 40 and hrdps_lat lt 45 and hrdps_lon gt -85 and hrdps_lon lt -75., naa)
    if naa ne 0 then begin
        hrdps_lat = hrdps_lat[aa]
        hrdps_lon = hrdps_lon[aa]
        hrdps_me  =        me[aa]
    endif
endif

;default dataset
if n_elements(dataset) eq 0 then begin
    dataset= 'data1'
endif 

;predefined resolution of range gates
if strmid(r_name, 0,2) eq 'us' then res = .25
if strmid(r_name, 0,2) eq 'ca' then res = 1.

;parameters for rhi image
pnx = 1000
if n_elements(xrange) ne 0 then begin
    xmin = xrange[0]
    xmax = xrange[1]
endif else begin
    xmin = 0.
    xmax = 250.
endelse
pnz = 500
if n_elements(zrange) ne 0 then begin
    zmin = zrange[0]
    zmax = zrange[1]
endif else begin
    zmin = 0.
    zmax = 15.
endelse

;time interval in which we search for files
dt = 30d /1440.     ;30 min
t0 = jul_time - dt
tf = jul_time + dt

;make list of files in interval
caldat, t0, month, day, year, hour, minute, second
minute = floor(minute/10.)*10.  ;nearest decimal minute: 00, 10, 20, etc...
time_start = julday(month, day, year, hour, minute)
this_time = time_start
count = 0l
while this_time lt tf do begin
    caldat, this_time, month, day, year, hour, minute, second
    ymdhm = string(year, month, day, hour, minute, format='(i4,"/",i02,"/",i02,"/",i02,"/",i02,"/")')
    ff = file_search(base_dir+ymdhm+r_name+'*', count=nf)
    if nf ne 0 then begin
        if count eq 0 then begin
            file_arr = ff
        endif else begin
            file_arr = [file_arr, ff]
        endelse
    endif
    count ++
    this_time = time_start + (10.*count)/1440.
endwhile
nf = n_elements(file_arr)

;determin3 exact measurement time for each file
time_arr = dblarr(nf)
for ii=0, nf-1 do begin
    this_file = file_arr[ii]
    ind = strpos(this_file, '2014',/reverse_search)
    date_str = strmid(this_file,ind,13)
    year   = strmid(date_str,0,4)
    month  = strmid(date_str,4,2)
    day    = strmid(date_str,6,2)
    hour   = strmid(date_str,9,2)
    minute = strmid(date_str,11,2)
    time_arr[ii] = julday(month, day, year, hour, minute, 00)
endfor

;file nearest file and determine if it is close enough to desired time
dum = min(abs(jul_time - time_arr),ind)
t_diff = abs(jul_time - time_arr[ind])*1440.    ;time difference in minutes
nodata = 0
if t_diff gt 10. then nodata=1

if nodata eq 0 then begin
    this_file = file_arr[ind]
    print, this_file
    ind = strpos(this_file, '2014',/reverse_search)
    date_str = strmid(this_file,ind,13)
    
    if ~file_test(this_file) then begin
        message, 'this_file: "'+this_file+'" does not exist'
    endif
    ;Result = H5_BROWSER(this_file)
    
    ;reading the h5 file
    file_id = H5F_OPEN(this_file)

    ;determine how many elevations in file
    n_obj = h5g_get_num_objs(file_id)
    nel = n_obj - 3
    max_r = 1000
    ref_arr      = fltarr(max_r, nel)
    ref_filt_arr = fltarr(max_r, nel)
    el_arr = fltarr(nel) + missing
    
        for kk=1,nel do begin
            num_str = strtrim(string(kk,format='(i2)'),1)
    
            ;get reflectivity attributes
            what_id = h5g_open(file_id,'/dataset'+num_str+'/'+dataset+'/what')
                gain_id      = h5a_open_name(what_id,'gain')
                ref_gain     = h5a_read(gain_id)
                h5a_close,     gain_id
                offset_id    = h5a_open_name(what_id,'offset')
                ref_offset   = h5a_read(offset_id)
                h5a_close,     offset_id
                nodata_id    = h5a_open_name(what_id,'nodata')
                ref_nodata   = h5a_read(nodata_id)
                h5a_close,     nodata_id
                undetect_id  = h5a_open_name(what_id,'undetect')
                ref_undetect = h5a_read(undetect_id)
                h5a_close,     undetect_id
            h5g_close, what_id
    
            ;get elelv and azimuth
            how_id = h5g_open(file_id,'/dataset'+num_str+'/how')
                elev_id      = h5a_open_name(how_id,'elangles')
                elev         = h5a_read(elev_id)
                h5a_close,     elev_id
                az_id        = h5a_open_name(how_id,'startazA')
                az_start     = h5a_read(az_id)
                h5a_close,     az_id
                az_id        = h5a_open_name(how_id,'stopazA')
                az_stop      = h5a_read(az_id)
                h5a_close,     az_id
            h5g_close, how_id
    
            ;get reflectivity data
            data_id = h5d_open(file_id,'/dataset'+num_str+'/'+dataset+'/data')
            ref_byte = h5d_read(data_id)
            h5d_close, data_id
    
            ;transform raw data into reflectivity
            ;locations of nodata
            nodata_ind = where(ref_byte eq ref_nodata, num_nodata)
            ;get reflectivity
            ref = ref_byte*ref_gain + ref_offset
            ;block out nodata
            if num_nodata ne 0 then ref[nodata_ind] = missing
    
            ;pick azimuth closest to desired degree
            dum = min(abs(((az_start+az_stop)/2.) - want_az), az_ind)
    
            ;get data we are interested in
            nr = n_elements(ref[*,0])
                    ;ref_arr[0:nr-1,kk-1] = smooth(ref[*,az_ind], 3)
                ;dum = ref[*,az_ind]
                ;median_filter, dum, 4
                ;ref_filt_arr[0:nr-1,kk-1] = dum
            ref_arr[0:nr-1,kk-1] = ref[*,az_ind]
            el_arr[kk-1] = elev[az_ind]
        endfor
    
    ;done reading h5 file
    h5f_close, file_id

    ;get horiz distance and altitude of individual radar bins
    dist_beam = 2.+(findgen(max_r))*res;, nr
    
    dat_hdist  = fltarr(max_r, nel) 
    dat_height = fltarr(max_r, nel) 
    
    for kk=0, nel-1 do begin
        model43, dist_beam, el_arr[kk], height, DIST_EARTH=dist_earth
        dat_hdist[*,kk]  = dist_earth
        dat_height[*,kk] = height
    endfor
    ;min and max altitudes
    min_elev = min(el_arr) - .1
    max_elev = max(el_arr) + .5
    
    ;fill in data in image space
    rhi_img      = fltarr(pnx, pnz)
    rhi_filt_img = fltarr(pnx, pnz)
    for i=0d, pnx-1 do begin
        xpos = i/(pnx-1)*(xmax-xmin)+xmin

        ;get height at this location
        lat_lon_range, rinfo.lat, rinfo.lon,xpos,want_az,lat_o,lon_o
        dum = min(abs(hrdps_lat-lat_o)+abs(hrdps_lon-lon_o),ind)
        terrain_h = hrdps_me[ind]/1000. ;in km

        for k=0d, pnz-1 do begin
            zpos = k/(pnz-1)*(zmax-zmin)+zmin

            if zpos le terrain_h then begin
                rhi_img[i,k]      = ground
                rhi_filt_img[i,k] = ground
            endif else begin
            
                model43_elev, xpos, zpos, this_elev,radar_height=hrad

                dum = min( abs(el_arr - this_elev), el_ind)
                if (this_elev lt min_elev) then begin
                    rhi_img[i,k]      = missing
                    rhi_filt_img[i,k] = missing
                endif else begin
                    if (this_elev gt max_elev) then begin
                        rhi_img[i,k]      = missing
                        rhi_filt_img[i,k] = missing
                    endif else begin
                        model43_earth, xpos, this_elev, dum, dist_beam=this_dist_beam
                        dum = min( abs(dist_beam - this_dist_beam), ran_ind)
                        rhi_img[i,k] = ref_arr[ran_ind,el_ind]
                        rhi_filt_img[i,k] = ref_filt_arr[ran_ind,el_ind]
                    endelse
                endelse
            endelse
        endfor
        ;break
    endfor
    
    ;flip horizontally if desired
    if keyword_set(xflip) then begin
        rhi_img = rotate(rhi_img,5)
        xr = [xmax,xmin]
    endif else begin
        xr = [xmin,xmax]
    endelse
    
    ;map colors 
    legs, data=rhi_img, mapping=mapping, tv_out=pos, img_out=ppp
    
    ;range height plot
    loadct, 40, /s
    plot, [0], /nodata, /noerase, /normal, pos=pos, col=255, $
          xr=xr,          xs=9, xtitle='distance [km]',xminor=1, $
          yr=[zmin,zmax], ys=9, ytitle='height [km]', yminor =1,yticklen=.004
    plots, [pos[0],pos[2],pos[2]], [pos[3], pos[3],pos[1]], /normal, thick=.8

        ;;image
        ;legs, data=rhi_filt_img, mapping=mapping, tv_out=pos
        ;
        ;;range height plot
        ;loadct, 40, /s
        ;plot, [0], /nodata, /noerase, /normal, pos=pos, $
        ;      xr=[xmin,xmax], xs=9, xtitle=' ', xtickname=replicate(' ',20),xminor=1, $
        ;      yr=[zmin,zmax], ys=9, ytitle='height [km]', yminor =1, yticklen=.004
        ;plots, [pos[0],pos[2],pos[2]], [pos[3], pos[3],pos[1]], /normal, thick=.8

    ;date
    xyouts, pos[0]+.45*(pos[2]-pos[0]), pos[1]+1.05*(pos[3]-pos[1]), date_str, charsize=1.3, /normal
    
endif else begin
    loadct, 40, /s
    plot, [0], /nodata, /noerase, /normal, pos=pos,col=255, $
          xr=[xmin,xmax], xs=9, xtitle='distance [km]',xminor=1, $
          yr=[zmin,zmax], ys=9, ytitle='height [km]', yminor =1,yticklen=.004
    plots, [pos[0],pos[2],pos[2]], [pos[3], pos[3],pos[1]], /normal, thick=.8
    ;date
    xyouts, pos[0]+.3*(pos[2]-pos[0]), pos[1]+1.1*(pos[3]-pos[1]), 'no data', charsize=1.1, /normal
endelse

end

