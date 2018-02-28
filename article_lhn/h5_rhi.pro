pro h5_rhi, r_name=r_name, want_az=want_az, jul_time=jul_time
;read h5 volume scans and display a rhi

base_dir = '/fs/cetus/fs3/mrb/armp/armpdbm/output/'

;default value for missing
if n_elements(missing) eq 0 then missing = -9999.

;time 
t0 = julday(07, 08, 2014, 04, 00, 00)
tf = julday(07, 08, 2014, 12, 00, 00)

;desired azimuth
want_az = 165.

;derired radar
;r_name = 'ussgf'
r_name = 'caxme'

if strmid(r_name, 0,2) eq 'us' then res = .25
if strmid(r_name, 0,2) eq 'ca' then res = 1.

;set up image coords setup 
ps = {pic_w:0.,pic_h:0.,sq_w:0.,sq_h:0.,pal_sp:0.,pal_w:0.,rec_w:0.,rec_h:0.,sp_w:0.,sp_h:0.,$
         x1:0.,   x2:0.,  x3:0.,  x4:0.,    x5:0.,   x6:0.,   x7:0.,   x8:0.,  x9:0.,        $
         y1:0.,   y2:0.,  y3:0.,  y4:0.,    y5:0.,   y6:0.,   y7:0.,   y8:0.,  y9:0.}
;ps.sq_w = 20.  ;true aspect ratio for 15km high rhi
;ps.sq_h = 1.2
ps.sq_w = 20.  ;true aspect ratio for 15km high rhi
ps.sq_h = 3.
ps.pic_h = 9.
ps.pic_w  = ps.sq_w+3.
ps.pal_sp = .5/ps.pic_w
ps.pal_w  = .25/ps.pic_w
ps.rec_w  = ps.sq_w/ps.pic_w
ps.rec_h  = ps.sq_h/ps.pic_h
ps.sp_w   = 2./ps.pic_w
ps.sp_h   = .5/ps.pic_h
ps.x1     = 1.2/ps.pic_w 
ps.x2     = ps.x1 + ps.rec_w + 2.*ps.sp_w
ps.y1     = 1.3/ps.pic_h
ps.y2     = ps.y1 + ps.rec_h + ps.sp_h

;make mapping structures
range = [0., 60]
legs, range=range, n_col=6, over_high='extend', under_low='white', $
      excep_val=[missing], excep_col=['grey_200'], excep_tol=[1e-3], $
      mapping=mapping
          

;make list of directories to look in
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


;this_file = '/fs/cetus/fs3/mrb/armp/armpdbm/output/2014/07/08/05/40/ussgf_vcp12_20140708T0542Z_0x2.h5'
for ii=0, nf-1 do begin
    this_file = file_arr[ii]
    print, ''
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
            what_id = h5g_open(file_id,'/dataset'+num_str+'/data1/what')
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
            data_id = h5d_open(file_id,'/dataset'+num_str+'/data1/data')
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
                dum = ref[*,az_ind]
                median_filter, dum, 4
                ref_filt_arr[0:nr-1,kk-1] = dum
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
    
    
    ;select data on image grid
    pnx = 900
    xmin = 0.
    xmax = 250.
        ;xmin = 142.
        ;xmax = 144.
    pnz = 500
    zmin = 0.
    zmax = 15.
        ;zmin = 1.5
        ;zmax = 4.
    rhi_img      = fltarr(pnx, pnz)
    rhi_filt_img = fltarr(pnx, pnz)
    
    
    for i=0d, pnx-1 do begin
        xpos = i/(pnx-1)*(xmax-xmin)+xmin

        for k=0d, pnz-1 do begin
            zpos = k/(pnz-1)*(zmax-zmin)+zmin
            
            model43_elev, xpos, zpos, this_elev

            dum = min( abs(el_arr - this_elev), el_ind)
            if (this_elev lt min_elev) or (this_elev gt max_elev) then begin
                rhi_img[i,k]      = missing
                rhi_filt_img[i,k] = missing
            endif else begin
                model43_earth, xpos, this_elev, dum, dist_beam=this_dist_beam
                dum = min( abs(dist_beam - this_dist_beam), ran_ind)
                rhi_img[i,k] = ref_arr[ran_ind,el_ind]
                rhi_filt_img[i,k] = ref_filt_arr[ran_ind,el_ind]
            endelse
        endfor
        ;break
    endfor
    
    ;first image is that of the threshold fields
    pic_name = '~/documents/ps/h5_rhi/h5_rhi_'+date_str+'.ps'
    ps_start, pic_name, ps.pic_w, ps.pic_h, /white
    

    ;raw data
    x0 = ps.x1
    y0 = ps.y1
    pos = [x0, y0, x0+ps.rec_w, y0+ps.rec_h]
    
    ;image
    legs, data=rhi_img, mapping=mapping, tv_out=pos
    
    ;range height plot
    loadct, 40, /s
    plot, [0], /nodata, /noerase, /normal, pos=pos, $
          xr=[xmin,xmax], xs=9, xtitle='distance [km]',xminor=1, $
          yr=[zmin,zmax], ys=9, ytitle='height [km]', yminor =1,yticklen=.004
    plots, [pos[0],pos[2],pos[2]], [pos[3], pos[3],pos[1]], /normal, thick=.8



    ;filtered rhi
    x0 = ps.x1
    y0 = ps.y2
    pos = [x0, y0, x0+ps.rec_w, y0+ps.rec_h]

    ;image
    legs, data=rhi_filt_img, mapping=mapping, tv_out=pos
    
    ;range height plot
    loadct, 40, /s
    plot, [0], /nodata, /noerase, /normal, pos=pos, $
          xr=[xmin,xmax], xs=9, xtitle=' ', xtickname=replicate(' ',20),xminor=1, $
          yr=[zmin,zmax], ys=9, ytitle='height [km]', yminor =1, yticklen=.004
    plots, [pos[0],pos[2],pos[2]], [pos[3], pos[3],pos[1]], /normal, thick=.8

    ;date
    xyouts, x0+.3*ps.rec_w, y0+1.1*ps.rec_h, date_str, charsize=1.1, /normal
    
    ;color palette
    pos = [x0+ps.rec_w+ps.pal_sp,ps.y1,x0+ps.rec_w+ps.pal_sp+ps.pal_w,y0+ps.rec_h]
    legs, mapping=mapping, palette=pos, units='dBZ'
    
    ;fps_close, pic_name,  /del_ps, font='lmroman', /v, /pdf
    ps_close, pic_name,  /del_ps, font='lmroman', /v, /gif, density='400', geometry='55%'
endfor

end

