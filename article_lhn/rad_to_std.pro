pro test_interpol
    ;this routine shows a side-by-side comparison of the original and the interpolated data.

    ;location
    loc = 'lhn_rdps'
    ;loc = 'mid_us'
    ;loc = 'missouri'

    ;get latitude and longitude on projected grid
    ;determine latitudes and longitudes on device used for projection
    set_plot, 'Z'
    ; projection image size in pixels
    sq_w = 10.
    sq_h = .8*sq_w
    dpi = 400
    sz_zbuf = long(floor([sq_w/2.54*dpi,sq_h/2.54*dpi]))     ;data square smaller than this because of boundaries
    print, sz_zbuf
    device, set_resolution=sz_zbuf, set_pixel_depth=24
    device, decomposed=0
    ;map projection
    sub_domain = [0,0.,1.,1.]
        maps, pos=sub_domain, loc=loc
        pnx = sz_zbuf[0]
        pny = sz_zbuf[1]
        xp = rebin(lindgen(pnx),pnx,pny)
        yp = rebin(rotate(lindgen(pny),1),pnx,pny)
        recomp = yp*pnx + xp
        ;convert 2d arrays into 1d
        aa = where(finite(xp))
        xxp = xp[aa]
        yyp = yp[aa]
        d = convert_coord(xxp,yyp, /device, /to_data,/double)
        dummy = reform(d[0,*])
        dev_lon = dummy[recomp]
        dummy = reform(d[1,*])
        dev_lat = dummy[recomp]
    device, z_buffering = 0


    ;interpolated data
    interp_file = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_12_avg_09_min/2014/07/30/2014073000_00.fst'
    get_gem_data, interp_file, var_name='RDPR', values=mod_pr, lat=mod_lat, lon=mod_lon
    get_gem_data, interp_file, var_name='RDQI', values=mod_qi
    sz = size(mod_lat,/dim)
    mod_nx = sz[0]
    mod_ny = sz[1]

    
    ;raw radar data
    ;rad_file = '/local/drive2/arma/armadja/data/radar_nat_composites/raw_data/operation.radar.Composite-USACDN-4km.precipet.std-rpn/2014070812_00ref_4km.stnd'
    ;get_gem_data, rad_file, getvar='RDBR', lat=rad_lat, lon=rad_lon
    ;get_gem_data, rad_file, var_name='RDBR', values=rad_ref
    rad_file = '/fs/cetus/fs3/mrb/armp/armpdbm/output/composites/2014/07/30/qcomp_v7_20140730T0010Z.h5'
    read_h5, file=rad_file, ref=rad_ref, /qced, tot_qi=rad_qi, missing=missing, lats=rad_lat, lons=rad_lon
    sz = size(rad_lat,/dim)
    rad_nx = sz[0]
    rad_ny = sz[1]

    ;range for color pics
    range = [0., 60]
    ;make reflectivity mapping structure
    legs, range=range, n_col=6, over_high='extend', under_low='white', excep_val=missing, excep_col='white', $
          mapping=mapping_ref
    ;make qi mapping structure
    legs, range=[0,1], color_arr='b_w', excep_val=missing, excep_col='blue', $
          mapping=mapping_qi

    base = 20.
    range = [base/64.,base/32.,base/16.,base/8.,base/4.,base/2.,base]
    legs, range=range, $
          color_arr=['brown','blue','green','orange','red','pink'], $
          excep_val=[missing,0.], excep_col=['grey_230','white'], excep_tol=[1e-3,1e-2], $
          over_high='extend', under_low='white',   $
          mapping=mapping_rr

    range = [0., 50]
    legs, range=range, color_arr='b_w', over_under='extend', $
          excep_val=[missing], excep_col=[169,222,255], $
          mapping=mapping_bw

    ;set up image coords setup 
    pic_h = 25.
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
    
    pic_dir = '~/documents/ps/'
    pic_name = pic_dir+'rad_to_std.ps'
    ps_start, pic_name, pic_w, pic_h

        ;plot interpolated r
        x0 = x1
        y0 = y1
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;get projection indice
        kdll, mod_lat, mod_lon, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
              proj_ind_mod
        ;project data
        aa = where(mod_qi eq 0., naa)
        if naa ne 0 then mod_pr[aa] = missing
        apply_proj, mod_pr, proj_ind_mod, proj_mod_ref, missing=missing
        ;plot image
        legs, data=proj_mod_ref, mapping=mapping_rr,  tv_out=pos
        ;title
        xyouts, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'interpolated on model grid',/normal,charsize=2., col=0
        ;overlay grid
        maps, pos=pos, /grid, /map, loc=loc
        plot_border, mat_lat=mod_lat, mat_lon=mod_lon
        plots, [-97.1], [49.9], psym=8, symsize=.2, col=210

        ;;plot quality index
        ;x0 = x1
        ;y0 = y2
        ;pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;;project data
        ;apply_proj, mod_qi, proj_ind_mod, proj_mod_qi, missing=missing
        ;;plot image
        ;legs, data=proj_mod_qi, mapping=mapping_qi,  tv_out=pos
        ;;title
        ;xyouts, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'interpolated qi',/normal,charsize=2., col=0
        ;;overlay grid
        ;maps, pos=pos, /grid, /map, loc=loc
        ;plot_border, mat_lat=mod_lat, mat_lon=mod_lon

        ;plot radar rr
        x0 = x2
        y0 = y1
        pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;get projection indice
        kdll, rad_lat, rad_lon, dev_lat, dev_lon, missing=missing, /nearest, $ ;input
              proj_ind_rad
        ;project data
        aa = where(rad_qi eq 0., naa)
        if naa ne 0 then rad_ref[aa] = missing
        apply_proj, rad_ref, proj_ind_rad, proj_rad_ref, missing=missing
        ;plot image
        legs, data=proj_rad_ref, mapping=mapping_ref,  tv_out=pos
        ;title
        xyouts, pos[0]+.01*(pos[2]-pos[0]),pos[1]+.105*(pos[3]-pos[1]),'orig radar data',/normal,charsize=2., col=0
        ;overlay grid
        maps, pos=pos, /grid, /map, loc=loc
        plot_border, mat_lat=rad_lat, mat_lon=rad_lon
        loadct, 40, /s
        plots, [-92.], [44.3], psym=8, symsize=.2, col=210


        ;plot r palette
        x0 = x0 + rec_w + sp_w
        pos = [x0,y0,x0+pal_w,y0+rec_h]
        ;plot image
        legs, mapping=mapping_ref, palette=pos, units='dbz'
        ;plot qi palette
        x0 = x0 + rec_w + sp_w
        pos = [x0,y1,x0+pal_w,y1+rec_h]
        ;plot image
        ;legs, mapping=mapping_qi, palette=pos, units='unitless'
        legs, mapping=mapping_rr, palette=pos, pal_prop='equal',units='mm/h', ytickformat='(f6.1)'

    ps_close, pic_name, /del_ps, font='lmroman', /v, /pdf

end











;MAIN program--------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
pro rad_to_std, urp=urp, h5=h5, now=now

;convert radar reflectivity from whatever source to standard files
if n_elements(urp) eq 0 and $
   n_elements(h5)  eq 0 and $
   n_elements(now) eq 0 then message, 'the "urp", "h5" or "now" keyword must be used'

if n_elements(urp)+n_elements(h5)+n_elements(now) gt 1 then message, 'no more than one keyword at a time please'

;;2016
;t0 = julday(07,31,2016,00,00,00)
;tf = julday(09,04,2016,00,00,00)

;2014
t0 = julday(07,01,2014,00,00,00)
tf = julday(07,31,2014,00,00,00)

;internal variables & parameters
missing=-999.
undetect = -32.

;start and end time where fst files are desired
if keyword_set(now) then r_data_dir = '/fs/cetus/fs3/mrb/arma/armadja/nowcast_workdir/'
if keyword_set(urp) then r_data_dir = '/local/raid/armadja/data/radar_nat_composites/reduced_rdata'
if keyword_set(h5)  then r_data_dir = '/fs/cetus/fs3/mrb/armp/armpdbm/output/composites/'

;time resolution in minutes
dt=10D  
;dt=5D   

;template file for fst structure
;fst_template = '/local/raid/armadja/data/radar_nat_composites/dearchived/2014070812_001' 
fst_template = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/fst_template_rdps.fst'

;path for output data
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_rdps/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/nowcasts/2014070815/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v4/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v4_kdll_40/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v4_kdll_40/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v4_kdll_12_avg_15_min/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v6_kdll_12_avg_15_mi/'
    ;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_12_avg_09_min/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_14_avg_09_min/'
rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_01_avg_01_min/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_08_avg_05_min/'
;rad_out_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_02_avg_01_min/'

;create dir if it does not exist
if ~file_test(rad_out_dir) then file_mkdir, rad_out_dir

;template for fst data
get_gem_data, fst_template, var_name='RDPR', lat=mod_lat, lon=mod_lon
sz = size(mod_lat,/dim)
mod_nx = long(sz[0])
mod_ny = long(sz[1])
;obtain fst structure from a template
unit=102
file_id=fstouv(unit=unit,file=fst_template)

    ;insure that cmc_timestamp is a long integer        this really should be done in the fst library...
    if n_elements(cmc_timestamp) ne 0 then begin
        if ~isa(cmc_timestamp, 'long') then begin
            cmc_timestamp = long(cmc_timestamp)
        endif
    endif
    ;obtain fst
    key=fstinf(u=unit,nomvar='RDPR',date=cmc_timestamp)
    err_catch, unit, key
    ;get idl structure with all fields describing the entry
    pr_struct = fstprm(r=key)
    ;insure npas is set to zero
    pr_struct.npas = 0
    pr_struct.etiket = 'idl_convert'

    ;get associated >> and ^^
    ref_ig1 = pr_struct.ig1
    ref_ig2 = pr_struct.ig2
    ;>>
    found = 0
    key=fstinf(u=unit,nomvar='>>',date=cmc_timestamp)
    while key gt 0 do begin
        tic_struct = fstprm(r=key)
        if (tic_struct.ip1 eq ref_ig1) and (tic_struct.ip2 eq ref_ig2) then begin
            found = 1
            break
        endif
        key=fstinf(u=unit,nomvar='>>',date=cmc_timestamp,/sui)
    endwhile
    if found ne 1 then message, 'did not find appropriate >> field'
    tic_data   = fstluk(r=key)
    ;>>
    found = 0
    key=fstinf(u=unit,nomvar='^^',date=cmc_timestamp)
    while key gt 0 do begin
        tac_struct = fstprm(r=key)
        if (tac_struct.ip1 eq ref_ig1) and (tac_struct.ip2 eq ref_ig2) then begin
            found = 1
            break
        endif
        key=fstinf(u=unit,nomvar='^^',date=cmc_timestamp,/sui)
    endwhile
    if found ne 1 then message, 'did not find appropriate ^^ field'
    tac_data   = fstluk(r=key)
stat = fstfrm(u=unit)

;make a list of files to interpolate and insure that they are there
count = 0l
this_time = t0
time_arr = this_time
caldat, this_time, month, day, year, hour, minute
if keyword_set(now) then this_r_file = r_data_dir+'radar_extrapole.std'
if keyword_set(urp) then this_r_file = r_data_dir+string(year,month,day,hour,minute,format='(i4,3i02,"_",i02,"ref_4km.stnd")')
if keyword_set(h5)  then this_r_file = r_data_dir+string(year,month,day,year,month,day,hour,minute,format='(i4,"/",i02,"/",i02,"/qcomp_v7_",i4,2i02,"T",2i02,"Z.h5")')
if ~file_test(this_r_file) then message, 'file:  '+this_r_file+'   is missing'
r_file_arr = this_r_file
count ++
this_time = t0 + count*dt/1440.
while this_time le tf do begin
    time_arr = [time_arr,this_time]
    caldat, this_time, month, day, year, hour, minute
    if keyword_set(now) then this_r_file = r_data_dir+'radar_extrapole.std'
    if keyword_set(urp) then this_r_file = r_data_dir+string(year,month,day,hour,minute,format='(i4,3i02,"_",i02,"ref_4km.stnd")')
    if keyword_set(h5)  then this_r_file = r_data_dir+string(year,month,day,year,month,day,hour,minute,format='(i4,"/",i02,"/",i02,"/qcomp_v7_",i4,2i02,"T",2i02,"Z.h5")')
    print, this_r_file
    if ~file_test(this_r_file) then message, 'file:  '+this_r_file+'   is missing'
    r_file_arr = [r_file_arr,this_r_file]
    count ++
    this_time = t0 + count*dt/1440.
endwhile

;generate interplation table 
;get_gem_data, r_file_arr[0], getvar='rdbr', lat=rad_lat, lon=rad_lon
read_h5, lats=rad_lat, lons=rad_lon, missing=missing

;define projection of radar point to model grid
radius = 1.7777    ;return all points within a radius of x kilometers
nmax = 6; radius^2.    ;maximum
valid_nmin = 1  ;minimum number of valid points required
kdll, rad_lat, rad_lon, mod_lat, mod_lon, missing=missing, within_dist=radius, nmax=nmax , $ ;input
      proj_ind, nfound=nfound                                                                ;output

;data structure containing list of points to interpolate from
mod_ind = {data:lonarr(mod_nx*mod_ny,nmax),$
           npts:lonarr(mod_nx*mod_ny),     $
           mod_nx:0l, mod_ny:0l}
mod_ind.data   = proj_ind
mod_ind.npts   = nfound
mod_ind.mod_nx = mod_nx
mod_ind.mod_ny = mod_ny


;interpolate radar data and make standard files
n_files = n_elements(r_file_arr)

for nn=0, n_files-1 do begin
    ;determine which data source is to be interpolated (set source to 1)
    urp = 0
    h5  = 1
    now = 0

    ;load rmn lib
    LOAD_RMN_LIB

    ;cmc time for this date
    jul_to_cmc, time_arr[nn], cmc_time

    ;get source reflectivity
    if keyword_set(urp) or keyword_set(now) then begin
        get_gem_data, r_file_arr[nn], var_name='RDBR', values=rad_ref, cmc_timestamp=cmc_time
        rad_ref = rad_ref[*,0:1499]
        ;fake quality index
        rad_qi = replicate(0., size(rad_ref,/dim))
        aa = where(rad_ref gt 0., caa)
        if caa ne 0 then rad_qi[aa] = 1.
    endif
    if keyword_set(h5) then begin
        read_h5, file=r_file_arr[nn], ref=rad_ref, tot_qi=rad_qi, missing=missing, /qced
    endif

    ;apply the projection for radar data points
    mod_pr = replicate(float(missing),mod_ind.mod_nx,mod_ind.mod_ny)   ;initialize reflectivity field with missing value
    mod_qi = replicate(0.,mod_ind.mod_nx,mod_ind.mod_ny)               ;initialize quality field with zero
    for mp=0l, n_elements(mod_ind.npts)-1 do begin
        ;interpolate radar pts
        if mod_ind.npts[mp] ne 0 then begin
            ;neighbouring points were found
            qi_arr  =  rad_qi[mod_ind.data[mp,0:mod_ind.npts[mp]-1]]
            ref_arr = rad_ref[mod_ind.data[mp,0:mod_ind.npts[mp]-1]]
            aa = where(qi_arr gt 0.1, counta)
            if counta ge valid_nmin then begin    ;there is at least n valid measurements in neighbourhood
                valid_ref = ref_arr[aa]
                valid_qi  = qi_arr[aa]
                bb = where(valid_ref gt 15., countb)
                if countb ge valid_nmin then begin  ;there is at least n measurements where reflectivity is greater than 15 dBZ
                                            ;this check acts as a speckle filter
                    ;;;median
                    ;mod_pr[mp] = median_val(valid_ref,ind=mind)
                    ;mod_qi[mp]  = valid_qi[mind]

                    ;convert radar reflectivities to rain rates
                    ;          Marshall-Palmer: Z_lin = aR^b with a = 200 and b = 1.6
                    ;          with input in dB the equation for R is
                    ;          R = (10.^(Z_dB/10.)/200.)^(1./1.6)
                    ;          which is simplified below
                    radar_pr = 10d^(valid_ref/16d) / 27.424818   ;precip rate in mm/hr
                    ;mod_pr[mp] = total(valid_qi*radar_pr)/total(valid_qi)  ;quality weighted average
                        ;take only one point for nearest neighbor
                        mod_pr[mp] = total(valid_qi[0]*radar_pr[0])/total(valid_qi[0])  ;quality weighted average

                endif else begin
                    ;not enough echoes with precip > 15 dBZ
                    mod_pr[mp] = 0.
                endelse
                ;quality index is always a mix of everything valid
                mod_qi[mp]  = mean(valid_qi)
            endif
        endif
    endfor

    ;fake quality index if needed
    if keyword_set(urp) or keyword_set(now) then begin
        ;set quality index to 1 in middle of us
        ;mod_qi[250:500,50:250] = 1.     ;reduced domain
        mod_qi[550:855,220:450] = 1.    ;rdps
    endif

    ;data structure from template
    ;adjust data struct for reflectivity
    ref_struct = pr_struct
    ref_struct.nomvar = 'RDPR'
    ref_struct.date = cmc_time
    ;adjust data struct for reflectivity
    qi_struct = pr_struct
    qi_struct.nomvar = 'RDQI'
    qi_struct.date = cmc_time

    ;create and save standard file
    caldat, time_arr[nn], mo, dd, yy, hh, mi, ss
    tmp_file = rad_out_dir+string(yy,mo,dd,yy,mo,dd,hh,mi,format='(i4,"/",i02,"/",i02,"/tmp_",i4,3i02,"_",i02,".fst")')
    rad_file = rad_out_dir+string(yy,mo,dd,yy,mo,dd,hh,mi,format='(i4,"/",i02,"/",i02,"/",    i4,3i02,"_",i02,".fst")')
    print, tmp_file
    print, rad_file

    ;make directory if it does not exist
    dir_name = file_dirname(rad_file)
    if ~file_test(dir_name) then file_mkdir, dir_name

    ;create standard file
    file_delete, tmp_file, /allow_nonexistent
    file_delete, rad_file, /allow_nonexistent
    unit=102
    file_id=fstouv(unit=unit,file=tmp_file,/rw)
        key=fstecr(unit=unit,descr=ref_struct,data=mod_pr)
        key=fstecr(unit=unit,descr=qi_struct, data=mod_qi )
        key=fstecr(unit=unit,descr=tic_struct,data=tic_data)
        key=fstecr(unit=unit,descr=tac_struct,data=tac_data)
    stat = fstfrm(u=unit)
    
    ;compress file
    spawn, ['fstcompress','-fstin',tmp_file,'-fstout',rad_file], /noshell

    ;remove tmp file
    file_delete, tmp_file, /allow_nonexistent

    print, ''
endfor





end
