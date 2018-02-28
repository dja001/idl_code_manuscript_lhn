pro make_precip_accum
;takes in a bunch of standard files of instantaneous precipitation in dBZ, acumulate them and save output in other standard files

input_dir  = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5/'
output_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/accums_1h_for_francois/'
dt     = 10. ;temporal resoluton of data in inutes
acc_dt = 60. ;time of accumulations in minutes

t0 = julday(07,08,2014,00,00,00)
tf = julday(07,10,2014,00,00,00)

;internal variables & parameters
missing=-999.

fst_template = '/local/raid/armadja/data/radar_nat_composites/dearchived/2014070812_001' 
;template for fst data
get_gem_data, fst_template, getvar='PR', lat=mod_lat, lon=mod_lon
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
    key=fstinf(u=unit,nomvar='PR',date=cmc_timestamp)
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
this_r_file = input_dir+string(year,month,day,hour,minute,format='(i4,3i02,"_",i02,".fst")')
if ~file_test(this_r_file) then message, 'file:  '+this_r_file+'   is missing'
r_file_arr = this_r_file
count ++
this_time = t0 + count*dt/1440.
while this_time le tf do begin
    time_arr = [time_arr,this_time]
    caldat, this_time, month, day, year, hour, minute
    this_r_file = input_dir+string(year,month,day,hour,minute,format='(i4,3i02,"_",i02,".fst")')
    if ~file_test(this_r_file) then message, 'file:  '+this_r_file+'   is missing'
    r_file_arr = [r_file_arr,this_r_file]
    count ++
    this_time = t0 + count*dt/1440. + 1e-5
endwhile

;accum_time array
count = 0l
this_time = t0 + count*acc_dt/1440.
acc_time_arr = this_time
while this_time le tf do begin
    count ++
    this_time = t0 + count*acc_dt/1440. + 1e-5
    acc_time_arr = [acc_time_arr,this_time]
endwhile


;accumulate radar data and make standard files
n_acc = n_elements(acc_time_arr)
counter = 0
for nn=0, n_acc-1 do begin

    ;get files composing this accumulation
    aa = where(time_arr gt acc_time_arr[nn] - 60d/1440.+1e-5 and time_arr le acc_time_arr[nn], nfiles)
    nparts = acc_dt/dt
    if nfiles eq nparts then begin
        ;get_data
        rnx = 1108
        rny = 1082
        accum_arr = fltarr(rnx,rny, nparts)
        these_files = r_file_arr[aa]
        these_times = time_arr[aa]
        for jj=0, nparts-1 do begin
            jul_to_cmc,   these_times[jj], cmc_time
            get_gem_data, these_files[jj], var_name='RDBR', values=rad_ref, cmc_timestamp=cmc_time
            get_gem_data, these_files[jj], var_name='RDQI', values=rad_qi,  cmc_timestamp=cmc_time
            ;conversion from reflectivity to mm
            good = where(rad_ref ne missing, ngood, complement=bad, ncomplement=nbad)
            rad_mm = fltarr(1108, 1082)
            if ngood ne 0 then rad_mm[good] = (10.0^(rad_ref[good]/16.) / 27.424818) * 10./60.      ;mm/h * 10min/60min
            if nbad  ne 0 then rad_mm[bad]  = missing
            ;save data
            accum_arr[*,*,jj] = rad_mm
        endfor
        ;do the accumulation
        accum = fltarr(rnx,rny)
        qi    = fltarr(rnx,rny)
        for ii=0,rnx-1 do begin
            for jj=0,rny-1 do begin
                vals = accum_arr[ii,jj,*]
                bb = where(vals ne missing, nbb)
                if nbb eq 0 then begin
                    accum[ii,jj] = missing
                       qi[ii,jj] = missing
                endif else begin
                    accum[ii,jj] = total(vals[bb])
                       qi[ii,jj] =  mean(vals[bb])
                endelse
            endfor
        endfor

        ;data structure from template
        jul_to_cmc, acc_time_arr[nn], cmc_time
        ;adjust data struct for reflectivity
        ref_struct = pr_struct
        ref_struct.nomvar = 'ACMM'
        ref_struct.date = cmc_time
        ;adjust data struct for reflectivity
        qi_struct = pr_struct
        qi_struct.nomvar = 'ACQI'
        qi_struct.date = cmc_time

        ;create and save standard file
        tmp_file = output_dir+'tmp.fst'
        caldat, acc_time_arr[nn], mo, dd, yy, hh, mi, ss
        rad_file = output_dir+string(yy,mo,dd,hh,mi,format='(i4,4i02,".fst")')
        file_delete, tmp_file, /allow_nonexistent
        file_delete, rad_file, /allow_nonexistent
        unit=102
        file_id=fstouv(unit=unit,file=tmp_file,/rw)
            key=fstecr(unit=unit,descr=ref_struct,data=accum)
            key=fstecr(unit=unit,descr=qi_struct, data=qi )
            key=fstecr(unit=unit,descr=tic_struct,data=tic_data)
            key=fstecr(unit=unit,descr=tac_struct,data=tac_data)
        stat = fstfrm(u=unit)
        
        ;compress file
        spawn, ['fstcompress','-fstin',tmp_file,'-fstout',rad_file], /noshell

        ;remove tmp file
        file_delete, tmp_file, /allow_nonexistent

        print, 'done with :' ,rad_file
    endif

    print, ''

endfor



end
