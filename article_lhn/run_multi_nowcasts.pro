pro read_std_struc, file,     jul_time, var_name,  $ ;in
                    tic_str,  tic_dat,             $ ;out
                    tac_str,  tac_dat,             $
                    rain_str, rain_dat
    ;read std file and return content in the form of a structure + data
    if ~file_test(file) then message, 'radar file missing:  "'+file+'"'

    ;get cmc time
    jul_to_cmc, jul_time, cmc_timestamp

    ;read data
    unit=102
    file_id=fstouv(unit=unit,file=file)
    
        ;insure that cmc_timestamp is a long integer        this really should be done in the fst library...
        if n_elements(cmc_timestamp) ne 0 then begin
            if ~isa(cmc_timestamp, 'long') then begin
                cmc_timestamp = long(cmc_timestamp)
            endif
        endif
        ;obtain fst
        key=fstinf(u=unit,nomvar=var_name,date=cmc_timestamp)
        err_catch, unit, key
        ;get idl structure with all fields describing the entry
        rain_str = fstprm(r=key)
        ;get precip data
        rain_dat = fstluk(r=key)
    
        ;get associated >> and ^^
        rain_ig1 = rain_str.ig1
        rain_ig2 = rain_str.ig2
        ;>>
        found = 0
        key=fstinf(u=unit,nomvar='>>')
        while key gt 0 do begin
            tic_str = fstprm(r=key)
            print, tic_str.ip1 , rain_ig1 , tic_str.ip2 , rain_ig2
            if (tic_str.ip1 eq rain_ig1) and (tic_str.ip2 eq rain_ig2) then begin
                found = 1
                break
            endif
            key=fstinf(u=unit,nomvar='>>',/sui)
        endwhile
        if found ne 1 then begin
            message, 'did not find appropriate >> field', /informational
            tic_str = 'n/a'
            tic_dat = 'n/a'
        endif else begin
            tic_dat   = fstluk(r=key)
        endelse
        ;>>
        found = 0
        key=fstinf(u=unit,nomvar='^^')
        while key gt 0 do begin
            tac_str = fstprm(r=key)
            if (tac_str.ip1 eq rain_ig1) and (tac_str.ip2 eq rain_ig2) then begin
                found = 1
                break
            endif
            key=fstinf(u=unit,nomvar='^^',/sui)
        endwhile
        if found ne 1 then begin
            message, 'did not find appropriate ^^ field', /informational
            tac_str = 'n/a'
            tac_dat = 'n/a'
        endif else begin
            tac_dat   = fstluk(r=key)
        endelse
    stat = fstfrm(u=unit)
end


pro run_multi_nowcasts
;procedure to prepare data and run nowcast suite for many cases

;begin and end time of analysis hours
;   nowcast will be run some hours later
t0 = julday(07, 02, 2014, 00, 00, 00)
tf = julday(07, 31, 2014, 00, 00, 00)
analy_dt   = 12d ;hours
nowcast_lag = 3d ;hours
radar_dt = 10d   ;minutes
now_nradars = 5  ;number of radar timeframes used by the nowcasting tool
radar_base_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_12_avg_09_min/'
now_tmp_dir = '/fs/cetus/fs3/mrb/arma/armadja/nowcast_workdir/source_data/'
now_dir = '/fs/cetus/fs3/mrb/arma/armadja/nowcast_workdir/'
cfg_dir = '/users/dor/arma/dja/mylib/nowcast_suite/cfg/'
fcst_base_dir = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/nowcasts/'
nfcsts = 37 ;number of maple nowcast
fcst_dt= 10d;time resolution of maple forecast dt


;from a sample rdps file:
sample_file = '/fs/cetus/fs3/mrb/arma/armadja/lhn_test_input/r_data_h5_v7_12_avg_09_min//2014/07/02/2014070202_20.fst'
sample_time = julday(07,02,2014,02,20,00)
;   1- get lats and lons of RDPS grid
get_gem_data, sample_file, lat=rdps_lat, lon=rdps_lon, var_name='RDPR'
;   2- save tic tacs of RDPS grid
read_std_struc, sample_file, sample_time, 'RDPR',   $ ;in
                rdps_tic_str,  rdps_tic_dat,        $ ;out
                rdps_tac_str,  rdps_tac_dat,        $
                rdps_str,      rdps_dum


;get lat lon from a sample URP composite 
urp_dom_file = '/local/raid/armadja/data/domains/urp_radar_composite.fst'
urp_dom_time = julday(07,03,2014,09,50,00)
get_gem_data, urp_dom_file, lat=urp_lat, lon=urp_lon, var_name='RDBR'
;get data structure for urp composite grid
read_std_struc, urp_dom_file, urp_dom_time, 'RDBR',   $ ;in
                tic_str,  tic_dat,          $ ;out
                tac_str,  tac_dat,          $
                urp_str,  urp_dummy

;projection indices between the URP composite and the RDPS grid
kdll, urp_lat, urp_lon, rdps_lat, rdps_lon, missing=missing, /nearest, $ ;input
      urp_to_rdps                                                        ;output
kdll, rdps_lat, rdps_lon, urp_lat, urp_lon, missing=missing, /nearest, $ ;input
      rdps_to_urp                                                        ;output

count=0d
this_time = t0
while this_time le tf do begin

    ;time of nowcast is offet with analysis time
    nowcast_t0 = this_time + nowcast_lag/24d
    caldat, nowcast_t0, mo, dd, yy, hh, mi, ss
    print, yy, mo, dd, hh, mi, ss
    print, ''

    ;clean nowcast work dir
    cmd = 'rm -f '+now_dir+'/*'
    spawn, cmd
    cmd = 'rm -f '+now_tmp_dir+'/*'
    spawn, cmd

    ;copy and transform radar mm/h precip data into dbz as this is what the nowcasting tool uses
    r_fnames = strarr(now_nradars)
    for rr=now_nradars-1, 0, -1 do begin
        ;time fo this radar file
        r_time = nowcast_t0 - rr*radar_dt/1440d
        jul_to_cmc, r_time, cmc_time

        ;make filename from date
        caldat, r_time, rmo, rdd, ryy, rhh, rmi, rss
        ryy = string(ryy,format='(i04)')
        rmo = string(rmo,format='(i02)')
        rdd = string(rdd,format='(i02)')
        rhh = string(rhh,format='(i02)')
        rmi = string(rmi,format='(i02)')
        rad_file = radar_base_dir+'/'+ryy+'/'+rmo+'/'+rdd+'/'+ryy+rmo+rdd+rhh+'_'+rmi+'.fst'

        ;read data
        read_std_struc, rad_file, r_time, 'RDPR',   $ ;in
                        tic_str,  tic_dat,          $ ;out
                        tac_str,  tac_dat,          $
                        rain_str, rain_mmh

        ;transform mm/h to dBZ
        rain_dbz = fltarr(size(rain_mmh,/dim))
        aa = where(rain_mmh gt 0., naa)
        if naa ne 0 then rain_dbz[aa] = 16d*alog10(27.424818*rain_mmh[aa])

        ;interpolate onto URP composite grid for nowcasting
        rain_dbz = rain_dbz[rdps_to_urp]

        ;change name of rain variable
        urp_str.nomvar = 'RDBZ'
        urp_str.etiket = 'IDL_CONVERT'
        ;adjust validity date of rain variable
        urp_str.date = cmc_time

        ;write data in tmp dir
        tmp_file  = now_tmp_dir+'tmp.fst'
        dest_file = now_tmp_dir+'/'+ryy+rmo+rdd+rhh+'_'+rmi+'.fst'
        ;create standard file
        unit=102
        file_id=fstouv(unit=unit,file=tmp_file,/rw)
            key=fstecr(unit=unit,descr=urp_str,data=rain_dbz)
            ;key=fstecr(unit=unit,descr=tic_str, data=tic_dat)
            ;key=fstecr(unit=unit,descr=tac_str, data=tac_dat)
        stat = fstfrm(u=unit)
        ;compress file and remove tmp file
        spawn, ['fstcompress','-fstin',tmp_file,'-fstout',dest_file], /noshell
        file_delete, tmp_file, /allow_nonexistent

        ;save name of this file for later use
        r_fnames[(now_nradars-1)-rr] = file_basename(dest_file)

    endfor

    ;modif config file
    file_string = '"'+strjoin(r_fnames,' ')+'"'
    to_extrapol_file = r_fnames[now_nradars-1]
    cmd = "cat "+cfg_dir+"template.cfg | sed -e 's/__flist__/"+file_string+"/g' -e 's/__to_extrapol__/"+to_extrapol_file+"/g' > "+cfg_dir+"dominik.cfg"
    spawn, cmd

    ;run advection program
    ;must have sourced the following before:
     ;. ssmuse-sh -d /ssm/net/rpn/diag/16.2
    cmd = '/users/dor/arma/dja/mylib/nowcast_suite/scripts/dominik.bash'
    spawn, cmd

    ;divide output, convert fo mmh and place in nowcast dir as if they were actual radar measurements
    for ff=0, nfcsts-1 do begin
        f_time = nowcast_t0 + ff*fcst_dt/1440.
        caldat, f_time, fmo, fdd, fyy, fhh, fmi, fss
        fyy = string(fyy,format='(i04)')
        fmo = string(fmo,format='(i02)')
        fdd = string(fdd,format='(i02)')
        fhh = string(fhh,format='(i02)')
        fmi = string(fmi,format='(i02)')
        fcst_file = fcst_base_dir+'/'+fyy+'/'+fmo+'/'+fdd+'/'+fyy+fmo+fdd+fhh+'_'+fmi+'.fst'

        ;make dir if it does not exist
        dir_name = file_dirname(fcst_file)
        if ~file_test(dir_name) then file_mkdir,dir_name

        ;get content at desired time
        read_std_struc, now_dir+'radar_extrapole.std', f_time, 'RDBZ',   $ ;in
                        tic_str,  tic_dat,  $ ;out
                        tac_str,  tac_dat,  $
                        rain_str, now_dbz

        ;the nowcast domain has been squared, remove extra columns to fit URP composite domain
        now_dbz = now_dbz[*,0:1499]
            
        ;interpolate to RDPS grid
        rdps_dbz = now_dbz[urp_to_rdps]

        ;;transform dBZ to mm/h
        rdps_mmh = fltarr(size(rdps_dbz,/dim))-999.
        aa = where(rdps_dbz gt 0., naa)
        if naa ne 0 then rdps_mmh[aa] = 10d^(rdps_dbz[aa]/16d) / 27.424818   ;precip rate in mm/hr

        ;change date of rain variable
        jul_to_cmc, f_time, cmc_timestamp
        rdps_str.date = long(cmc_timestamp)

        ;write data in tmp dir
        tmp_file  = now_tmp_dir+'tmp.fst'
        file_delete, tmp_file, /allow_nonexistent
        file_delete, fcst_file, /allow_nonexistent
        ;create standard file
        unit=102
        file_id=fstouv(unit=unit,file=tmp_file,/rw)
            key=fstecr(unit=unit,descr=rdps_str,     data=rdps_mmh)
            key=fstecr(unit=unit,descr=rdps_tic_str, data=rdps_tic_dat)
            key=fstecr(unit=unit,descr=rdps_tac_str, data=rdps_tac_dat)
        stat = fstfrm(u=unit)
        ;compress file and remove tmp file
        spawn, ['fstcompress','-fstin',tmp_file,'-fstout',fcst_file], /noshell
        file_delete, tmp_file, /allow_nonexistent

    endfor


    count ++
    this_time = t0 + count*analy_dt/24d 
endwhile



end
