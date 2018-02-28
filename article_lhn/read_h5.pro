pro read_h5, file=file, missing=missing, $
             lats=lats, lons=lons,       $
             ref=ref,   qced=qced,        nonqced=nonqced, $             
             block_percent=block_percent, tot_qi=tot_qi,         stat_filt=stat_filt, $
             attenuation=attenuation,     broadening=broadening, distance=distance,   $
             hit_acc_clut=hit_acc_clut
;read radar files in the h5 format and do whatever processing there needs to be done

;only get lat lon matrices for the composite domain
if arg_present(lats) or arg_present(lons) then begin
    lat_lon_file = '/local/raid/armadja/data/domains/radar_continental_2.5km.fst'
    ;lat_lon_file = '/fs/cetus/fs3/mrb/arma/armadja/idl_comp/radar_continental_2.5km.fst'
    get_gem_data, lat_lon_file, lat=lats, lon=lons,var_name='MSKC'
    if n_elements(file) eq 0 then return
endif

;timming
t0 = systime(/sec)
print, 'reading h5 file'

;default dataset
if ~(keyword_set(qced) xor keyword_set(nonqced)) then message, 'One, and only one, of the "qced" or "nonqced" keywords must be specified.'
if keyword_set(qced)    then want_data='DBZH'
if keyword_set(nonqced) then want_data='TH'

;test input h5 file
if n_elements(file) eq 0 then begin
    message, 'input file not defined'
endif
if ~file_test(file) then begin
    message, 'file: "'+file+'" does not exist'
endif
    ;for reference
    ;Result = H5_BROWSER(file)

;default value for missing
if n_elements(missing) eq 0 then missing = -9999.


;explore data structure and 
h5_str = h5_parse(file)
names = strlowcase(tag_names(h5_str))
;look for datasets
res = where(strmatch(names, 'dataset*',/fold_case), nres)
if nres eq 0 then message, 'No "datasets" found in file:'            +file
if nres gt 1 then message, 'More than one datasets found in file: '  +file
dataset_ind  = res
dataset_name = names[res]
;look for QCed vs non QCed data
names = strlowcase(tag_names(h5_str.(dataset_ind)))
res = where(strmatch(names, 'data*',/fold_case), nres)
if nres eq 0 then message, 'No "data" found in file:'                +file
;look in all data directory for the quantity we are looking for
found=0
for nn=0, nres-1 do begin
    if h5_str.(dataset_ind).(res[nn]).what.quantity._data eq want_data then begin
        data_ind  = res[nn]
        data_name = names[res[nn]]
        found=1
        break
    endif
endfor
if found eq 0 then message, 'The data: "'+want_data+'" was not found in file: '+file

;look for quality indexes
names = strlowcase(tag_names(h5_str.(dataset_ind).(data_ind)))
res = where(strmatch(names, 'quality*',/fold_case), nres)
if nres eq 0 then message, 'No "quality" found in file:'                +file
;retrieve what quantity is stored in each quality index
qual_names = strarr(nres)
for nn=0, nres-1 do begin
    qual_names[nn] = h5_str.(dataset_ind).(data_ind).(res[nn]).how.task._data
endfor

;name in H5 file                            output name in this IDL routine
;
;fi.fmi.ropo.detector.classification        stat_filt
;se.smhi.detector.beamblockage              block_percent
;pl.imgw.radvolqc.att                       attenuation
;pl.imgw.radvolqc.broad                     broadening
;se.smhi.composite.distance.radar           distance
;pl.imgw.quality.qi_total                   tot_qi          
;eu.opera.odyssey.hac                       hit_acc_clut

if arg_present(tot_qi) then begin
    this_ind = where(qual_names eq 'pl.imgw.quality.qi_total', nind)
    if nind eq 0 then message, '"tot_qi" is not found in file: '+file
    if nind gt 1 then message, 'Something weird going on'
    tot_qi_qual_ind  = this_ind
    tot_qi_qual_name = names[res[this_ind]]
endif
;if arg_present(tot_qi) then begin
;    this_ind = where(qual_names eq 'pl.imgw.quality.qi_total', nind)
;    if nind eq 0 then message, '"tot_qi" is not found in file: '+file
;    if nind gt 1 then message, 'Something weird going on'
;    tot_qi_qual_ind  = this_ind
;    tot_qi_qual_name = names[res[this_ind]]
;endif



;reading the h5 file
file_id = H5F_OPEN(file)

    ;get reflectivity attributes
    what_id = h5g_open(file_id,'/'+dataset_name+'/'+data_name+'/what')
        gain_id        = h5a_open_name(what_id,'gain')
        ref_gain       = h5a_read(gain_id)
        h5a_close,       gain_id
        offset_id      = h5a_open_name(what_id,'offset')
        ref_offset     = h5a_read(offset_id)
        h5a_close,       offset_id
        nodata_id      = h5a_open_name(what_id,'nodata')
        ref_nodata_val = h5a_read(nodata_id)
        h5a_close,       nodata_id
        undetect_id    = h5a_open_name(what_id,'undetect')
        ref_undetect   = h5a_read(undetect_id)
        h5a_close,       undetect_id
    h5g_close, what_id
    ;get reflectivity data
    data_id   = h5d_open(file_id,'/'+dataset_name+'/'+data_name+'/data')
    ref_byte  = h5d_read(data_id)
    h5d_close,  data_id
    
    ;;get blockage attributes
    ;what_id = h5g_open(file_id,'/dataset1/'+dataset+'/quality2/what')
    ;    gain_id      = h5a_open_name(what_id,'gain')
    ;    block_gain   = h5a_read(gain_id)
    ;    h5a_close,     gain_id
    ;    offset_id    = h5a_open_name(what_id,'offset')
    ;    block_offset = h5a_read(offset_id)
    ;    h5a_close,     offset_id
    ;h5g_close, what_id
    ;;get blockage data
    ;data_id = h5d_open(file_id,'/dataset1/'+dataset+'/quality2/data')
    ;block_byte = h5d_read(data_id)
    ;h5d_close, data_id

    if arg_present(tot_qi) then begin
        ;get total quality attributes
        qual_str = '/'+dataset_name+'/'+data_name+'/'+tot_qi_qual_name
        what_id = h5g_open(file_id,qual_str+'/what')
            gain_id      = h5a_open_name(what_id,'gain')
            tot_gain     = h5a_read(gain_id)
            h5a_close,     gain_id
            offset_id    = h5a_open_name(what_id,'offset')
            tot_offset   = h5a_read(offset_id)
            h5a_close,     offset_id
        h5g_close, what_id
        how_id = h5g_open(file_id,qual_str+'/how')
            task_id      = h5a_open_name(how_id,'task')
            tot_task     = h5a_read(task_id)
            h5a_close,     task_id
        h5g_close, how_id
        ;get total quality data
        data_id = h5d_open(file_id,qual_str+'/data')
        tot_byte = h5d_read(data_id)
        h5d_close, data_id
    endif

;done reading h5 file
h5f_close, file_id


;;transform byte data into useable quantities
ref_byte   = rotate(ref_byte,7)     ;flip vertically for compatibility with IDL index system
sz  = size(ref_byte,/dim)
ref = fltarr(sz)
nodata_ind = where(ref_byte eq ref_nodata_val, n_nodata, complement=good_ind,ncomplement=n_good)
if n_nodata ne 0 then ref[nodata_ind] = missing
if n_good   ne 0 then ref[good_ind]   = ref_byte[good_ind]*ref_gain + ref_offset

if arg_present(tot_qi) then begin
    tot_byte   = rotate(tot_byte,7)     ;flip vertically for compatibility with IDL index system
    sz  = size(tot_byte,/dim)
    tot_qi = fltarr(sz)
    if n_nodata ne 0 then tot_qi[nodata_ind] = missing
    if n_good   ne 0 then tot_qi[good_ind]   = tot_byte[good_ind]*tot_gain + tot_offset
endif





;
;block_byte = rotate(block_byte,7)
;tot_byte   = rotate(tot_byte,7)

;block_percent = fltarr(sz)
;tot_qi        = fltarr(sz)
;    block_percent[nodata_ind] = missing
;    tot_qi[nodata_ind]        = missing

    ;block_percent[good_ind] = 100.*(1. - (block_byte[good_ind]*block_gain + block_offset))
    ;tot_qi[good_ind]        =               tot_byte[good_ind]*  tot_gain +   tot_offset


tf = systime(/sec)
print, 'done reading file '
print, string(tf-t0), ' seconds'
    
end

