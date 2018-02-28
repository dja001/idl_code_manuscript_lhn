pro read_h5_noproc, file=file, ref=ref, block_percent=block_percent, $
             lats=lats, lons=lons, missing=missing
;read radar files in the h5 format and do whatever processing there needs to be done

;only get lat lon matrices for the composite domain
if arg_present(lats) or arg_present(lons) then begin
    lat_lon_file = '/local/drive2/arma/armadja/data/domains/radar_continental_2.5km.fst'
    get_gem_data, lat_lon_file, lat=lats, lon=lons,getvar='MSKC'
endif


;we are reading a h5 file
if n_elements(file) ne 0 then begin

    if ~file_test(file) then begin
        message, 'file: "'+file+'" does not exist'
    endif
    ;Result = H5_BROWSER(file)

    ;default value for missing
    if n_elements(missing) eq 0 then missing = -9999.
    
    ;reading the h5 file
    file_id = H5F_OPEN(file)

        ;get reflectivity attributes
        what_id = h5g_open(file_id,'/dataset1/data2/what')
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
        ;get reflectivity data
        data_id = h5d_open(file_id,'/dataset1/data2/data')
        ref_byte = h5d_read(data_id)
        h5d_close, data_id
        
        ;get blockage attributes
        what_id = h5g_open(file_id,'/dataset1/data2/quality2/what')
            gain_id      = h5a_open_name(what_id,'gain')
            block_gain   = h5a_read(gain_id)
            h5a_close,     gain_id
            offset_id    = h5a_open_name(what_id,'offset')
            block_offset = h5a_read(offset_id)
            h5a_close,     offset_id
        h5g_close, what_id
        ;get blockage data
        data_id = h5d_open(file_id,'/dataset1/data2/quality2/data')
        block_byte = h5d_read(data_id)
        h5d_close, data_id

    ;done reading h5 file
    h5f_close, file_id

    ;flip vertically for compatibility with IDL index system
    ref_byte   = rotate(ref_byte,7)
    block_byte = rotate(block_byte,7)

    ;locations of nodata
    nodata_ind = where(ref_byte eq ref_nodata, num_nodata)

    ;get reflectivity
    ref = ref_byte*ref_gain + ref_offset

    ;percentage of beam blockage
    block_percent = 1. - (block_gain*block_byte + block_offset)


    ;;some quality controls being applied

    ;;based on beam blockage
    ;aa = where(block_percent gt .5, naa)
    ;if naa ne 0 then begin
    ;    ref[aa] = missing
    ;    block_percent[aa] = 10.
    ;endif

    ;;based on intensity
    ;aa = where(ref lt 15., naa)
    ;if naa ne 0 then ref[aa] = 0.

    ;;speckle filter
    ;median_filter, ref, 4.

    ;block out nodata
    if num_nodata ne 0 then begin
        ref[nodata_ind] = missing
        block_percent[nodata_ind] = missing
    endif
    
endif
end

