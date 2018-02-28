
;;some quality controls being applied---------------------------------------
;if ~keyword_set(noproc) then begin
;
;    ;speckle filter
;    filt_l = 0.
;    median_filter, ref, filt_l, missing=missing, ind=median_ind
;    ;apply same median filter to variables other than ref
;    block_percent_filt = fltarr(sz)
;    valid_ind = where(median_ind ne missing, n_valid, complement=nodata_ind,ncomplement=num_nodata)
;    if n_valid ne 0 then begin
;        dum = median_ind[valid_ind]
;        jj = floor(dum / sz[0])
;        ii = dum - jj*sz[0]
;        block_percent_filt[valid_ind] = block_percent[median_ind[valid_ind]]
;    endif
;    ;block out nodata
;    if num_nodata ne 0 then begin
;        ref[nodata_ind] = missing
;        block_percent_filt[nodata_ind] = missing
;    endif
;    ;block percent is now filtered
;    block_percent = block_percent_filt
;
;    ;remove blocked echoes based on beam blockage
;    aa = where(block_percent ge 60., naa)
;    if naa ne 0 then begin
;        ref[aa] = missing
;    endif
;
;    ;based on intensity
;    ;aa = where(ref[good_ind] lt 15., naa)
;    ;if naa ne 0 then ref[good_ind[aa]] = 0.
;endif
