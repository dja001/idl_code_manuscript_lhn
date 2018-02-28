pro pow_avg, pow_2d, k_nbins, min_k, max_k, k_arr, pow_arr, knum=knum, bounds=karr_bounds

;given a 2d power spectrum


;determine wavenumber of every point in the domain
sz = size(pow_2d, /dim)
if n_elements(sz) ne 2 then begin
    print, 'only workd for 2d fields'
    stop
endif
xx = rebin(findgen(sz[0]), sz[0], sz[1])
yy = rebin(rotate(findgen(sz[1]), 1), sz[0], sz[1])
knum = sqrt(xx^2. + yy^2.)      

;array of bin bounds in log units
karr_bounds_log = findgen(k_nbins+1)/(k_nbins) * (alog(max_k) - alog(min_k)) + alog(min_k)
;array of bin bounds in linear units
karr_bounds = exp(karr_bounds_log)

;initialize array values
k_arr = findgen(k_nbins)
pow_arr = findgen(k_nbins)

;average power in each k-value bins
for i=0, k_nbins-1 do begin
    ;boundaries of this bin
    low  = karr_bounds[i]
    high = karr_bounds[i+1]
    ;center value of bin
    k_arr[i] = ((karr_bounds[i]+karr_bounds[i+1])/2.)

    ;find all data entries in this bin
    aa = where(((knum ge low) and (knum lt high)), count)
    if count ne 0 then begin
        pow_arr[i] = mean(pow_2d[aa])   ;mean values if data was found
        ;print, aa
        ;if i gt 5 then stop
    endif else begin
        pow_arr[i] = !values.f_nan      ;nan if nothing in bin
    endelse
endfor



end
