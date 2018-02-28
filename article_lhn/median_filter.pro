pro median_filter, data, this_l, missing=missing, ind=ind
;applies a median filter with a certain radius

if n_elements(data) eq 0 then message, 'must provide some data to median filter'
sz = size(data,/dim)
n_dim = n_elements(sz)

if n_elements(missing) eq 0 then missing = -9999.

if n_elements(this_l) eq 0 then message, 'must provide a length for median filter'
if this_l lt 0. then message, 'Length of median filter must be greater of equal to zero'
if this_l eq 0. then begin
    ;no filtering done with l=0
    ind = lonarr(sz)+long(missing)
    good = where(data ne missing,ngood)
    ind[good] = good
    return     
endif

case n_dim of
    1: begin
            nx = sz[0]

            ;array keeping track of where filtered data came from
            ind = lonarr(nx,ny)+long(missing)

            ;make index for region of filtering
            sq_nx=2L*this_l+1L
            aa = findgen(sq_nx) - this_l

            data_in = data
            data = fltarr(nx)
            for ii=this_l+1, nx-this_l-1 do begin
                data[ii] = median(data_in[ii+aa])
            endfor
       end
    2: begin
            nx = sz[0]
            ny = sz[1]

            ;matrix keeping track of where filtered data came from
            ind = lonarr(nx,ny)+long(missing)

            ;make index for this circle
            sq_nx=2L*this_l+1L
            xx = rebin(findgen(sq_nx),sq_nx,sq_nx) - this_l
            yy = rotate(xx,1)
            aa = where(sqrt(xx^2.+yy^2.) le this_l, nc)
            xx_vals = xx[aa]
            yy_vals = yy[aa]

            data_in = data
            data = fltarr(nx, ny)
            count=0l
            for ii=this_l+1, nx-this_l-1 do begin
                i_ind = ii+xx_vals
                for jj=this_l+1, ny-this_l-1 do begin
                    j_ind = jj+yy_vals
                    c_ind = j_ind*nx + i_ind    ;compound index

                    ;filter out nodata
                    o_data = data_in[c_ind]     ;original data  obtained with c_ind
                    aa = where(o_data ne missing, naa)
                    if naa le 2 then begin      ;no or very few valid data points
                        data[ii,jj] = missing
                        ind[ii,jj]  = missing
                    endif else begin
                        ;enough valid data pts to proceed
                        g_data = o_data[aa]         ;good data, contains only valid measurements
                        bb = sort(g_data)
                        data[ii,jj] =   g_data[bb[naa/2]]   ;median of all valid data
                        ind[ii,jj]  = c_ind[aa[bb[naa/2]]]  ;index of selected pixel in the original array
                    endelse
                endfor
                if ii mod 300 eq 0 then print, string(100.*ii/(nx-this_l-1)), ' percent done'
            endfor
       end
       else: message, 'Only 1D and 2D fields supported'
endcase
end
