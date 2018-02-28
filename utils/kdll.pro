pro prep_data_ll, ref_lats_in, ref_lons_in, want_lats_in, want_lons_in, extend=extend, $   ;input
                  ref_pts, want_pts                                                        ;output
;prepare latitude and longitude data for call to fortran function

sz_ref  = size(ref_lats_in, /dim)
sz_want = size(want_lats_in,/dim)


if keyword_set(extend) then begin
    ;extend input array by one pixel on each boundary, this will be used to determine if points outside of the domain
    sz_aug = sz_ref+2
    ref_lats_aug = dblarr(sz_aug)
    ref_lons_aug = dblarr(sz_aug)
    ;inside of the augmented matrix
    ref_lats_aug[1:sz_aug[0]-2,1:sz_aug[1]-2] = ref_lats_in
    ref_lons_aug[1:sz_aug[0]-2,1:sz_aug[1]-2] = ref_lons_in
    ;left boundary  excluting top and bottom most rows
    i=0
    for j=1,sz_aug[1]-2 do begin
        lat_lon_extend, ref_lats_aug[i+2,j], ref_lons_aug[i+2,j], ref_lats_aug[i+1,j], ref_lons_aug[i+1,j], new_lat, new_lon
        ref_lats_aug[i,j] = new_lat
        ref_lons_aug[i,j] = new_lon
    endfor
    ;right boundary excluting top and bottom most rows
    i=sz_aug[0]-1
    for j=1,sz_aug[1]-2 do begin
        lat_lon_extend, ref_lats_aug[i-2,j], ref_lons_aug[i-2,j], ref_lats_aug[i-1,j], ref_lons_aug[i-1,j], new_lat, new_lon
        ref_lats_aug[i,j] = new_lat
        ref_lons_aug[i,j] = new_lon
    endfor
    ;bottom boundary    whole matrix
    j=0
    for i=0,sz_aug[0]-1 do begin
        lat_lon_extend, ref_lats_aug[i,j+2], ref_lons_aug[i,j+2], ref_lats_aug[i,j+1], ref_lons_aug[i,j+1], new_lat, new_lon
        ref_lats_aug[i,j] = new_lat
        ref_lons_aug[i,j] = new_lon
    endfor
    ;top boundary    whole matrix
    j=sz_aug[1]-1
    for i=0,sz_aug[0]-1 do begin
        lat_lon_extend, ref_lats_aug[i,j-2], ref_lons_aug[i,j-2], ref_lats_aug[i,j-1], ref_lons_aug[i,j-1], new_lat, new_lon
        ref_lats_aug[i,j] = new_lat
        ref_lons_aug[i,j] = new_lon
    endfor
endif else begin
    sz_aug = sz_ref
    ref_lats_aug = ref_lats_in
    ref_lons_aug = ref_lons_in
endelse

;convert degrees to radians and insure double precision 
;ref
nelem_ref = n_elements(ref_lats_aug)
aa = lindgen(nelem_ref)
ref_lats  = double(ref_lats_aug[aa])*!dtor
ref_lons  = double(ref_lons_aug[aa])*!dtor
;want
nelem_want = n_elements(want_lats_in)
aa = lindgen(nelem_want)
want_lats = double(want_lats_in[aa])*!dtor
want_lons = double(want_lons_in[aa])*!dtor


;compute x,y,z on unit sphere
;ref
ref_pts = dblarr(nelem_ref,3)
ref_pts[*,0] = sin(ref_lons)*cos(ref_lats)  ;x
ref_pts[*,1] = cos(ref_lons)*cos(ref_lats)  ;y
ref_pts[*,2] =               sin(ref_lats)  ;z
;want
want_pts = dblarr(nelem_want,3)
want_pts[*,0] = sin(want_lons)*cos(want_lats)  ;x
want_pts[*,1] = cos(want_lons)*cos(want_lats)  ;y
want_pts[*,2] =                sin(want_lats)  ;z

end





pro kdll, ref_lats_in, ref_lons_in, want_lats_in, want_lons_in, missing=missing,     $ ;input
          nearest=nearest, within_dist=within_dist_km, nmax=nmax, noextend=noextend, $ 
          proj_ind, nfound=nfound                                                      ;output
;given each coordinate defined in want_lats and want_lons, this routine 
;returns the nearest neighbor in ref_lat/ref_lon grids
;
;for example, 
;given data on a grid defined by ref_lat and ref_lon
;val = data[proj_ind]
;will have the same size as want_lat and want_lon and contain data from nearest points in ref_lats and ref_lons
;
;The nearest neighbor search is done using a kd-tree for efficiency
;
;   input:
;   ref_lats ; ref_lons         reference grid of coordinates
;   want_lats; want_lons        grid of coordinates with respect to which nearest neighbors will be estimated
;   nearest                     if this keyword is set, only the nearest neighbor is returned 
;   within_dist                 if this keyword is defined all nearest neighbor within distance "within_dist" (in km) will be returned
;                               this keyword needs to be called with the nmax keyword
;
;                               one and only one of "nearest" or "within_dist" must be defined
;
;   nmax                        specifies the maximum number of nearest neighbors expected to be found
;
;   output:
;   proj_ind                    indices to entries in the 'ref' data
;
;                               with the keyword nearest:
;                               proj_ind will have the same dimension as the "want" data
;                       
;                               with the keyword 'within_dist'
;                               proj_ind needs to 
;                               proj_ind will have dimension [n_elements(want_lats_in), 


;check if necessary keywords are set
if (n_elements(nearest) eq 0) and (n_elements(within_dist_km) eq 0) then message, 'One of the keywords "nearest" or "within_dist" must be used'
if n_elements(within_dist_km) ne 0 then begin
    if n_elements(nmax) eq 0 then message, 'The keyword "nmax" must be specified when using the keyword "within_dist"'

    ;convert within_dist from km to distances on the unit sphere
    within_dist = double(within_dist_km) / 6371.0072d    ;[km]
endif

;data check
;ref
sz1 = size(ref_lats_in,/dim)
sz2 = size(ref_lons_in,/dim)
if n_elements(sz1) ne 2 or n_elements(sz2) ne 2 then message, 'reference lat long should be 2D arrays'
if (sz1[0] ne sz2[0]) or (sz1[1] ne sz2[1]) then message, 'ref_lats and ref_lons must have same dimension'
if total(finite(ref_lats_in),/integer) ne n_elements(ref_lats_in) then message, 'Presence of non finite numbers in ref_lats_in'
if total(finite(ref_lons_in),/integer) ne n_elements(ref_lons_in) then message, 'Presence of non finite numbers in ref_lons_in'
sz_ref = sz1
sz_aug = sz1+2
;want
sz1 = size(want_lats_in,/dim)
sz2 = size(want_lons_in,/dim)
is_1d = 0
is_2d = 0
if n_elements(sz1) eq 1 and n_elements(sz2) eq 1 then begin
    is_1d = 1
    sz_want = sz1
endif else begin
    if n_elements(sz1) eq 2 and n_elements(sz2) eq 2 then begin
        is_2d = 1
        if (sz1[0] ne sz2[0]) or (sz1[1] ne sz2[1]) then message, 'wantlats and want_lons must have same dimension'
        sz_want = sz1
    endif else begin
        message, 'destination domain must be a 1D or 2D  array.'
    endelse
endelse

;default value for missing pts
if n_elements(missing) eq 0 then missing = -9999.

;by default extend grid when keyword nearest is set
if keyword_set(nearest) then extend = 1
;do not extent grid for 1D arrays
if is_1d eq 1 then extend = 0 
;do not extent grid if noextend keyword is set, this is to be used for global outputs
if keyword_set(noextend) then extend=0


;prepare lat lon data for passing to the fortran routine
prep_data_ll, ref_lats_in, ref_lons_in, want_lats_in, want_lons_in, extend=extend, $   ;input
              ref_pts, want_pts                                                         ;output
    
;find nearest neighbor
call_kdtree_search, ref_pts, want_pts, proj_ind_aug, missing=missing, nearest=nearest, within_dist=within_dist, nmax=nmax, nfound=nfound

;in the case of a nearest neighbor search, reform proj_ind into a 2D array the size of want_lats and want_lons
if (n_elements(nearest) ne 0) and (is_2d eq 1) then begin
    aa = lindgen(sz_want)
    proj_ind_aug = proj_ind_aug[aa]    
    
    ;find points outside of domain 
    if extend eq 1 then begin
        ;x and y components (in input grid) of projection indices (augmented matrix)
        y_ind = floor(proj_ind_aug/sz_aug[0])
        x_ind = proj_ind_aug - y_ind*sz_aug[0]
        ;list of points referring to the boundary; those are outside of the domain
        aa = where((x_ind eq 0) or (x_ind eq sz_aug[0]-1) or $
                   (y_ind eq 0) or (y_ind eq sz_aug[1]-1), naa)
        
        ;any points on input that is not finite will also set to missing
        bb = where(~finite(ref_lats_in), nbb)
        
        ;adjust indexes so that they reflect the non-augmented reference grid
        proj_ind = (y_ind-1)*sz_ref[0] + (x_ind-1)
        
        ;assign missing values to points outside of the domain and those where input data is not finite
        if naa ne 0 then proj_ind[aa] = missing
        if nbb ne 0 then proj_ind[bb] = missing
    endif else begin
        proj_ind = proj_ind_aug
    endelse
endif else begin
    proj_ind = proj_ind_aug
endelse

end


