pro call_kdtree_search, ref_pts, want_pts, ind, missing=missing, nearest=nearest, within_dist=within_dist_in, nmax=nmax, nfound=nfound
;wrapper to call fortran routine that makes the kdtree and computes nearest neighbours
;
;   input:
;   ref_pts[0:n_pts-1,0:nd-1]   reference points for building the kd_tree  n_pts is the number of points; nd is the dimension of the data
;   want_pts[0:n_w-1,0:nd-1]    list of points with respect to which nearest neighbors are desired n_w is the number of desired points; nd is the dimension of the data
;                               nd must be the same for ref_pts and want_pts
;   nearest                     if this keyword is set, only the nearest neighbor is returned 
;   within_dist                 if this keyword is defined all nearest neighbor within distance "within_dist" (in km) will be returned
;                               one and only one of "nearest" or "within_dist" must be defined
;                               this keyword needs to be called with the nmax keyword
;   nmax                        specifies the maximum number of nearest neighbors expected to be found
;   output:
;   ind[0:n_w-1]                index pointing to the nearest neighbor (in ref_pts) to each element of want_pts

;check if necessary keywords are set
if (n_elements(nearest) eq 0) and (n_elements(within_dist_in) eq 0) then message, 'One of the keywords "nearest" or "within_dist" must be used'
if n_elements(within_dist_in) ne 0 then begin
    if n_elements(nmax) eq 0 then message, 'The keyword "nmax" must be specified when using the keyword "within_dist"'
endif

;define and check fortran lib file
shared_obj = '/users/dor/arma/dja/fortran/kdtree2/src-f90/f_kdtree_search.so'
if (file_test(shared_obj) ne 1) then message, "shared_obj does not exist"

;row <==> colum such that fortran code gets what it expects     also insure that data type is double
ref_pts  = double(transpose(ref_pts))
want_pts = double(transpose(want_pts))

;get size of ref_pts array 
sz = size(ref_pts,/dim)
nd    = long(sz[0])
nhave = long(sz[1])

;get size of want_pts array 
sz = size(want_pts,/dim)
if sz[0] ne nd then message, 'Please insure that the last dimention of ref_pts and want_pts is the same'
nwant = long(sz[1])

if n_elements(nmax) ne 0 then begin
    nmax = long(nmax)   ;insure nmax is a long integer
endif else begin
    nmax = 1l
endelse 

if n_elements(nearest) ne 0 then begin
    ;initialize output array
    ind = lonarr(nwant,nmax) + long(missing)
    
    stat = CALL_EXTERNAL(shared_obj, 'f_kdtree_nearest_', $                                     ;call to fortran library wrapper
           nd, nhave, nwant, nmax, ref_pts, want_pts, ind, /unload)                             ;input/output
endif

if n_elements(within_dist_in) ne 0 then begin

    ;fortran code takes distances squared
    within_dist = double(within_dist_in^2d)

    ;initialize output arrays
    ind = lonarr(nwant,nmax) + long(missing)
    nfound = lonarr(nwant) 

    stat = CALL_EXTERNAL(shared_obj, 'f_kdtree_nwd_', $                                         ;call to fortran library wrapper
           nd, nhave, nwant, nmax, ref_pts, want_pts, ind, within_dist, nfound, /unload)        ;input/output
endif

;remove 1 to go from fortran indices to idl indices
ind -= 1

end
