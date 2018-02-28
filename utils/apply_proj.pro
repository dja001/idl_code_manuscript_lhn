pro apply_proj, in, proj_ind, missing=missing,    $ ;in
                out                                 ;out

    if n_elements(in) eq 0 then begin
        message, 'input variable is empty'
    endif

    if n_elements(proj_ind) eq 0 then begin
        message, 'variable proj_ind is empty'
    endif

    if ~keyword_set(missing) then begin
        missing = -9999.
    endif

    ;initialize out array
    sz = size(proj_ind,/dim)
    out = replicate(in[0], sz)*0. + missing

    ;deal with missing pts
    aa = where(proj_ind ne missing, naa)
    if naa gt 0 then begin
        out[aa]=in[proj_ind[aa]]
    endif

end
