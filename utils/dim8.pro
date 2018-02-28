FUNCTION DIM8, in
    ;returns number of elements of 8 possible dimensions of an array,
    ;returns 1 element for scalars

    ;up to 8 dimensions are processed
    nd = SIZE(in, /N_DIM)   ;number of dimension 
    sz = SIZE(in, /DIM)     ;size of each dimensions

    ;init output array with zero for each dims
    dim = LONARR(8) 

    ;fill dim arr with values in sz
    IF nd EQ 0 THEN BEGIN
        dim[0] = 1
    ENDIF ELSE BEGIN
        dim[0:nd-1] = sz[0:nd-1]
    ENDELSE
    
    RETURN, dim
END

