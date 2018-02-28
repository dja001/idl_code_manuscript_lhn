PRO ALPHAPIC, var, trans_range, alpha

    ;transparency range
    ;  
    ;max range
    ;  |
    ;  |        opaque
    ;  |
    ;max tr range
    ;  |
    ;  |
    ;min tr range
    ;  |
    ;  |        transparent
    ;  |
    ;min range

    

    sz = SIZE(var, /DIM)
    alpha = FLTARR(sz)                                      ;completely transparent everywhere

    aa = WHERE((var GE trans_range[0] ) AND (var LE trans_range[1]), count)    ;varying transparency
    IF count NE 0 THEN BEGIN
        LINMAP, var[aa], dummy, trans_range, [0.,1]
        alpha[aa] = dummy
    ENDIF
    aa = WHERE((var GE trans_range[1] ), count)    ;completely opaque where var gt max_tr_range
    IF count NE 0 THEN BEGIN
        alpha[aa] = 1.
    ENDIF

END
