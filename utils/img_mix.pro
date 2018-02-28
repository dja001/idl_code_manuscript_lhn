PRO IMG_MIX, out, img1, img2, alpha
    ;blends images with respective weights
    ;
    ;img2 is overlaid over img1
    ;alpha is 0-1 in range and is weight associated with img2 
    ;             0 = img2 completely transparent
    ;
    ;img1 and img2 should be true color images [n, n, 3]

    sz_1 = SIZE(img1, /DIM)
    sz_2 = SIZE(img2, /DIM)
    sz_3 = SIZE(alpha, /DIM)

    IF (sz_1[0] NE sz_2[0]) OR (sz_1[1] NE sz_2[1]) OR (sz_1[0] NE sz_3[0]) OR (sz_1[1] NE sz_3[1]) THEN BEGIN
        print, 'img1', sz_1
        print, 'img2', sz_2
        print, 'alpha', sz_3
        MESSAGE, 'dimension mismatch between input parameters'
    ENDIF 

    out = FLTARR(sz_1)

    ;the actual blend
    FOR k=0, 3-1 DO BEGIN
        out[*,*,k]=BYTE((img1[*,*,k]*(1.-alpha)) + alpha*img2[*,*,k])
    ENDFOR

END

