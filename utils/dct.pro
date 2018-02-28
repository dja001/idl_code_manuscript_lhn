PRO DCT, in, out, AC=ac
    ;computes fourrier cosine transform on a field using mirroring
    ;out is power array and has same size as in

    ;get size
    sz = SIZE(in, /DIM)
    IF N_ELEMENTS(sz) EQ 1 THEN BEGIN       ;1D fields
        ;mirror field
        bl = in
        br = ROTATE(bl, 5)
        comp = [bl, br]

        ;remove mean        ;not necessary for DCT but necessary for AC computations
        comp -= MEAN(comp)

        ;FFT
        comp_fft = FFT(comp)

        ;power spectrum
        u_power = REAL_PART(CONJ(comp_fft)*comp_fft)
        out = u_power[0:sz[0]-1]

        IF ARG_PRESENT(ac) THEN BEGIN
            ac_mirror = REAL_PART(FFT(u_power, /INVERSE))
            ac = ac_mirror[0:sz[0]-1]
            ;normalize ac
            ac /= MAX(ac)
        ENDIF
    ENDIF ELSE BEGIN

        IF N_ELEMENTS(sz) EQ 2 THEN BEGIN   ;2D fields

            ;mirror field
            bl = in
            br = ROTATE(bl, 5)
            comp = [bl, br]
            comp = [[ROTATE(comp,2)], [comp]]

            ;remove mean        ;not necessary for DCT but necessary for AC computations
            comp -= MEAN(comp)

            ;FFT
            comp_fft = FFT(comp)

            ;power spectrum
            u_power = REAL_PART(CONJ(comp_fft)*comp_fft)
            out = u_power[0:sz[0]-1,0:sz[1]-1]

            IF ARG_PRESENT(ac) THEN BEGIN
                ac_mirror = REAL_PART(FFT(u_power, /INVERSE))
                ac = ac_mirror;[0:sz[0]-1,0:sz[1]-1]
                ;normalize ac
                ac /= MAX(ac)
            ENDIF


        ENDIF ELSE BEGIN
            print, 'only works with 1D or 2D field so far...'
            stop
        ENDELSE
    ENDELSE

END
