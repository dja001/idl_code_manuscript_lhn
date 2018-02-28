pro print_fortran, lun, cat, varr
    ;prints an array in the fortran format

    ;number of elements to process
    nl = N_ELEMENTS(varr)
    
    ;print, header and category
    printf, lun, ''
    printf, lun, ''
    printf, lun, cat
    printf, lun, ''

    printf, lun, FORMAT="('(/',$)"
    for nn=0, nl-2 do begin
        if (nn ne 0) and ((nn mod 5) eq 0) then printf, lun, FORMAT="('  ',$)"
        if ((nn+1) mod 5) eq 0 then begin
            format = "(1e10.3,', &')"
        endif else begin
            format = "(1e10.3,',',$)"
        endelse
        printf, lun, varr[nn], FORMAT=format
    endfor
    format = "(1e10.3,' /)')"
    printf, lun, varr[nl-1], FORMAT=format
end

pro process_typical, avg_p=avg_p, cat=cat, values=values

;reads typical.txt files , and return arrays of avg pres and values for different categories

file='~/idl/article_lhn/typical.txt'
nl = FILE_LINES(file)
openr, lun, file, /get_lun
    dummy = ''
    readf, lun, dummy
    print, dummy
    cat = strsplit(dummy,',',/extract)
    ncat = n_elements(cat)
    ;initialize output arrays
    values = fltarr(nl-1,ncat)
    for nn=0,nl-2 do begin
        readf, lun, dummy
        val = strsplit(dummy,',',/extract)
                           values[nn,0 ] = float(val[0 ])*100.  ;pressure from hPa to Pa
        for kk=1,ncat-1 do values[nn,kk] = float(val[kk])
    endfor
free_lun, lun



;;write values in fortran format

file = '~/idl/article_lhn/typical_fortran.txt'
openw, lun, file, /get_lun
    printf,lun, (values[1,0] - values[0,0]), format='("delta_p:",1e10.3)'
    printf,lun, ''
    printf,lun, 'number of elements: ', nl-1

    for kk=0, ncat-1 do print_fortran, lun, cat[kk], values[*,kk]
free_lun, lun

END
