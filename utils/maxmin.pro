pro maxmin, in, name=name, missing=missing, nan=nan
    ;change name to avoid modifying the iput
    input = in

    ;init name
    if n_elements(name) eq 0 then begin
        name = ''
    endif

    ;remove missing values if desired
    if n_elements(missing) ne 0 then begin
        aa = where(input ne missing,caa)
        if caa ne 0 then begin
            input = input[aa]
        endif else begin
            message, 'all input are equal to the missing value', /informational
        endelse
    endif 

    ;print, maximum and minimum value of in
    min = min(input, max=max, nan=nan)
    print, name+'  min:',min, '         max:',max

end
