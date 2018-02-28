pro check_pos, pos
    ;insure validity of a position vector in normal coordinates
    ;pos = [x_bottom_left, y_bottom_left, x_top_right, y_top_right] 
    if n_elements(pos) eq 4 then begin
        range_ok = (pos ge 0.) and (pos le 1.)
        if total(range_ok) ne 4 then begin
            if (!d.name eq 'win') then newline = string([13b, 10b]) else newline = string(10b)
            print, 'position array:', pos
            message, 'elements of position array must be in the interval [0.,1.].'
        endif
    endif else begin
        message, 'the position vector provided must be a 4 element array.'
    endelse
end
