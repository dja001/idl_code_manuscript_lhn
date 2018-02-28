pro ps_start, ps_name, xsz, ysz, font=font, charsize=charsize, thickness=thickness, white=white

;define circle for psym plots
angle = findgen(49)/48*2.*!pi
usersym, sin(angle) , cos(angle) , thick=2., /fill

;system variables for ps
set_plot, 'ps'
loadct, 0, /silent
;device, /portrait
!p.font=0
!p.charthick=4
if keyword_set(thickness) then begin
    !x.thick=thickness
    !y.thick=thickness
    !p.thick=thickness
endif else begin
    !x.thick=1
    !y.thick=1
    !p.thick=3.
endelse
if keyword_set(charsize) then begin
    !p.charsize=charsize
endif else begin
    !p.charsize=.9
endelse
!p.color=-1

if n_elements(font) ne 0 then begin
    if font eq 'lmroman' then begin
        device, set_font='LMRoman10-Regular'    ;use latin modern roman fonts
    endif else begin
        device, /helvetica                      ;default to helvetica
    endelse
endif else begin
    device, /helvetica                      ;default to helvetica
endelse
device, file=ps_name, /color,bits=8,xsize=xsz, ysize=ysz, xoff=0.0, yoff=0.0, /encapsulated

;white background if required
if keyword_set(white) then begin
    tv, [255], 0., 0, xs=1., ys=1., /normal
endif

END
