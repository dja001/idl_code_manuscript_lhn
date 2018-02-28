PRO CLEAN_PATH

;remove unused stuff from !PATH

dummy = !PATH
;print, 'Before'
;print, dummy
;print, ''
str = STRSPLIT(dummy, ':',/EXTRACT)
np = N_ELEMENTS(str)
new_path=''
FOR ii=0, np-1 DO BEGIN
    this_str = str[ii]
    aa = STRPOS(this_str,'itools')
    bb = STRPOS(this_str,'examples')
    cc = STRPOS(this_str,'share/contrib/arma')
    IF (aa GE 0) OR (bb GE 0) OR (cc GE 0)THEN CONTINUE
    new_path = new_path+this_str+':'
ENDFOR

;print, 'After'
;print, new_path
!PATH=new_path


END
