PRO CHECK_N_CPUS, max_cpu, del_ps
;insures that no more than max_cpu CPUS are running the convert task
cmd = 'ps -ef | grep convert | wc -l'
SPAWN, cmd, res
WHILE FIX(res)-1 GE 2.*max_cpu DO BEGIN        ;cmd returns n process + 1 so 1 is substracted to res    
    WAIT, .2                                   ;also SPAWN launches 2 process lines per command so max_cpu is multiplied by 2
    SPAWN, cmd, res
ENDWHILE

END


PRO PS_CLOSE, ps_name, PDF=pdf, GIF=gif, PNG=png, JPG=jpg, $
              DEL_PS=del_ps, $
              DENSITY=density, GEOMETRY=geometry, FONT=font, $
              NUM_CPUS=num_cpus, VERBOSE=verbose

;default number of cpus is 1
IF N_ELEMENTS(num_cpus) EQ 0 THEN num_cpus=1

;finalize already initialized post-script picture
DEVICE, /close
IF KEYWORD_SET(verbose) THEN print, ps_name + '     sucessfully created'

;not so pretty hack to get lmroman fonts on systems where IDL fonts cannot be changed for lack of root access
IF N_ELEMENTS(font) NE 0 THEN BEGIN
    IF KEYWORD_SET(verbose) THEN print, 'change Helvetica font in ps for LMRoman'
    IF font EQ 'lmroman' THEN BEGIN
        cmd = 'sed -i s/Helvetica/LMRoman10\-Regular/ '+ps_name
        SPAWN, cmd
    ENDIF
ENDIF


;command to delete ps once conversion is done
del_cmd = ''
IF KEYWORD_SET(del_ps) THEN BEGIN
    ;error message if no conversion is to be performed
    IF (KEYWORD_SET(pdf) + KEYWORD_SET(gif) + KEYWORD_SET(png) + KEYWORD_SET(jpg)) EQ 0 THEN BEGIN
        message, 'You might want to convert ps file before deleting it', /informational
        del_cmd = 'rm -f '+ ps_name
        SPAWN, del_cmd
        RETURN
    ENDIF
    ;command to delete ps
    del_cmd = ' && rm -f '+ ps_name
ENDIF

;run convert in the background if NUM_CPUS is set
endchar = ''
IF num_cpus GT 1 THEN endchar = ' &'

;pdf conversion
IF KEYWORD_SET(pdf) THEN BEGIN
    pdf_name = STRMID(ps_name,0,STRLEN(ps_name)-3)+'.pdf'
    cmd='ps2pdf -dEPSCrop -dColorConversionStrategy=/LeaveColorUnchanged  -dEncodeColorImages=false  -dEncodeGrayImages=false  -dEncodeMonoImages=false ' $
        +ps_name+' '+pdf_name+del_cmd+endchar

    IF NUM_CPUS GT 1 THEN CHECK_N_CPUS, num_cpus
    SPAWN, cmd
    IF KEYWORD_SET(verbose) THEN print, pdf_name+ '     sucessfully created'
ENDIF

;gif conversion
IF KEYWORD_SET(gif) THEN BEGIN
    IF ~KEYWORD_SET(DENSITY) THEN density = 72
    IF ~KEYWORD_SET(GEOMETRY) THEN geometry = '100%'
    gif_name = STRMID(ps_name, 0, STRLEN(ps_name)-3 )+'.gif'
    cmd = 'convert -density'+' '+STRING(density, FORMAT='(i0)')+' -geometry '+geometry+' '$
          +ps_name+' '+gif_name+del_cmd+endchar
    IF NUM_CPUS GT 1 THEN CHECK_N_CPUS, num_cpus
    SPAWN, cmd
    IF KEYWORD_SET(verbose) THEN print, gif_name+ '     sucessfully created'
ENDIF

;png conversion
IF KEYWORD_SET(png) THEN BEGIN
    IF ~KEYWORD_SET(DENSITY) THEN density = 72
    IF ~KEYWORD_SET(GEOMETRY) THEN geometry = '100%'
    png_name = STRMID(ps_name, 0, STRLEN(ps_name)-3 )+'.png'
    cmd = 'convert -density'+' '+STRING(density, FORMAT='(i0)')+' -geometry '+geometry+' '$
          +ps_name+' '+png_name+del_cmd+endchar
    IF NUM_CPUS GT 1 THEN CHECK_N_CPUS, num_cpus
    SPAWN, cmd
    IF KEYWORD_SET(verbose) THEN print, png_name+ '     sucessfully created'
ENDIF

;jpg conversion
IF KEYWORD_SET(jpg) THEN BEGIN
    IF ~KEYWORD_SET(DENSITY) THEN density = 72
    IF ~KEYWORD_SET(GEOMETRY) THEN geometry = '100%'
    jpg_name = STRMID(ps_name, 0, STRLEN(ps_name)-3 )+'.jpg'
    cmd = 'convert -density'+' '+STRING(density, FORMAT='(i0)')+' -geometry '+geometry+' '$
          +ps_name+' '+jpg_name+del_cmd+endchar
    IF NUM_CPUS GT 1 THEN CHECK_N_CPUS, num_cpus
    SPAWN, cmd
    IF KEYWORD_SET(verbose) THEN print, jpg_name+ '     sucessfully created'
ENDIF




END
