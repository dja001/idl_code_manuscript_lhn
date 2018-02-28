PRO GET_VAR, this_file, nomvar,                               $          ;mandatory input variables
             DATE=cmc_timestamp, INFORMATIONAL=informational, $          ;optional input keywords
             OUTVAR=outvar, IS_THERE=is_there, FIRST_HEADER=first_header, IP1_ARR=ip1_arr ;optional output keywords

;read a given variable in a standard file
;lowest z index will correspond to lowest vertical levels

;defines newline character for error messages
IF (!D.NAME EQ 'WIN') THEN newline = STRING([13B, 10B]) ELSE newline = STRING(10B)

;get nx and ny and nz for this variable
;save sygma levels and keys for every level
unit=102
file_id=FSTOUV(UNIT=unit,FILE=this_file)
    key=FSTINF(U=unit,NOMVAR=nomvar,DATE=cmc_timestamp)
    IF key LT 0 THEN BEGIN
        is_there=0
        stat = FSTFRM(U=unit)
        err_mes = 'Variable: '+nomvar+' not found. -ve key'+newline+$
                  'file: '+this_file
        IF N_ELEMENTS(cmc_timestamp) NE 0 THEN BEGIN
            err_mes = err_mes + newline + 'cmc_timestamp: '+STRING(cmc_timestamp)
        ENDIF ELSE BEGIN
            err_mes = err_mes + newline + 'cmc_timestamp not defined'
        ENDELSE
        MESSAGE, err_mes, INFORMATIONAL=informational
    ENDIF ELSE BEGIN
        is_there=1
        nz = 0  
        ;for every model level
        WHILE key GT 0 DO BEGIN
            ;update level counter
            nz ++
            ;get and save key and sigma level
            prm = FSTPRM(R=key)
            this_level=0.
            stat=CONVIP(IP=prm.ip1,P=this_level,KIND=1,MODE=-1)      ;P=var  var must have been initialized
            ip1 = prm.ip1
            IF nz EQ 1 THEN BEGIN
                ;save nx and ny
                nx  = prm.ni
                ny  = prm.nj
                ip1_arr = ip1
                key_arr = key
                hyb_arr = this_level
            ENDIF ELSE BEGIN
                ip1_arr = [ip1_arr, ip1       ]
                key_arr = [key_arr, key       ]
                hyb_arr = [hyb_arr, this_level]
            ENDELSE
            ;next occurence of same variable
            key=FSTINF(U=unit,/SUI)
        ENDWHILE

        ;order keys so that lower levels are read first  syg level 1 = surface level lower numbers are higher
        sorted = REVERSE(UNIQ(hyb_arr,SORT(hyb_arr)))   ;the uniq keyword avoid possible double entries in standard files
        ip1_arr = ip1_arr[sorted]
        hyb_arr = hyb_arr[sorted]
        key_arr = key_arr[sorted]
        nz = N_ELEMENTS(sorted)

        IF ARG_PRESENT(first_header) THEN BEGIN
            ;return header of first valid entry
            IF nz GT 0 THEN BEGIN
                this_key = key_arr[0]
                first_header=FSTPRM(R=this_key)
            ENDIF
        ENDIF ELSE BEGIN
            ;return (possibly 3D) array of output field
            ;initialize output variable
            outvar = FLTARR(nx,ny,nz)
            ;read data
            FOR kk=0, nz-1 DO BEGIN
                this_key = key_arr[kk]
                ;print, kk, hyb_arr[kk]
                this_data=FSTLUK(R=this_key)
                outvar[*,*,kk]=this_data
            ENDFOR
        ENDELSE

        ;close file
        stat = FSTFRM(U=unit)
    ENDELSE
END


PRO GET_GEM_DATA, origin_filename, $
                  uu=uu, vv=vv, tt=tt, hu=hu, ww=ww, pr=pr, air_pres_pa=pres_pa, me=me, p0=p0, zet=zet, qd=qd, $    ;legacy code, remove in 2017
                  var_name=var_name, values=values, $
                  p_from_var=p_var,  $
                  ip1_arr=ip1_arr, $
                  cmc_timestamp=cmc_timestamp, $
                  p_levels=p_levels,$
                  lat=lat, lon=lon, $
                  getvar=getvar, is_there=is_there, nobot=nobot
;takes in a gem std file and output atmosheric fields on sygma or pressure levels                    

if n_elements(getvar) ne 0 then message, 'The keyword getvar is deprecated, please use var_name instead'

IF ARG_PRESENT(is_there) THEN BEGIN
    ;if this argument is supplied, the program will return sucessfully on error
    informational=1   ;don't stop on error
ENDIF ELSE BEGIN
    informational=0   ;stop on error
ENDELSE

;defines newline character for error messages
IF (!D.NAME EQ 'WIN') THEN newline = STRING([13B, 10B]) ELSE newline = STRING(10B)

;a random number for temporary files
SPAWN, 'echo $RANDOM', rand
rand_str = STRING(rand,FORMAT='(i06)')

;temp dir for this session
tmpdir = GETENV('TMPDIR')

;insure that cmc_timestamp is a long integer
IF N_ELEMENTS(cmc_timestamp) NE 0 THEN BEGIN
    if n_elements(cmc_timestamp) gt 1 then message, 'cmc_timestamp cannot be an array'
    IF ~ISA(cmc_timestamp, 'LONG') THEN BEGIN
        cmc_timestamp = LONG(cmc_timestamp)
    ENDIF
ENDIF

;insure that filename exists
IF ~FILE_TEST(origin_filename, /REGULAR) THEN BEGIN
    err_mes='Origin_filename:'     +newline+ $
            '"'+origin_filename+'"'+newline+ $
            'does not exist, or is not a regular file'
    MESSAGE, err_mes, INFORMATIONAL=informational
    is_there = 0
    RETURN
ENDIF

;interpolate vertically if desired
IF N_ELEMENTS(p_levels) NE 0 THEN BEGIN

    ;list of levels in string form
    nz = N_ELEMENTS(p_levels)
    p_levels_str=STRING(p_levels[0],FORMAT='(f7.2)')
    FOR pp=1, nz-1 DO BEGIN
        p_levels_str = p_levels_str+','+STRING(p_levels[pp],FORMAT='(f7.2)')
    ENDFOR
    p_levels_str = STRCOMPRESS(p_levels_str, /REMOVE_ALL)
    
    ;code for type of interpolation
    ;CUB_    CUBIC   
    ;CUBP_   CUBIC   Clip negative values
    ;LIN_    LINEAR  
    ;NOI_    NO INTERPOLATION    Use for surface or 2D  variables only 
    ;string w/ all desired variables
    var_str= 'CUB_'+var_name

    ;get info on P0 and desired variable
    GET_VAR, origin_filename, 'P0',     DATE=cmc_timestamp, FIRST_HEADER=header_p0,  IS_THERE=is_there_p0
    GET_VAR, origin_filename, var_name, DATE=cmc_timestamp, FIRST_HEADER=header_var, IS_THERE=is_there_var
    ;insure that P0 and desired variable are both available at cmc_time
    IF (is_there_p0 NE 1) OR (is_there_var NE 1) THEN BEGIN
        err_mes = 'Error: either P0 or variable unavailable at desired time' +newline+$
                  'P0  validity: '+STRING(is_there_p0)                       +newline+$
                  'Var is valid: '+STRING(is_there_var)
        MESSAGE, err_mes, INFORMATIONAL=informational
    ENDIF
    ;help, is_there_p0, is_there_var

    ;perform vertical interplation
    directives_file = tmpdir+'/'+rand_str+'directives'
    src_p0_file     = tmpdir+'/'+rand_str+'src_p0_file.fst'
    pxs_file        = tmpdir+'/'+rand_str+'temp_pxs_file.fst'
    interp_file     = tmpdir+'/'+rand_str+'interplated.fst'
    sec_source_file = tmpdir+'/'+rand_str+'sec_source.fst'

    ;If variable and p0 not on same grid, interpolate p0 to variable grid
    IF (header_p0.ig1 EQ header_var.ig1) AND (header_p0.ig2 EQ header_var.ig2) THEN BEGIN
        ;P0 and var are on the same grid, get P0 in the pxs file
        ;write directives file
        OPENW, lun, directives_file, /GET_LUN
        PRINTF, lun, " desire(-1,['P0','>>','^^','^>','!!'])"
        FREE_LUN, lun
            ;code to deal with multiple P0 in a file. Should normally be commented----------------------
            OPENW, lun, directives_file, /GET_LUN
                PRINTF, lun, " desire(-1,['>>','^^','^>','!!'])"
                PRINTF, lun, " desire(-1,['P0'],'E2AVGTRPALL')"
            FREE_LUN, lun
            ;-------------------------------------------------------------------------------------------
        ;generate pxs_file
        cmd = 'editfst -s '+origin_filename + $
                     ' -d '+pxs_file        + $
                     ' -i '+directives_file
        SPAWN, cmd, output, errstat
        print, output
            ;make a second source file with desired variable and P0 Should normally be commented--------
            SPAWN, 'rm -f '+directives_file      
            OPENW, lun, directives_file, /GET_LUN
                PRINTF, lun, " desire(-1,['>>','^^','^>','!!'])"
                PRINTF, lun, " desire(-1,['P0'],'E2AVGTRPALL')"
                PRINTF, lun, " desire(-1,['"+var_name+"'],'E2AVGTRPALL')"
            FREE_LUN, lun
            cmd = 'editfst -s '+origin_filename + $
                         ' -d '+sec_source_file + $
                         ' -i '+directives_file
            SPAWN, cmd, output, errstat
            print, output
            origin_filename = sec_source_file
            ;-------------------------------------------------------------------------------------------

        ;if process was not sucessful stop
        IF errstat NE 0 THEN BEGIN
            err_mes = 'Something went wrong with editfst' +newline+$
                      'CMD: '+cmd
            MESSAGE, err_mes, INFORMATIONAL=informational
        ENDIF
        ;remove useless tmp files
        SPAWN, 'rm -f '+directives_file      
    ENDIF ELSE BEGIN
        ;P0 and var are NOT on the same grid, interpolate P0 to the variable grid
        ;copy p0 at desired time
        ;write directives file
        OPENW,  lun, directives_file, /GET_LUN
        PRINTF, lun, " desire(-1,['>>','^^','^>','!!'])"
        PRINTF, lun, " desire(-1,['P0'],-1,"+STRING(cmc_timestamp,FORMAT='(i012)')+")"
        FREE_LUN, lun
        ;copy necessary info in intermediate file
        cmd='editfst -s '+origin_filename      + $
                        ' -d '+src_p0_file     + $
                        ' -i '+directives_file
        SPAWN, cmd, status, errstat
        ;if process was not sucessful stop
        IF errstat NE 0 THEN BEGIN
            err_mes = 'Something went wrong with editfst' +newline+$
                      'CMD: '+cmd
            MESSAGE, err_mes, INFORMATIONAL=informational
        ENDIF
        ;remove tmp files
        SPAWN, 'rm -f '+directives_file      
        ;write pgsm directives file
        OPENW,  lun, directives_file, /GET_LUN
        PRINTF, lun, " sortie(STD,4000,A)"
        PRINTF, lun, " grille(TAPE2,"+STRING(header_var.ig1,header_var.ig2,header_var.ig3,FORMAT='(i5,",",i5,",",i1)')+")"
        PRINTF, lun, " setintx(LINEAIR)"
        PRINTF, lun, " heure(TOUT)"
        PRINTF, lun, " champ('P0')"
        FREE_LUN, lun
        cmd = 'pgsm    -iment '+src_p0_file     + $ 
              '        -ozsrt '+pxs_file        + $
              '        -i     '+directives_file
        SPAWN, cmd, status, errstat
        ;if process was not sucessful stop
        IF errstat NE 0 THEN BEGIN
            err_mes = 'Something went wrong with PGSM' +newline+$
                      'CMD: '+cmd
            MESSAGE, err_mes, INFORMATIONAL=informational
        ENDIF
        ;remove tmp files
        SPAWN, 'rm -f '+directives_file      
        SPAWN, 'rm -f '+src_p0_file      
    ENDELSE

    ;Do the interpolation
    cmd= 'd.pxs2pxt -s '    + origin_filename                        + $
                  ' -datev '+ STRING(cmc_timestamp,FORMAT='(i012)')  + $
                  ' -d '    + interp_file                            + $
                  ' -pxs '  + pxs_file                               + $
                  ' -plevs '+ p_levels_str                           + $
                  ' -var '  + var_str
    print, '-----------IDL cmd------------'
    print, cmd
    SPAWN, cmd, status, errstat
    print, status
    print, '------------------------------'
    print, ''
    print, ''
    print, ''
    ;if process was not sucessful stop
    ;IF LONG(errstat) NE 0 THEN BEGIN
    ;    err_mes = 'Something went wrong with pxs2pxt' +newline+$
    ;              'CMD: '+cmd
    ;    MESSAGE, err_mes, INFORMATIONAL=informational
    ;ENDIF
    ;remove tmp files
    SPAWN, 'rm -f '+pxs_file      
    SPAWN, 'rm -f '+sec_source_file

    ;read file is now file containing interpolated data
    read_file = interp_file
ENDIF ELSE BEGIN

    ;file to be read is original standard file
    read_file = origin_filename

ENDELSE

;read fst file and fill in output variables
IF N_ELEMENTS(var_name) NE 0 THEN BEGIN
    print, var_name
    GET_VAR, read_file, var_name, DATE=cmc_timestamp, OUTVAR=values, IS_THERE=is_there, INFORMATIONAL=informational, IP1_arr=ip1_arr
    ;CASE var_name OF
    ;    'RT': values *= 6e4     ;conversion from m/s to to mm / min (*1000 *60)
    ;    ELSE:a=0
    ;ENDCASE
ENDIF


;----------------------------------------
;keep for compatibility with older code
    ;to remove in 2017
    IF ARG_PRESENT(uu) THEN BEGIN
        GET_VAR, read_file, 'UU', DATE=cmc_timestamp, OUTVAR=uu, IS_THERE=is_there, INFORMATIONAL=informational
        ;convert knots to meters per seconds
        ;1knot = 0.514 444 m/s
        uu *= 0.514444
    ENDIF
    IF ARG_PRESENT(vv) THEN BEGIN
        GET_VAR, read_file, 'VV', DATE=cmc_timestamp, OUTVAR=vv, IS_THERE=is_there, INFORMATIONAL=informational
        ;convert knots to meters per seconds
        ;1knot = 0.514 444 m/s
        vv *= 0.514444
    ENDIF
    IF ARG_PRESENT(tt)    THEN GET_VAR, read_file, 'TT',  DATE=cmc_timestamp, OUTVAR=tt,  IS_THERE=is_there, INFORMATIONAL=informational
    IF ARG_PRESENT(hu)    THEN GET_VAR, read_file, 'HU',  DATE=cmc_timestamp, OUTVAR=hu,  IS_THERE=is_there, INFORMATIONAL=informational
    IF ARG_PRESENT(me)    THEN GET_VAR, read_file, 'ME',  DATE=cmc_timestamp, OUTVAR=me,  IS_THERE=is_there, INFORMATIONAL=informational
    IF ARG_PRESENT(p0)    THEN GET_VAR, read_file, 'P0',  DATE=cmc_timestamp, OUTVAR=p0,  IS_THERE=is_there, INFORMATIONAL=informational
    IF ARG_PRESENT(ww)    THEN GET_VAR, read_file, 'WT1', DATE=cmc_timestamp, OUTVAR=ww,  IS_THERE=is_there, INFORMATIONAL=informational
    IF ARG_PRESENT(zet)   THEN GET_VAR, read_file, 'ZET', DATE=cmc_timestamp, OUTVAR=zet, IS_THERE=is_there, INFORMATIONAL=informational
    IF ARG_PRESENT(qd) THEN BEGIN  
        GET_VAR, read_file, 'QD', DATE=cmc_timestamp, OUTVAR=qd, IS_THERE=is_there, INFORMATIONAL=informational
        ;convert kg/kg to g/kg
        qd *= 1e3
    ENDIF
    
    
    ;PR is different
    IF ARG_PRESENT(pr) THEN BEGIN
        unit=102
        file_id=FSTOUV(UNIT=unit,FILE=read_file)
    
        varname = 'PR'
        ;There is no way to directly specify the desired date in minutes so I look for a match with each date present
        mod_dt = 1.       ;model time step in minutes
        key=FSTINF(U=unit,NOMVAR=varname)
        IF KEYWORD_SET(cmc_timestamp) THEN BEGIN
            WHILE key GE 0 DO BEGIN
                ;get parameters for this entry
                prm = FSTPRM(R=key)
                ;convert to real time
                SPAWN, 'r.date '+STRING(prm.date,FORMAT='(i9)'), date_deb
                ;add minutes to get valid time
                minutes=LONG(prm.npas*prm.deet)
                SPAWN, 'r.date '+date_deb+' +'+STRING(minutes,FORMAT='(i012)')+'S',date_valid
                ;print, date_valid
                ;cmc timestamp of valid date
                SPAWN, 'r.date '+date_valid, date_valid_cmc
                ; if desired time stop here   we have the good key
                IF LONG(date_valid_cmc) EQ cmc_timestamp THEN BREAK
                ;update key to next entry in fst file
                key=FSTINF(U=unit,NOMVAR=varname, /SUI)
            ENDWHILE
        ENDIF ELSE BEGIN
            ;get parameters for this entry
            prm = FSTPRM(R=key)
        ENDELSE
        IF key LT 0 THEN BEGIN
            stat = FSTFRM(U=unit)
            SPAWN,'r.date '+STRING(cmc_timestamp),date_real          
            err_mes= 'Problem getting PR'                                    +newline+$
                     'PR is not there or not there at the desired timestanp' +newline+$
                     'file: '+read_file                                      +newline+$ 
                     'CMC_TIMESTAMP '+STRING(cmc_timestamp)                  +newline+$ 
                     'Which is:  '+STRING(date_real)
            MESSAGE, err_mes, INFORMATIONAL=informational
        ENDIF
    
        ;initialize variable
        nx = prm.ni
        ny = prm.nj
        pr = FLTARR(nx,ny)
        ;read data
        this_data=FSTLUK(R=key)
        ;fill output array
        pr[*,*]=this_data
    
        ;convert meters to mm
        pr *= 1000.
    
        stat = FSTFRM(U=unit)
    ENDIF
    
;----------------------------------------------------------

IF ARG_PRESENT(p_var) THEN BEGIN
    ;temporary file containing the pressure 
    pres_file       = tmpdir+'/'+rand_str+'pressure.fst'
    directives_file = tmpdir+'/'+rand_str+'directives'
    temp_file       = tmpdir+'/'+rand_str+'temp_file.fst'
    IF N_ELEMENTS(cmc_timestamp) EQ 0 THEN MESSAGE, 'cmc_timestamp is required when requesting pressure'
    ;write directives file
    OPENW, lun, directives_file, /GET_LUN
    PRINTF, lun, " desire(-1,['P0','"+var_name+"'],-1,"+STRING(cmc_timestamp,FORMAT='(i09)')+')'
    PRINTF, lun, " desire(-1,['>>','^^','^>','!!'])"
    FREE_LUN, lun
    ;copy p0 at desired time in a temporary file
    cmd = 'editfst -s '+read_file+' -d '+temp_file+' -i '+directives_file
    SPAWN, cmd, status, errstat
    print, status
    ;if process was not sucessful stop
    IF errstat NE 0 THEN BEGIN
        err_mes = 'Something went wrong with editfst' +newline+$
                  'CMD: '+cmd
        MESSAGE, err_mes, INFORMATIONAL=informational
    ENDIF
    ;write pressure in file
    cmd= 'd.compute_pressure -s '+temp_file+' -d '+pres_file+' -var '+var_name
    SPAWN, cmd, status, errstat
    print, status
    ;if process was not sucessful stop
    IF errstat NE 0 THEN BEGIN
        err_mes = 'Something went wrong with compute pressure' +newline+$
                  'CMD: '+cmd
        MESSAGE, err_mes, INFORMATIONAL=informational
    ENDIF

    ;read in data from temp file
    GET_VAR, pres_file, 'PX', DATE=cmc_timestamp, OUTVAR=p_var, IS_THERE=is_there_px, INFORMATIONAL=1
    IF is_there_px NE 1 THEN BEGIN
        err_mes= '"PX" variable not there for variable: '+var_name
        MESSAGE, err_mes, INFORMATIONAL=informational
    ENDIF

    ;remove temporary file
    SPAWN, 'rm -f '+pres_file      
    SPAWN, 'rm -f '+directives_file
    SPAWN, 'rm -f '+temp_file      
ENDIF


;get lat lon if desired
IF ARG_PRESENT(lat) OR ARG_PRESENT(lat) THEN BEGIN
    ;default variable to get lat/lon from
    IF N_ELEMENTS(var_name) EQ 0 THEN var_name='UU'
    ;get the lat/lon from gdll function
    CALL_GDLL, origin_filename, var_name, lat, lon
    ;transform lon form [0, 360] to [-180, 180] because idl likes it better
    aa = WHERE(lon GE 180., count)
    IF count NE 0 THEN lon[aa] = lon[aa] - 360.
ENDIF

;remove interpolated data if necessary
IF N_ELEMENTS(p_levels) NE 0 THEN BEGIN
    SPAWN, 'rm -f '+interp_file      
ENDIF

;remove bottom level if desired
if keyword_set(nobot) then begin
    sz = size(values,/dim)
    values = values[*,*,1:sz[2]-1]
    ip1_arr = ip1_arr[1:sz[2]-1]
    if n_elements(p_var) ne 0 then p_var = p_var[*,*,1:sz[2]-1]
endif

END
