PRO MATCH_TIME, desired_cmc, data_dir, var, $
    files, AVAIL_TIME_ARR=avail_time_arr, AVAIL_FILES=avail_files, REUSE=reuse, nearest=nearest, EXT=ext

if n_elements(nearest) eq 0 then nearest = 0

if n_elements(ext) eq 0 then ext='/*' ;file extension is an optional parameter by default all files are used as input


;function that returns the filenames of GEM output containing variable 'var' at the 'desired_time'
;desired time is an array of cmc_timestamp where filenames are desired

;desired number of times
nd = N_ELEMENTS(desired_cmc)
;define desired interval [t0,tf] to accelerate search
t0 = MIN(desired_cmc, MAX=tf)

;default value for reuse
IF n_elements(reuse) eq 0 then reuse=0

;define source vectors if not already defined
IF reuse eq 0  THEN BEGIN
    ;print, 'Making file time and file list arrays, '

    ;test source dir
    IF ~FILE_TEST(data_dir, /DIRECTORY) THEN BEGIN
        MESSAGE, 'Directory: '+data_dir+'  does not exist. All files set to not_avail.', /INFORMATIONAL
        files = REPLICATE('not_avail',nd)
        avail_time_arr = REPLICATE(-9990., 10)
        RETURN
    ENDIF 

    ;step one, make a list of all the time where a given variable is available
    file_list = FILE_SEARCH(data_dir+ext, COUNT=nf)
    IF nf EQ 0 THEN MESSAGE, 'Directory: '+data_dir+ext+'  is empty'
    ;initialize output arrays
    count = 0L
    avail_time_arr = LONARR(10000)
    avail_files    = STRARR(10000)
    ;for each file in directory
    FOR ii=0, nf-1 DO BEGIN
        this_file = file_list[ii]
        basename = FILE_BASENAME(this_file)
        deb = strpos(basename,'20')
        if deb eq -1 then message, 'Please insure run is in the 2000s'
        yyyy = STRMID(basename, deb+0,4)
        mo   = STRMID(basename, deb+4,2)
        dd   = STRMID(basename, deb+6,2)
        hh   = STRMID(basename, deb+8,2)
        first = STRMID(basename,deb+11,1)
        if first eq '-' then begin
            ;-ve extension
            ext  = STRMID(basename,deb+11,4)
        endif else begin
            ;+ve extension
            ext  = STRMID(basename,deb+11,3)
        endelse
        ext_min1 = STRING(LONG(ext)-1,FORMAT='(i04)')

        SPAWN, 'r.date -S '+yyyy+mo+dd+hh+' +'+ext_min1, cmc_min
        SPAWN, 'r.date -S '+yyyy+mo+dd+hh+' +'+ext,      cmc_max

        ;extract extension of file
        len = STRLEN(basename)
        ext = STRMID(basename,len-4,4)

        ;IF (cmc_max GE t0) AND (cmc_min LE tf) AND (ext NE '_000') THEN BEGIN
        ;IF (cmc_max GE t0) AND (cmc_min LE tf) THEN BEGIN
            unit=102
            if ~file_test(this_file, /regular) then message, 'Expecting a regular standard file. " '+this_file
            file_id=FSTOUV(UNIT=unit,FILE=this_file)
            print, this_file
                key=FSTINF(U=unit,NOMVAR=var)
                WHILE key GT 0 DO BEGIN
                    ;get time 
                    prm = FSTPRM(R=key)
                    cmc_date_str=STRING(prm.date,FORMAT='(i9)')
                    elasped_s = STRING(prm.npas*prm.deet ,FORMAT='(i010)')+'S'
                    cmd = 'r.date -S '+cmc_date_str+' +'+elasped_s
                    SPAWN, cmd, date_str
                    avail_time_arr[count] = date_str
                    avail_files[count]    = file_list[ii]

                    count ++ 
    
                    ;next occurence of same variable
                    key=FSTINF(U=unit,/SUI)
                ENDWHILE
            stat = FSTFRM(U=unit)
        ;ENDIF 
    ENDFOR
    
    ;trim arrays
    avail_time_arr = avail_time_arr[0:count-1]
    avail_files    = avail_files[0:count-1]

    ;sort arrays
    aa = SORT(avail_time_arr)
    avail_time_arr = avail_time_arr[aa]
    avail_files    = avail_files[aa]
ENDIF ELSE BEGIN
    ;print, 'Received file time and file list arrays, '
ENDELSE

;number of available times
n_avail = N_ELEMENTS(avail_time_arr)

;index of file nearest to desired
aa = VALUE_LOCATE(avail_time_arr, desired_cmc)
;match 
files = STRARR(nd)
FOR ii=0, nd-1 DO BEGIN
    CASE aa[ii] OF
        -1:      files[ii] = 'not_avail'
        n_avail: files[ii] = 'not_avail'
        ELSE:  BEGIN
                   ;take file only if time matches exactly
                   IF (avail_time_arr[aa[ii]] EQ desired_cmc[ii]) or (nearest eq 1)  THEN BEGIN
                       ;where may me more than one file, take the latest
                       IF aa[ii]-1 GE 0 THEN BEGIN
                           IF avail_time_arr[aa[ii]-1] EQ desired_cmc[ii] THEN BEGIN
                               ;the time before also matces the desired time
                               ind1 = aa[ii]
                               bf1  = FILE_BASENAME(avail_files[ind1])
                               len1 = STRLEN(bf1)
                               ext1 = FIX(STRMID(bf1,len1-3,3))  ;extension of the file aa[ii]
                               ind2 = aa[ii]-1
                               bf2  = FILE_BASENAME(avail_files[ind2])
                               len2 = STRLEN(bf2)
                               ext2 = FIX(STRMID(bf2,len2-3,3))  ;extension of the file aa[ii] - 1
                               IF var EQ 'PR' THEN BEGIN
                                   ;for PR, time 0 of a forecast is meaningless take forecast whose extension is the largest
                                   IF ext1 GT ext2 THEN BEGIN
                                       files[ii] = avail_files[ind1]
                                   ENDIF ELSE BEGIN
                                       files[ii] = avail_files[ind2]
                                   ENDELSE
                               ENDIF ELSE BEGIN
                                   ;for p0, QD, WT1,... we want the _000 file, we thus take the one with the smallest extension
                                   IF ext1 LT ext2 THEN BEGIN
                                       files[ii] = avail_files[ind1]
                                   ENDIF ELSE BEGIN
                                       files[ii] = avail_files[ind2]
                                   ENDELSE
                               ENDELSE
                           ENDIF ELSE BEGIN
                               files[ii] = avail_files[aa[ii]]        
                           ENDELSE
                       ENDIF ELSE BEGIN
                           files[ii] = avail_files[aa[ii]]        
                       ENDELSE
                   ENDIF ELSE BEGIN
                       files[ii] = 'not_avail'
                   ENDELSE
               END
    ENDCASE
    ;;debuging
    ;print, 'want', desired_cmc[ii], aa[ii]
    ;cmd = 'r.date -V '+STRING(desired_cmc[ii], FORMAT='(i10)')
    ;SPAWN, cmd, date_str
    ;print, date_str, files[ii]
    ;print, ''
ENDFOR

END
