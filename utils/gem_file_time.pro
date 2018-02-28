PRO GEM_FILE_TIME, fcst_t0, this_time, output_freq_h, dir,   $
               filename, cmc_time, NOT_FOUND=not_found
    ;determine the filename of GEM output at a given desired time 
    
    ;string at begining of all fst files
    CALDAT, fcst_t0, t0_month, t0_day, t0_year, t0_hour, t0_minute, t0_second
    print, t0_year, t0_month, t0_day, t0_hour, t0_minute, t0_second
    t0_ymdh = STRING(t0_year,t0_month,t0_day,t0_hour,FORMAT='(i4,3i02)')

    ;desired time
    CALDAT, this_time, month, day, year, hour, minute, second        
    print, year, month, day, hour, minute, second        
    ymdhm=STRING([year,month,day,hour,minute],FORMAT='(i04,4i02)')

    ;cmc timestamp for this hour
    SPAWN, ['r.date',ymdhm], cmc_time, /NOSHELL

    IF this_time GT fcst_t0 THEN BEGIN
        ;determine forecast hour 001, 002, .... 
        ;difference in hour between this time and fcst_t0
        IF minute EQ 0 THEN BEGIN
            ;is this the initial time?
            IF  minute EQ t0_minute AND $
                hour   EQ t0_hour   AND $
                day    EQ t0_day    AND $
                month  EQ t0_month  AND $
                year   EQ t0_year THEN BEGIN
                nearest_hour = JULDAY(month, day, year, hour, 0, 0)               ;12:00 forecast will be in 12_001 file
                print, 'aaaaaaaaaaaaaaaaaaaaaaaaaa'
            ENDIF ELSE BEGIN
                nearest_hour = JULDAY(month, day, year, hour, 0, 0)               ;13:00 forecast will be in 12_001 file,  14:00 forecast will be in 13_001 file, etc
            ENDELSE    
        ENDIF ELSE BEGIN
            nearest_hour =     JULDAY(month, day, year, hour+1, 0, 0)             ;date of nearest integer hour after time,13:10 forecast will be in 13_001 file 
        ENDELSE
        fcst_dt = (nearest_hour - fcst_t0) * 24.
        fcst_dt_str = STRING(fcst_dt,FORMAT='(i03)')

        filename=dir+t0_ymdh+'_'+fcst_dt_str
    ENDIF ELSE BEGIN
        ;requested time is before start time of forecast
        filename='dummy'
    ENDELSE

    not_found=0
    IF ~FILE_TEST(filename) THEN BEGIN
        print, 'file:'
        print, filename
        print, 'does not exist'
        not_found=1
    ENDIF
END



