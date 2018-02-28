PRO TIME_LIST, t0, tf, model_dt, pr_dt, img_dt, NEAREST=nearest, PR_NEAREST=pr_nearest, $ ;in
               TIME=time,NUM_ONLY=num_only                                                ;out
    ;generate a list of times for which model output are desired for a desired figure / metric calculation
    ;input:
    ;   t0, tf:     julian timestamps       initial and final times for which images are desired
    ;   model_dt    in minutes              model timestep
    ;   pr_dt       in minutes              time for pr accumulations
    ;   img_dt      in minutes              frequency at which images are desired
    ;
    ;When set, the keyword nearest will insure that time t and mdt are integer number of model timesteps
    ;
    ;output:
    ;four time are generated:
    ;   1 time of figure
    ;   2 time of previous timestep
    ;   3 time for pr accumulations
    ;
    ;output is a structure with all times
    ;all times are outputted as:
    ;   1 a julian date      
    ;   2 a CMC timestamp string 
    ;   3 a string for display purposes

    ;number of times 
    num_dt = ((tf -t0)*1440.)/img_dt + 1
    IF ARG_PRESENT(num_only) THEN BEGIN
        num_only=num_dt
        RETURN
    ENDIF

    ;output structure
    time={nt:0L,                                                                                       $
          t_jul:DBLARR(num_dt),mdt_jul:DBLARR(num_dt),pr_jul:DBLARR(num_dt),pr_mdt_jul:DBLARR(num_dt), $
          t_cmc:STRARR(num_dt),mdt_cmc:STRARR(num_dt),pr_cmc:STRARR(num_dt),pr_mdt_cmc:STRARR(num_dt), $
          t_str:STRARR(num_dt),mdt_str:STRARR(num_dt),pr_str:STRARR(num_dt),pr_mdt_str:STRARR(num_dt), $
          img_str:STRARR(num_dt)}
    time.nt = num_dt
    
    ;generate all times
    FOR nn=0L, num_dt-1 DO BEGIN

        img_time = t0 + nn*img_dt/1440D

        ;by default, we want data at img_time
        output_time = img_time
        pr_time     = img_time

        ;if desired insure that output time is an integer number of model timesteps
        IF KEYWORD_SET(nearest) THEN BEGIN
            CALDAT, output_time, month,day,year,hour,minute,second
            count=1L
            WHILE (minute MOD model_dt) NE 0 DO BEGIN
                output_time = img_time - count*1D/1440D
                CALDAT, output_time, month,day,year,hour,minute,second
                count ++
                IF count GT 10 THEN MESSAGE, 'Rewinded 10 minutes and still did not find, something is going wrong'
            ENDWHILE
            ;ps time is equal to p0 time
            pr_time = output_time
        ENDIF

        ;if desired insure that only PR time is an integer number of pr_dt
        IF KEYWORD_SET(pr_nearest) THEN BEGIN
            CALDAT, pr_time, month,day,year,hour,minute,second
            count=1L
            WHILE (minute MOD pr_dt) NE 0 DO BEGIN
                pr_time = img_time - count*1D/1440D
                CALDAT, pr_time, month,day,year,hour,minute,second
                count ++
                IF count GT 10 THEN MESSAGE, 'Rewinded 10 minutes and still did not find, something is going wrong'
            ENDWHILE
        ENDIF

        ;image time
        ;-------------
        ;string 
        CALDAT, img_time, month, day,year,hour,minute,second
        time.img_str[nn]=STRING(year,month,day,hour,minute,second,FORMAT='(i4,"_",i02,"_",i02,"__",i02,"_",i02,"_",i02)')
        ;model timestep
        ;-------------
        this_time = output_time
        ;julian
        time.t_jul[nn]=this_time
        ;string 
        CALDAT, this_time, month, day,year,hour,minute,second
        time.t_str[nn]=STRING(year,month,day,hour,minute,second,FORMAT='(i4,"_",i02,"_",i02,"__",i02,"_",i02,"_",i02)')
        ;cmc
        ymdhms=STRING(year,month,day,hour,minute,0,FORMAT='(i4,6i02)')
        SPAWN, ['r.date','-S',ymdhms], cmc_stamp, /NOSHELL
        time.t_cmc[nn]=cmc_stamp
    
        ;previous model timestep 
        ;-------------
        this_time = output_time - model_dt/1440D
        ;julian
        time.mdt_jul[nn]=this_time
        ;string 
        CALDAT, this_time, month, day,year,hour,minute,second
        time.mdt_str[nn]=STRING(year,month,day,hour,minute,second,FORMAT='(i4,"_",i02,"_",i02,"__",i02,"_",i02,"_",i02)')
        ;cmc
        ymdhms=STRING(year,month,day,hour,minute,0,FORMAT='(i4,6i02)')
        SPAWN, ['r.date','-S',ymdhms], cmc_stamp, /NOSHELL
        time.mdt_cmc[nn]=cmc_stamp

        ;time for end of pr accumulation
        ;-------------
        this_time = pr_time 
        ;julian
        time.pr_jul[nn]=this_time
        ;string 
        CALDAT, this_time, month, day,year,hour,minute,second
        time.pr_str[nn]=STRING(year,month,day,hour,minute,second,FORMAT='(i4,"_",i02,"_",i02,"__",i02,"_",i02,"_",i02)')
        ;cmc
        ymdhms=STRING(year,month,day,hour,minute,0,FORMAT='(i4,6i02)')
        SPAWN, ['r.date','-S',ymdhms], cmc_stamp, /NOSHELL
        time.pr_cmc[nn]=cmc_stamp

        ;time for beginning of pr accumulation
        ;-------------
        this_time = pr_time - pr_dt/1440D
        ;julian
        time.pr_mdt_jul[nn]=this_time
        ;string 
        CALDAT, this_time, month, day,year,hour,minute,second
        time.pr_mdt_str[nn]=STRING(year,month,day,hour,minute,second,FORMAT='(i4,"_",i02,"_",i02,"__",i02,"_",i02,"_",i02)')
        ;cmc
        ymdhms=STRING(year,month,day,hour,minute,0,FORMAT='(i4,6i02)')
        SPAWN, ['r.date','-S',ymdhms], cmc_stamp, /NOSHELL
        time.pr_mdt_cmc[nn]=cmc_stamp

    ENDFOR




END
