PRO JUL_TO_CMC, jul_time, cmc_time
;convertion from julian to cmc time

CALDAT,jul_time, month, day, year, hour, minute, seconds
nd = N_ELEMENTS(jul_time)
cmc_time = LONARR(nd)
FOR ii=0, nd-1 DO BEGIN
    ymdhms=STRING(year[ii],month[ii],day[ii],hour[ii],minute[ii],seconds[ii],FORMAT='(i4,6i02)')
    SPAWN, ['r.date','-S',ymdhms], dummy, /NOSHELL
    cmc_time[ii] = dummy
ENDFOR

END
