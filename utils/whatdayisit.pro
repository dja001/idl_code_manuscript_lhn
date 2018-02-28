FUNCTION WhatDayIsIt, juliandate

   ; Need a date? Duh...
   
IF N_Elements(juliandate) EQ 0 THEN $
   juliandate = Systime(/Julian)

   ; Make a table. Use week of March 17th, 2002.
   
daysOfWeek = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', $
              'Thursday', 'Friday', 'Saturday']
datemod = IntArr(7)

FOR j=0,6 DO BEGIN
   jdate = Julday(3, 17 + j, 2002)
   datemod[j] = jdate MOD 7
ENDFOR
 
   ; Convert to day, month, year.

CalDat, juliandate, month, day, year
jdate = Julday(month, day, year)

   ; What day of the week is it? Return it.
   
index = Where(datemod EQ (jdate MOD 7))
RETURN, daysOfWeek[index]
END
