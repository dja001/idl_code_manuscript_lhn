pro interval, min, max, delta, n_bin=n_bin, val_arr=val_arr, bound_arr=bound_arr, mm=m, bb=b
;make interval for counting bins and outputs the formula for mapping indexes
;index = floor(m*val + b)
; in the interval [min, max[
;
;   there may be a need to limit bounds of indexes thyus generated
    ;;limit upper and lower bounds of hindex 
    ;bb = WHERE(index LT 0, nbb)
    ;IF nbb NE 0 THEN hindex[bb] = 0
    ;bb = WHERE(hindex GE max_num, nbb)
    ;IF nbb NE 0 THEN hindex[bb] = max_num-1

;how it works:

;indexes:
;     0     1     2
;  |-----|-----|-----|
; min               max
;   -----
;   delta
;
;bound_arr
;  v1    v2    v3    v4
;
;val_arr
;     v1    v2   v3

;check that min, max and delts are consistent
n_float = abs((float(max) - float(min)))/float(delta)
if n_float ne floor(n_float) then begin
    message, '(max - min)/delta must be an integer'
endif
n_bin = long(n_float)

if delta lt 0. then message, 'delta should be greater than zero'

;make bound array
bound_arr = findgen(n_bin+1)/(n_bin)*(max-min)+min

;make _val_arr
if max gt min then begin
    ;from smallest to largest
    val_arr = findgen(n_bin)/(n_bin-1)*((max-min)-delta)+min+delta/2.
endif else begin
    ;from largest to smallest
    val_arr = findgen(n_bin)/(n_bin-1)*((max-min)+delta)+min-delta/2.
endelse

;determine m and b
x1 = float(min)
x2 = float(max)
y1 = float(0)
y2 = float(n_bin)
m = (y2 -y1)/(x2 - x1)
b = (x2*y1 - x1*y2)/(x2-x1)






end
