FUNCTION min_f_golden, FNAME=fname, BOUNDS=bounds, TOL=tol, OPT_VAR_1=opt_var_1, OPT_VAR_2=opt_var_2, OPT_VAR_3=opt_var_3

;   finds the minimum of a function of one variable using golden section search
;   for faster convergence, couls be replaced by BRENT's algorithm which is more complicated...
;
;   [xmin, fmin] = golden(f, ax, bx, cx, tol) computes a local minimum
;   of f. xmin is the computed local minimizer of f and fmin is
;   f(xmin). xmin is computed to an relative accuracy of TOL.
;	tol defaults to 0.001 if no input provided
;
;   The parameters ax, bx and cx must satisfy the following conditions:
;   ax < bx < cx, f(bx) < f(ax) and f(bx) < f(cx).
;
;   xmin satisfies ax < xmin < cx. golden is guaranteed to succeed if f
;   is continuous between ax and cx
;
;   Roman Geus, ETH Zuerich, 9.12.97
;	IDL translation by Dominik Jacques 2009
;
;opt vars are optionnal variables to be passed to the function
  
C = (3.-SQRT(5.))/2.
R = 1.-C

;test inputa variables
IF Size(fname, /Type) NE 7 THEN BEGIN
	Print, 'String argument required as function name'
	RETURN, -1
ENDIF 
IF bounds[1] LE bounds[0] THEN BEGIN
	print, "bounds must be [small, big]!!!"
	RETURN, -1
ENDIF
IF N_ELEMENTS(tol) EQ 0 THEN tol = 0.001

ax = bounds[0]
bx = C*(bounds[1] - bounds[0])
cx = bounds[1]
 
x0 = ax
x3 = cx
IF ABS(cx-bx) GT ABS(bx-ax) THEN BEGIN
	x1 = bx
	x2 = bx + C*(cx-bx)
ENDIF ELSE BEGIN
	x2 = bx
	x1 = bx - C*(bx-ax)
ENDELSE

IF N_ELEMENTS(opt_var_1) NE 0 THEN BEGIN
    IF N_ELEMENTS(opt_var_2) NE 0 THEN BEGIN
        IF N_ELEMENTS(opt_var_3) NE 0 THEN BEGIN
            f1 = CALL_FUNCTION(fname,x1, opt_var_1, opt_var_2, opt_var_3)
            f2 = CALL_FUNCTION(fname,x2, opt_var_1, opt_var_2, opt_var_3)
        ENDIF ELSE BEGIN
            f1 = CALL_FUNCTION(fname,x1, opt_var_1, opt_var_2)
            f2 = CALL_FUNCTION(fname,x2, opt_var_1, opt_var_2)
        ENDELSE
    ENDIF ELSE BEGIN
        f1 = CALL_FUNCTION(fname,x1, opt_var_1)
        f2 = CALL_FUNCTION(fname,x2, opt_var_1)
    ENDELSE
ENDIF ELSE BEGIN
    f1 = CALL_FUNCTION(fname,x1)
    f2 = CALL_FUNCTION(fname,x2)
ENDELSE
 
k = 1
WHILE ABS(x3-x0) GT tol*(ABS(x1)+ABS(x2)) DO BEGIN
    ;print output to debug convergence
	;PRINT, k, ABS(x3-x0)
	IF f2 LT f1 THEN BEGIN
		x0 = x1
		x1 = x2
		x2 = R*x1 + C*x3   ; x2 = x1+c*(x3-x1)
		f1 = f2
        IF N_ELEMENTS(opt_var_1) NE 0 THEN BEGIN
            IF N_ELEMENTS(opt_var_2) NE 0 THEN BEGIN
                IF N_ELEMENTS(opt_var_3) NE 0 THEN BEGIN
                    f2 = CALL_FUNCTION(fname,x2, opt_var_1, opt_var_2, opt_var_3)
                ENDIF ELSE BEGIN
                    f2 = CALL_FUNCTION(fname,x2, opt_var_1, opt_var_2)
                ENDELSE
            ENDIF ELSE BEGIN
                f2 = CALL_FUNCTION(fname,x2, opt_var_1)
            ENDELSE
        ENDIF ELSE BEGIN
            f2 = CALL_FUNCTION(fname,x2)
        ENDELSE
	ENDIF ELSE BEGIN
		x3 = x2
		x2 = x1
		x1 = R*x2 + C*x0   ; x1 = x2+c*(x0-x2)
		f2 = f1
        IF N_ELEMENTS(opt_var_1) NE 0 THEN BEGIN
            IF N_ELEMENTS(opt_var_2) NE 0 THEN BEGIN
                IF N_ELEMENTS(opt_var_3) NE 0 THEN BEGIN
                    f1 = CALL_FUNCTION(fname,x1, opt_var_1, opt_var_2, opt_var_3)
                ENDIF ELSE BEGIN
                    f1 = CALL_FUNCTION(fname,x1, opt_var_1, opt_var_2)
                ENDELSE
            ENDIF ELSE BEGIN
                f1 = CALL_FUNCTION(fname,x1, opt_var_1)
            ENDELSE
        ENDIF ELSE BEGIN
            f1 = CALL_FUNCTION(fname,x1)
        ENDELSE
	ENDELSE
	k = k+1
END
 
IF f1 LT f2 THEN BEGIN
	xmin = x1
	fmin = f1
ENDIF ELSE BEGIN
	xmin = x2
	fmin = f2
ENDELSE

RETURN, [xmin, fmin]


END
