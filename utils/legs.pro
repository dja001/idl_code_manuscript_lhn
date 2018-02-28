;main program line ~750


FUNCTION LIN, in, val_low, val_high, col_low, col_high
    ;does a linear mapping of "in" values to "out" values given specified ranges

    x1 = FLOAT(val_low)
    x2 = FLOAT(val_high)

    out = BYTARR(N_ELEMENTS(in),3)
    FOR cc=0, 2 DO BEGIN    ;save output in the 'out' array
        y1 = FLOAT(col_low[cc])
        y2 = FLOAT(col_high[cc])
        
        m = (y2 -y1)/(x2 - x1)
        b = (x2*y1 - x1*y2)/(x2-x1)
        
        out[*,cc] = m*in + b
    ENDFOR
    
    RETURN, out
END

function log, in, val_low, val_high, col_low, col_high
    ;does a linear mapping of "log(in)" values to "out" values given specified ranges

    x1 = alog(float(val_low))
    x2 = alog(float(val_high))

    out = bytarr(n_elements(in),3)
    for cc=0, 2 do begin    ;save output in the 'out' array
        y1 = float(col_low[cc])
        y2 = float(col_high[cc])
        
        m = (y2 -y1)/(x2 - x1)
        b = (x2*y1 - x1*y2)/(x2-x1)
        
        out[*,cc] = m*alog(in) + b
    endfor
    
    return, out
end

PRO HCL_in, in, val_low, val_high, col_low, col_high,$       ;in
            h_low, c_low, l_low, h_high, c_high, l_high, ind ;out
    ;convert color in HCL
    COLORSPACES, col_low[0], col_low[1], col_low[2],  h_low,  c_low,  l_low,  /RGB_to_HCL
    h_low  = h_low[0]
    c_low  = c_low[0]
    l_low  = l_low[0]
    ;print, h_low,c_low,l_low
    COLORSPACES, col_high[0],col_high[1],col_high[2], h_high, c_high, l_high, /RGB_to_HCL
    h_high = h_high[0]
    c_high = c_high[0]
    l_high = l_high[0]

    ;;black/white example  ;;blue/gray example   ;;red/yellow example     
    ;h_low  = 260.         ;h_low  = 260.        ;h_low  = 0.
    ;h_high = 260.         ;h_high = 260.        ;h_high = 90.
    ;c_low  = 0.           ;c_low  = 80.         ;c_low  = 80.
    ;c_high = 0.           ;c_high = 0.          ;c_high = 30.
    ;l_low  = 30.          ;l_low  = 30.         ;l_low  = 30.
    ;l_high = 90.          ;l_high = 95.         ;l_high = 90.
    ;pow1 = 2.2            ;pow1 = 1.5           ;pow1 = 1./5.
    ;pow2 = 2.2            ;pow2 = 1.5           ;pow2 = 2.0         

    ;input values mapped in the interval [1,0]
    x1 = FLOAT(val_low )
    x2 = FLOAT(val_high)
    y1 = 1.
    y2 = 0.
    m = (y2 -y1)/(x2 - x1)
    b = (x2*y1 - x1*y2)/(x2-x1)
    ind = m*in + b  
END

PRO HCL_out, h, c, l, $ ;in
             out        ;out
    ;get back to RGB
    COLORSPACES, h, c, l, r, g, b, /HCL_to_RGB
    ;assemble output
    out = FLTARR(N_ELEMENTS(h),3)
    out[*,0] = r
    out[*,1] = g
    out[*,2] = b
END

FUNCTION HCL_cst_h, in, val_low, val_high, col_low, col_high
    ;interpolation in HCL space with constant Hue 

    HCL_in, in, val_low, val_high, col_low, col_high,$       ;in
            h_low, c_low, l_low, h_high, c_high, l_high, ind ;out

    pow1 = 1.5
    pow2 = 1.0

    h = REPLICATE(h_high,N_ELEMENTS(ind))
    c = c_high - (ind^pow1)*(c_high - c_low)
    l = l_high - (ind^pow2)*(l_high - l_low)

    HCL_out, h, c, l, $ ;in
             out        ;out
    
    RETURN, out
END

FUNCTION HCL_var_h, in, val_low, val_high, col_low, col_high
    ;interpolation in HCL space with variable Hue 

    HCL_in, in, val_low, val_high, col_low, col_high,$       ;in
            h_low, c_low, l_low, h_high, c_high, l_high, ind ;out

    pow1 = 1.5
    pow2 = 1.0

    h = h_high -  ind      *(h_high - h_low)
    c = c_high - (ind^pow1)*(c_high - c_low)
    l = l_high - (ind^pow2)*(l_high - l_low)

    HCL_out, h, c, l, $ ;in
             out        ;out
    
    RETURN, out
END


FUNCTION COL_LOW, in, val_low, val_high, col_low, col_high
    ;assign 'low'  color in range_out to all elements in 'in'
    sz = N_ELEMENTS(in)
    out = BYTARR(sz,3)
    FOR cc=0, 2 DO BEGIN    ;save output in the 'out' array
        out[*,cc] = REPLICATE(col_low[cc], sz)
    ENDFOR

    RETURN, out
END


FUNCTION COL_HIGH, in, val_low, val_high, col_low, col_high
    ;assign 'high' color in range_out to all elements in 'in'
    sz = N_ELEMENTS(in)
    out = BYTARR(sz,3)
    FOR cc=0, 2 DO BEGIN    ;save output in the 'out' array
        out[*,cc] = REPLICATE(col_high[cc], sz)
    ENDFOR

    RETURN, out
END


PRO PROCESS_COL, desired_n_col, col_arr, ins_message, SOLID=solid
    ;insure that dimension and type of user specified colors (col_arr) are compatible with 
    ;colors to be mapped.
    ;
    ;if dimensions or type don't match, program stops with error message
    ;otherwise col_arr is transformed to rgb and program returns without errors
    ;
    ;The solid keyword indicates that a solid colors is provided (as opposed to a low/high pair).
    
    ;define newline character
    IF (!D.NAME eq 'WIN') THEN newline = STRING([13B, 10B]) ELSE newline = string(10B)

    provided_n_col = N_ELEMENTS(col_arr)
    CASE 1 OF 
        provided_n_col EQ desired_n_col:     BEGIN   
                                                 ;colors were specified by name 
                                                 ;check type
                                                 IF ~ISA(col_arr, 'STRING') THEN BEGIN
                                                     MESSAGE, 'Error: in this context, color array must be a string.'
                                                     MESSAGE, ins_message
                                                 ENDIF
                                                 ;determine if dark and pale colors are to be choosen or only 
                                                 darkpale = 0
                                                 ;obtain rgb from provided name 
                                                 dummy = col_arr
                                                 TXT_TO_RGB, dummy, col_arr, ins_message, SOLID=solid
                                             END        
        provided_n_col EQ 3*desired_n_col:   BEGIN   
                                                 ;solid colors were specified by rgb 
                                                 IF N_ELEMENTS(solid) EQ 0 THEN BEGIN
                                                     MESSAGE, "Error: a color array of this dimension is ONLY accepted for solid colors", /INFORMATIONAL
                                                     MESSAGE, ins_message
                                                 ENDIF
                                                 ;check type
                                                 IF ~ISA(col_arr, /NUMBER) THEN BEGIN
                                                     MESSAGE, 'Error: in this context, color array must be a number.', /INFORMATIONAL
                                                     MESSAGE, ins_message
                                                 ENDIF
                                                 IF desired_n_col EQ 1 THEN BEGIN
                                                     ;insure validity of color
                                                     IS_COL_VALID, col_arr, ins_message
                                                 ENDIF ELSE BEGIN
                                                     ;check compatibility of dimensions
                                                     sz = SIZE(col_arr, /DIM)
                                                     IF N_ELEMENTS(sz) NE 2 THEN BEGIN
                                                         MESSAGE, 'Error: in this context, color array must be a 2D array of dimension 3xnumber_of_colors.', /INFORMATIONAL
                                                         MESSAGE, ins_message
                                                     ENDIF
                                                     IF (sz[0] EQ 3) AND (sz[1] EQ desired_n_col) THEN BEGIN
                                                         FOR nn=0, desired_n_col-1 DO BEGIN
                                                             ;insure validity of color
                                                             IS_COL_VALID, col_arr[*,nn], ins_message
                                                         ENDFOR
                                                         ;at this point, col_arr is of the right size and has valid colors
                                                         ;reshape it so that it conforms to color_arr with low/high values
                                                         ;eg col_arr that is 3x8 will become 3x2x8 with elements duplicated in the second dimension
                                                         reshaped = BYTARR(3,2,desired_n_col)
                                                         FOR nn=0, desired_n_col-1 DO BEGIN
                                                            reshaped[*,0,nn] = col_arr[*,nn]
                                                            reshaped[*,1,nn] = col_arr[*,nn]
                                                         ENDFOR
                                                         col_arr = reshaped
                                                     ENDIF ELSE BEGIN
                                                         MESSAGE, 'Error: in this context, color array must be of dimension 3xnumber_of_colors.', /INFORMATIONAL
                                                         MESSAGE, ins_message
                                                     ENDELSE

                                                 ENDELSE
                                             END
        provided_n_col EQ 3*2*desired_n_col: BEGIN   
                                                 ;low/high colors specified by rgb 
                                                 IF N_ELEMENTS(solid) NE 0 THEN BEGIN
                                                     MESSAGE, "Error: a color array of this dimension is NOT compatible for solid colors. ", /INFORMATIONAL 
                                                     MESSAGE, ins_message
                                                 ENDIF
                                                 ;check type
                                                 IF ~ISA(col_arr, /NUMBER) THEN BEGIN
                                                     MESSAGE, 'Error: in this context, color array must be a number.'
                                                     MESSAGE, ins_message
                                                 ENDIF
                                                 ;insure appropriate dimensions
                                                 sz = SIZE(col_arr, /DIM)
                                                 IF desired_n_col EQ 1 THEN BEGIN
                                                     ;one color is desired
                                                     IF N_ELEMENTS(sz) NE 2 THEN BEGIN
                                                         MESSAGE, 'Error: in this context, color array must be of dimension 3x2.', /INFORMATIONAL
                                                         MESSAGE, ins_message
                                                     ENDIF
                                                     IF (sz[0] EQ 3) AND (sz[1] EQ 2) THEN BEGIN
                                                         ;insure validity of colors
                                                         IS_COL_VALID, col_arr[*,0], ins_message
                                                         IS_COL_VALID, col_arr[*,1], ins_message
                                                     ENDIF ELSE BEGIN
                                                         MESSAGE, 'Error: in this context, color array must be of dimension 3x2.' , /INFORMATIONAL
                                                         MESSAGE, ins_message
                                                     ENDELSE
                                                 ENDIF ELSE BEGIN
                                                     IF N_ELEMENTS(sz) NE 3 THEN BEGIN
                                                         MESSAGE, 'Error: in this context, color array must be of dimension 3x2xnumber_of_colors.', /INFORMATIONAL
                                                         MESSAGE, ins_message
                                                     ENDIF
                                                     IF (sz[0] EQ 3) AND (sz[1] EQ 2) AND (sz[2] EQ desired_n_col) THEN BEGIN
                                                         FOR nn=0, desired_n_col-1 DO BEGIN
                                                             ;insure validity of colors
                                                             IS_COL_VALID, col_arr[*,0,nn], ins_message
                                                             IS_COL_VALID, col_arr[*,1,nn], ins_message
                                                         ENDFOR
                                                     ENDIF ELSE BEGIN
                                                         MESSAGE, 'Error: in this context, color array must be of dimension 3x2xnumber_of_colors.', /INFORMATIONAL
                                                         MESSAGE, ins_message
                                                     ENDELSE
                                                 ENDELSE
                                             END
        ELSE:                 BEGIN   ;dimension mismatch
                                  MESSAGE, 'Error: color array has incorrect dimensions.', /INFORMATIONAL
                                  MESSAGE, ins_message
                              END
    ENDCASE
END


PRO IS_OVER_UNDER_VALID, cond_val, cond_name, OVER_UNDER=over_under
    ;check validity of that boundary conditions defined in keywords  OVER_UNDER, OVER_HIGH, UNDER_LOW
    ;
    ;cond_val  = value of condition
    ;cond_name = name of condition
    ;OVER_UNDER is value set by user or a previous call to this procedure

    ;define newline character
    IF (!D.NAME eq 'WIN') THEN newline = STRING([13B, 10B]) ELSE newline = string(10B)

    ;instruction message
    ins_message = " "                                                                                                             + newline $
                 +"Instructions: the keyword "+cond_name+" must be set to: "                                                      + newline $
                 +"    1- 'exact'           no data values expected beyond the range of the color mapping "                       + newline $
                 +"    2- 'extend'          lowest and/or highest colors are used for data beyond the range of the color mapping" + newline $
                 +"    3- a named color:    eg: 'red'  "                                                                          + newline $
                 +"    4- a rgb color:      eg: [000,000,255]  "                                                                  + newline $
                 +"In all cases, only one string or color must be present. "
    n_el = N_ELEMENTS(cond_val)
    CASE n_el OF
        0   :BEGIN
                ;cond_val not defiend by user, set its default value 
                IF cond_name EQ 'OVER_UNDER' THEN BEGIN
                    ;OVER_UNDER is de default value for other conditions, set its value explicitely
                    cond_val = 'exact'
                ENDIF ELSE BEGIN
                    ;OVER_HIGH and UNDER_LOW take whatever value was set to OVER_UNDER by a previous call to this procedure
                    cond_val = over_under
                ENDELSE
             END
        1   :BEGIN
                ;3 options:  'exact', 'extend' or a 'named_color'
                IF ISA(cond_val, 'STRING') THEN BEGIN
                    CASE cond_val OF 
                        'exact'  :   ;do nothing, all is well
                        'extend' :   ;do nothing, all is well
                        ELSE     :   BEGIN
                                        ;a named color was provided, check its validity and retrieve rgb values
                                        TXT_TO_RGB, cond_val, rgb_val, ins_message, /solid
                                        cond_val = rgb_val
                                     END
                    ENDCASE
                ENDIF ELSE BEGIN
                    MESSAGE, "Error: keyword "+cond_name+", has only one element but is not a string.", /INFORMATIONAL
                    MESSAGE, ins_message
                ENDELSE
             END
        3   :BEGIN
                ;a 3 element color array was provided, check its validity
                PROCESS_COL, 1, cond_val, ins_message, /SOLID
             END
        ELSE:BEGIN
                MESSAGE, "Error: keyword "+cond_name+" has an unallowed number of elements. ", /INFORMATIONAL
                MESSAGE, ins_message
             END
    ENDCASE
END


PRO IS_COL_VALID, col, ins_message
    ;insure validity of a rbg color array 
    ;col = [r,g,b]

    IF N_ELEMENTS(col) EQ 3 THEN BEGIN
        IF TOTAL((col GE 0) AND (col LE 255)) NE 3 THEN BEGIN
            MESSAGE, 'Error: elements of the rgb colors must be in the interval [0, 255].', /INFORMATIONAL
            MESSAGE, ins_message
        ENDIF
    ENDIF ELSE BEGIN
        MESSAGE, 'Error: the rgb color provided must be a 3 element array', /INFORMATIONAL
        MESSAGE, ins_message
    ENDELSE
END


PRO IS_POS_VALID, pos
    ;insure validity of a position vector in normal coordinates
    ;pos = [x_bottom_left, y_bottom_left, x_top_right, y_top_right] 
    IF N_ELEMENTS(pos) EQ 4 THEN BEGIN
        range_ok = (pos GE 0.) AND (pos LE 1.)
        IF TOTAL(range_ok) NE 4 THEN BEGIN
            IF (!D.NAME eq 'WIN') THEN newline = STRING([13B, 10B]) ELSE newline = string(10B)
            print, 'Position array:', pos
            MESSAGE, 'Elements of position array must be in the interval [0.,1.].'
        ENDIF
    ENDIF ELSE BEGIN
        MESSAGE, 'The position vector provided must be a 4 element array.'
    ENDELSE
END


PRO IS_MAPPING_VALID, mapping, range_arr, over_high, col_high, under_low, col_low
    ;performs different quality checks on a mapping structure 
    ;returns range_arr: a vector containning the boundaries of all legs
    ;        over_high and under_low:   instructions on how to treat data with values beyond the palette
    ;        col_high  and col_low  :   lowest and highest color of palette

    ;# of mappings
    n_tot = N_ELEMENTS(mapping)

    ;check that certain descriptions appear no more than once
    low_extend  = 0
    low_exact   = 0
    high_extend = 0
    high_exact  = 0
    FOR nn=0, n_tot-1 DO BEGIN
        CASE mapping[nn].desc OF
            'high_extend':  BEGIN
                                IF high_extend EQ 0  THEN BEGIN
                                    high_extend ++
                                    IF high_exact NE 0  THEN BEGIN
                                        MESSAGE, " 'high_extend' and 'high_exact' both appear in the mapping structure" 
                                    ENDIF 
                                    over_high = 'extend'
                                    col_high  = mapping[nn].col_high
                                ENDIF ELSE BEGIN
                                    MESSAGE, " 'high_extend' appears more than once in a leg description"
                                ENDELSE
                            END
            'high_exact':   BEGIN
                                IF high_exact EQ 0  THEN BEGIN
                                    high_exact ++
                                    IF high_extend NE 0  THEN BEGIN
                                        MESSAGE, " 'high_exact' and 'high_extend' both appear in the mapping structure" 
                                    ENDIF 
                                    over_high = 'exact'
                                    col_high  = mapping[nn].col_high
                                ENDIF ELSE BEGIN
                                    MESSAGE, " 'high_exact' appears more than once in a leg description"
                                ENDELSE
                            END
            'low_extend':   BEGIN
                                IF low_extend EQ 0  THEN BEGIN
                                    low_extend ++
                                    IF low_exact NE 0  THEN BEGIN
                                        MESSAGE, " 'low_extend' and 'low_exact' both appear in the mapping structure" 
                                    ENDIF 
                                    under_low = 'extend'
                                    col_low   = mapping[nn].col_low
                                ENDIF ELSE BEGIN
                                    MESSAGE, " 'low_extend' appears more than once in a leg description"
                                ENDELSE
                            END
            'low_exact':    BEGIN
                                IF low_exact EQ 0  THEN BEGIN
                                    low_exact ++
                                    IF low_extend NE 0  THEN BEGIN
                                        MESSAGE, " 'low_exact' and 'low_extend' both appear in the mapping structure" 
                                    ENDIF 
                                    under_low = 'exact'
                                    col_low   = mapping[nn].col_low
                                ENDIF ELSE BEGIN
                                    MESSAGE, " 'low_exact' appears more than once in a leg description"
                                ENDELSE
                            END
            ELSE:
        ENDCASE
    ENDFOR

    ;insure that combination of operators is valid
    FOR nn=0, n_tot-1 DO BEGIN
        ;retrieve operators
        oh = mapping[nn].oper_high
        ol = mapping[nn].oper_low
        ;only data concerned by this leg
        ;allowed operators are LT LE EQ GE GT and NA    not all combinations are allowed
        CASE 1 OF
            (ol EQ 'LT') AND (oh EQ 'NA'): 
            (ol EQ 'LE') AND (oh EQ 'NA'): 
            (ol EQ 'EQ') AND (oh EQ 'NA'): 
            (ol EQ 'GE') AND (oh EQ 'LE'): 
            (ol EQ 'GE') AND (oh EQ 'LT'): 
            (ol EQ 'GE') AND (oh EQ 'NA'): 
            (ol EQ 'GT') AND (oh EQ 'LE'): 
            (ol EQ 'GT') AND (oh EQ 'LT'): 
            (ol EQ 'GT') AND (oh EQ 'NA'): 
            (ol EQ 'NA') AND (oh EQ 'LT'): 
            (ol EQ 'NA') AND (oh EQ 'LE'): 
            (ol EQ 'NA') AND (oh EQ 'EQ'): 
            (ol EQ 'NA') AND (oh EQ 'GE'): 
            (ol EQ 'NA') AND (oh EQ 'GT'): 
            ELSE : BEGIN
                      if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
                      MESSAGE, 'in LEG: '+STRING(nn)              +newline+ $
                      'the combination of operators:'             +newline+ $
                      'low: '+ STRING(ol)+'   high: '+ STRING(oh) +newline+ $
                      'is not allowed.'
                   END
        ENDCASE
    ENDFOR

    ;check that mapping structure covers the everywhere in the interval ]-infty, infty[

    ;1st check continuity of values for all color mappings
    problem = 0
    ;extract range and operator for all color legs  
    aa = WHERE(mapping[*].desc EQ 'col', n_col)
    IF n_col EQ 0 THEN BEGIN
        MESSAGE, "at least one leg must have the 'col' description"
    ENDIF
    val_low   = mapping[aa].val_low
    val_high  = mapping[aa].val_high
    oper_low  = mapping[aa].oper_low
    oper_high = mapping[aa].oper_high
    ;sort color mapping in terms ot their values
    bb = SORT(val_low)
    val_low   = val_low[bb]
    val_high  = val_high[bb]
    oper_low  = oper_low[bb]
    oper_high = oper_high[bb]
    ;insure continuity and retrieve boundary vector
    range_arr = FLTARR(n_col+1)
    FOR nn=0, n_col-1 DO BEGIN
        ;operators insure mapping between low and high value of this leg
        CASE 1 OF 
            (oper_low[nn] EQ 'GT') AND (oper_high[nn] EQ 'LT'): 
            (oper_low[nn] EQ 'GT') AND (oper_high[nn] EQ 'LE'): 
            (oper_low[nn] EQ 'GE') AND (oper_high[nn] EQ 'LT'): 
            (oper_low[nn] EQ 'GE') AND (oper_high[nn] EQ 'LE'): 
        ELSE:BEGIN
                MESSAGE, 'operators of color legs must insure mapping between low and high value of every legs', /INFORMATIONAL
                problem = 1
             END
        ENDCASE
        IF nn NE n_col-1 THEN BEGIN
            ;legs are chained appropriately
            IF nn NE n_col-1 THEN BEGIN
                IF val_high[nn] NE val_low[nn+1] THEN BEGIN
                    MESSAGE, 'Color legs must be chained to each other.', /INFORMATIONAL
                    MESSAGE, 'That is, the high value of a leg must be equal to the low value of the leg going above it', /INFORMATIONAL
                    problem = 1
                ENDIF
            ENDIF
            ;operators insure continuous mapping between legs
            CASE 1 OF 
                (oper_high[nn] EQ 'LT') AND (oper_low[nn+1] EQ 'GE'):   ;everything is fine
                (oper_high[nn] EQ 'LE') AND (oper_low[nn+1] EQ 'GT'):   
                (oper_high[nn] EQ 'LE') AND (oper_low[nn+1] EQ 'GE'):   MESSAGE, 'Warning: the value '+STRING(val_high[nn])+' gets mapped twice. You may wish to correct this.', /INFORMATIONAL
            ELSE:BEGIN
                    MESSAGE, 'operators of color legs must insure continuous mapping between low and high value of contiguous legs', /INFORMATIONAL
                    problem = 1
                 END
            ENDCASE
        ENDIF
        ;save value of range array
        range_arr[nn] = val_low[nn]
        IF nn EQ n_col-1 THEN range_arr[nn+1] = val_high[nn]
    ENDFOR

    ;insure continuous mapping between lowest value and -infinity
    val_low_col  =  val_low[0]
    oper_low_col = oper_low[0]
    FOR nn=0, n_tot-1 DO BEGIN
        IF (mapping[nn].desc EQ 'low_exact') OR (mapping[nn].desc EQ 'low_extend') THEN BEGIN
            ;one of the two values of the low condition of must match lowest color value
            CASE val_low_col OF
                mapping[nn].val_low : oper_low_cond = mapping[nn].oper_low
                mapping[nn].val_high: oper_low_cond = mapping[nn].oper_high
                ELSE:BEGIN
                        MESSAGE, "one value of the low condition mapping must match the lowest color value", /INFORMATIONAL
                        problem = 1
                     END
            ENDCASE
            ;operators must insure continuous mapping between the lowest color leg and the low condition
            CASE 1 OF
                oper_low_col EQ 'GE':BEGIN
                                        CASE 1 OF 
                                           oper_low_cond EQ 'LT': ;everything is fine 
                                           oper_low_cond EQ 'LE': MESSAGE, 'Warning: the value '+STRING(val_low_col)+' gets mapped twice. You may wish to correct this.', /INFORMATIONAL
                                        ELSE:BEGIN
                                                MESSAGE, 'operators of lowest color and the low condition must insure continuous mapping.', /INFORMATIONAL
                                                problem = 1
                                             END
                                        ENDCASE
                                     END
                oper_low_col EQ 'GT':BEGIN
                                        CASE 1 OF 
                                           oper_low_cond EQ 'LE': ;everything is fine
                                        ELSE:BEGIN
                                                MESSAGE, 'operators of lowest color and the low condition must insure continuous mapping', /INFORMATIONAL
                                                problem = 1
                                             END
                                        ENDCASE
                                     END
                ELSE:BEGIN
                         MESSAGE, 'operators of lowest color and the low condition must insure continuous mapping', /INFORMATIONAL
                         problem = 1
                     END
            ENDCASE
        ENDIF
    ENDFOR

    ;insure continuous mapping between highest value and +infinity
    val_high_col  =  val_high[n_col-1]
    oper_high_col = oper_high[n_col-1]
    FOR nn=0, n_tot-1 DO BEGIN
        IF (mapping[nn].desc EQ 'high_exact') OR (mapping[nn].desc EQ 'high_extend') THEN BEGIN
            ;one of the two values of the high condition of must match highest color value
            CASE val_high_col OF
                mapping[nn].val_low : oper_high_cond = mapping[nn].oper_low
                mapping[nn].val_high: oper_high_cond = mapping[nn].oper_high
                ELSE:BEGIN
                        MESSAGE, "one value of the high condition mapping must match the highest color value", /INFORMATIONAL
                        problem = 1
                     END
            ENDCASE
            ;operators must insure continuous mapping between the lowest color leg and the low condition
            CASE 1 OF
                oper_high_col EQ 'LE':BEGIN
                                        CASE 1 OF 
                                           oper_high_cond EQ 'GT': ;everything is fine 
                                           oper_high_cond EQ 'GE': MESSAGE, 'Warning: the value '+STRING(val_high_col)+' gets mapped twice. You may wish to correct this.' , /INFORMATIONAL
                                        ELSE:BEGIN
                                                MESSAGE, 'LE operators of highest color and the high condition must insure continuous mapping', /INFORMATIONAL
                                                problem = 1
                                             END
                                        ENDCASE
                                     END
                oper_high_col EQ 'LT':BEGIN
                                        CASE 1 OF 
                                           oper_high_cond EQ 'GE': ;everything is fine
                                        ELSE:BEGIN
                                                MESSAGE, 'LT operators of highest color and the high condition must insure continuous mapping', /INFORMATIONAL
                                                problem = 1
                                             END
                                        ENDCASE
                                     END
                ELSE:BEGIN
                         MESSAGE, 'operators of highest color and the high condition must insure continuous mapping', /INFORMATIONAL
                         problem = 1
                     END
            ENDCASE
        ENDIF
    ENDFOR
    ;in case of error, print leg boundaries and operators
    IF PROBLEM EQ 1 THEN BEGIN
        FOR nn=n_tot-1,0,-1 DO BEGIN
            print, oper_low[nn], val_low[nn], val_high[nn], '     '+oper_high[nn]
        ENDFOR
        MESSAGE, 'It appears that the color legs do not insure a continuous mapping from -infty to + infty'
    ENDIF
END


PRO MAP_LEGS, in, out, mapping, no_excep
    ;procedure that does the mapping of colors 

    ;initialize output
    sz = SIZE(in, /DIM)
    out = BYTARR([sz, 3])+!values.f_nan     ;one z dimension per color
    excep_map = BYTARR(sz)

    ;perform color mapping for each legs
    n_tot = N_ELEMENTS(mapping)
    FOR nn=0, n_tot-1 DO BEGIN
        ;retrieve operators
        oh = mapping[nn].oper_high
        ol = mapping[nn].oper_low
        ;only data concerned by this leg
        ;allowed operators are LT LE EQ GE GT and NA    not all combinations are allowed
        CASE 1 OF
            (ol EQ 'LT') AND (oh EQ 'NA'): aa = WHERE((in LT mapping[nn].val_low),                                  count)  
            (ol EQ 'LE') AND (oh EQ 'NA'): aa = WHERE((in LE mapping[nn].val_low),                                  count)  
            (ol EQ 'EQ') AND (oh EQ 'NA'): aa = WHERE((in EQ mapping[nn].val_low),                                  count)  
            (ol EQ 'GE') AND (oh EQ 'LE'): aa = WHERE((in GE mapping[nn].val_low) AND (in LE mapping[nn].val_high), count)  
            (ol EQ 'GE') AND (oh EQ 'LT'): aa = WHERE((in GE mapping[nn].val_low) AND (in LT mapping[nn].val_high), count)  
            (ol EQ 'GE') AND (oh EQ 'NA'): aa = WHERE((in GE mapping[nn].val_low),                                  count)  
            (ol EQ 'GT') AND (oh EQ 'LE'): aa = WHERE((in GT mapping[nn].val_low) AND (in LE mapping[nn].val_high), count)  
            (ol EQ 'GT') AND (oh EQ 'LT'): aa = WHERE((in GT mapping[nn].val_low) AND (in LT mapping[nn].val_high), count)  
            (ol EQ 'GT') AND (oh EQ 'NA'): aa = WHERE((in GT mapping[nn].val_low),                                  count)  
            (ol EQ 'NA') AND (oh EQ 'LT'): aa = WHERE(                                (in LT mapping[nn].val_high), count)  
            (ol EQ 'NA') AND (oh EQ 'LE'): aa = WHERE(                                (in LE mapping[nn].val_high), count)  
            (ol EQ 'NA') AND (oh EQ 'EQ'): aa = WHERE(                                (in EQ mapping[nn].val_high), count)  
            (ol EQ 'NA') AND (oh EQ 'GE'): aa = WHERE(                                (in GE mapping[nn].val_high), count)  
            (ol EQ 'NA') AND (oh EQ 'GT'): aa = WHERE(                                (in GT mapping[nn].val_high), count)  
            ELSE                         : BEGIN
                                              if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
                                              MESSAGE, 'The combination of operators:',           +newline+ $
                                                       'low: '+STRING(ol)+'   high: '+STRING(oh), +newline+ $
                                                       'is not allowed.'
                                           END
        ENDCASE

        ;do nothing if no data point match the condition
        IF count EQ 0 THEN CONTINUE

        ;remember where all exception points are
        IF mapping[nn].desc EQ 'excep' THEN BEGIN
            excep_map[aa] = 1
        ENDIF

        ;apply color mapping for this leg
        this_leg_rgb = CALL_FUNCTION(mapping[nn].fct,     in[aa],                $
                                     mapping[nn].val_low, mapping[nn].val_high, $
                                     mapping[nn].col_low, mapping[nn].col_high) 
        FOR cc=0, 2 DO BEGIN    ;save output in the 'out' array
            dummy = out[*,*,cc]
            dummy[aa] = this_leg_rgb[*,cc]
            out[*,*,cc] = dummy
        ENDFOR

    ENDFOR

    ;location of all values that are not recognized as exception points
    no_excep = WHERE(excep_map EQ 0, n_val)
    IF n_val EQ 0 THEN no_excep = -1
END


PRO TXT_TO_RGB, in, out, ins_message, SOLID=solid
    ;returns rgb value for named colors 
    ;if keyword solid is set, this routine returns only dark values for the required color
    ;otherwise, dark and pale colors are returned

    ;check that in is a string or a string array
    IF ~ISA(in, 'STRING') THEN BEGIN
        MESSAGE, 'Error: input variable must be a string or a string array.', /INFORMATIONAL
        MESSAGE, ins_message
    ENDIF

    ncol = N_ELEMENTS(in)
    out  = BYTARR(3,2,ncol)
    FOR nn=0, ncol-1 DO BEGIN
        IF STRMID(in[nn],0,5) EQ 'grey_' THEN BEGIN
            grey_col = FIX(STRMID(in[nn],5,3))
            bright = [grey_col, grey_col, grey_col]
            dark   = [grey_col, grey_col, grey_col]
        ENDIF ELSE BEGIN
            CASE in[nn] OF
            'blue':       BEGIN
                            bright = [169, 222, 255]
                            dark   = [000, 081, 237]
                          END
            'purple':     BEGIN
                            bright = [196, 194, 255]
                            dark   = [108, 036, 079]
                          END
            'green':      BEGIN
                            bright = [134, 222, 134]
                            dark   = [000, 134, 000]
                          END
            'red':        BEGIN
                            bright = [255, 190, 187]
                            dark   = [158, 000, 013]
                          END
            'orange':     BEGIN
                            bright = [255, 194, 124]
                            dark   = [255, 086, 000]
                          END
            'pink':       BEGIN
                            bright = [255, 217, 255]
                            dark   = [220, 000, 255]
                          END
            'brown':      BEGIN
                            bright = [223, 215, 208]
                            dark   = [096, 056, 019]
                          END
            'bright_red': BEGIN
                            bright = [255, 215, 184]
                            dark   = [215, 000, 013]
                          END
            'yellow':     BEGIN
                            bright = [255, 245, 169]
                            dark   = [255, 167, 000]
                          END
            'white':      BEGIN
                            bright = [255, 255, 255]
                            dark   = [255, 255, 255]
                          END
            'black':      BEGIN
                            bright = [000, 000, 000]
                            dark   = [000, 000, 000]
                          END
            'b_w':        BEGIN
                            bright = [255, 255, 255]
                            dark   = [000, 000, 000]
                          END
            ELSE  : BEGIN
                        if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
                        MESSAGE, 'Error: the color: "'+in[nn]+'" is not supported. ', /INFORMATIONAL
                        MESSAGE, ins_message
                    END
            ENDCASE
        ENDELSE

        IF KEYWORD_SET(solid) THEN BEGIN
            out[*,0,nn]  = dark
            out[*,1,nn]  = dark
        ENDIF ELSE BEGIN
            out[*,0,nn]  = dark
            out[*,1,nn]  = bright
        ENDELSE

    ENDFOR
END





PRO LEGS, DATA=in_in,             RANGE=range_arr_in,                                                   $ 
          MAPPING=mapping,        MAP_OVERRIDE=map_override,                                            $
          DARK_POS=dark_pos_in,   COLOR_ARR=color_arr_in,    N_COL=n_col_in,                            $
          MAP_ARR=map_arr_in,     SOLID=solid_in,                                                       $
          OVER_HIGH=over_high_in, UNDER_LOW=under_low_in,    OVER_UNDER=over_under_in,                  $
          EXCEP_VAL=excep_val_in, EXCEP_TOL=excep_tol_in,    EXCEP_COL=excep_col_in,                    $
          TV_OUT=tv_out_in,       IMG_OUT=img_out,                                                      $
          PALETTE=palette_in,     UNITS=units_in,            PAL_PROP=pal_prop_in,                      $
          CAT_NAMES=cat_names_in,                                                                       $
          YTICKS=yticks, YTICKV=ytickv, YTICKFORMAT=ytickformat, YTICKNAME=ytickname, CHARSIZE=charsize
;this procedure creates mapping structures for different types of color mapping
;it can be used to directly output mapping on the pst-script device
;it is also used to generate the color palettes necessary to interpret color figures


;map_override just ignores whatever mapping structure is passed to legs

;
;MAP_ARR is the vector containing the name of the function used for the mapping of each legs.
;        if set to a single string, then this string is used for all legs
;SOLID = 'col_low' 'col_high' or 'supplied'
;        is just a shoutcut that sets all elements of map_arr to 'col_low' or 'col_high'
;


;EXCEP_VAL  
;EXCEP_TOL  
;EXCEP_COL
;NODATA  is just a shortcut

;OVER_HIGH - 1) a named color   2) 'exact'    3) 'extend'   4) rgb col array
;OVER_LOW
;OVER_UNDER is a shortcup for controling both under and over at the same time
;            OVER_HIGH and OVER_LOW have preceedence over OVER_UNDER

;PAL_PROP either 'equal'
;            or  [v1, v2, ..., v(nticks)]

;order of colors is from lowest to highest in the palette
;eg col = [ [[ low  col dark], [low  col pale]], $
;           [[ high col dark], [high col pale]] ]
;dark_pos = [low col, ... , high col]





;if the provided presets do not satisfy your needs, custom color mappings can be created
;and passed to this procedure 
;in such cases, all override keywords are ignored

;template for a simple black and white mapping:
;it is assumed that range=[low,high] and nodata=float are provided
;
;    ;basis structure for color mapping and exceptions
;    leg       = {  desc:'', oper_low:'NA', oper_high:'NA',$
;                   col_low:BYTARR(3), col_high:BYTARR(3), $
;                   val_low:0.,        val_high:0.,        $
;                   fct:''}
;    n_col = 1
;    n_excep_val = 3
;    n_tot = n_col+n_excep_val
;    mapping = REPLICATE(leg, n_tot)
;    ;                        r   g   b
;    mapping[0].desc       = 'col'
;    mapping[0].oper_high  = 'LT'
;    mapping[0].oper_low   = 'GE'
;    mapping[0].col_high   = [255,255,255]
;    mapping[0].col_low    = [000,000,000]
;    mapping[0].val_high   = range[1]
;    mapping[0].val_low    = range[0]
;    mapping[0].fct        = 'lin'
;    ;low values
;    mapping[1].desc       = 'low_exact'
;    mapping[1].oper_low   = 'LT'
;    mapping[1].col_low    = mapping[0].col_low
;    mapping[1].val_low    = range[0]
;    mapping[1].fct        = 'col_low'
;    ;high values
;    mapping[2].desc       = 'high_exact'
;    mapping[2].oper_high  = 'GE'
;    mapping[2].col_high   = mapping[0].col_high
;    mapping[2].val_high   = range[1]
;    mapping[2].fct        = 'col_high'
;    ;nodata
;    mapping[3].desc       = 'excep'
;    mapping[3].oper_high  = 'LT'
;    mapping[3].oper_low   = 'GE'
;    mapping[3].col_low    = [220,220,220]
;    mapping[3].val_high   = nodata + nodata_tol
;    mapping[3].val_low    = nodata - nodata_tol
;    mapping[3].fct        = 'col_low'


;;error catching
;ON_ERROR, 2
;CATCH, the_error
;IF the_error NE 0 THEN BEGIN
;    ;CATCH, /Cancel
;    HELP, /Last_Message, Output=the_error_message
;    FOR j=0,N_ELEMENTS(the_error_message)-1 DO BEGIN
;        PRINT, the_error_message[j]
;    ENDFOR
;    print, 'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
;    STOP
;ENDIF

;define newline character
IF (!D.NAME eq 'WIN') THEN newline = STRING([13B, 10B]) ELSE newline = string(10B)

;define default color for nodata 
default_col_name = 'grey_120'
ins_message = 'Problem processing default color'
TXT_TO_RGB, default_col_name, default_col_rgb, ins_message, /solid


;rename data input keywords to avoid changing their values
IF N_ELEMENTS(in_in        ) NE 0 THEN in         = in_in             
IF N_ELEMENTS(tv_out_in    ) NE 0 THEN tv_out     = tv_out_in    
IF N_ELEMENTS(palette_in   ) NE 0 THEN palette    = palette_in   
IF N_ELEMENTS(units_in     ) NE 0 THEN units      = units_in     
IF N_ELEMENTS(pal_prop_in  ) NE 0 THEN pal_prop   = pal_prop_in  
IF N_ELEMENTS(cat_names_in ) NE 0 THEN cat_names   = cat_names_in  

;If a low-level mapping structure is not provided construct one.
IF N_ELEMENTS(mapping) EQ 0 OR KEYWORD_SET(map_override) THEN BEGIN
    ;a mapping structure needs to be constructed

    ;rename input keywords to avoid changing their values
    ;for certain variables, dimension are saved in structures to bypass IDL removing 0 lengths dimension from arrays passed to functions
    IF N_ELEMENTS(range_arr_in ) NE 0 THEN range_arr  = range_arr_in 
    IF N_ELEMENTS(dark_pos_in  ) NE 0 THEN dark_pos   = dark_pos_in  
    IF N_ELEMENTS(color_arr_in ) NE 0 THEN color_arr  = color_arr_in 
    IF N_ELEMENTS(n_col_in     ) NE 0 THEN n_col      = n_col_in     
    IF N_ELEMENTS(map_arr_in   ) NE 0 THEN map_arr    = map_arr_in   
    IF N_ELEMENTS(solid_in     ) NE 0 THEN solid      = solid_in     
    IF N_ELEMENTS(over_high_in ) NE 0 THEN over_high  = over_high_in 
    IF N_ELEMENTS(under_low_in ) NE 0 THEN under_low  = under_low_in 
    IF N_ELEMENTS(over_under_in) NE 0 THEN over_under = over_under_in
    IF N_ELEMENTS(excep_val_in ) NE 0 THEN excep_val  = excep_val_in 
    IF N_ELEMENTS(excep_tol_in ) NE 0 THEN excep_tol  = excep_tol_in 
    IF N_ELEMENTS(excep_col_in ) NE 0 THEN BEGIN
        ;nd = SIZE(in, /N_DIM)   ;number of dimension 
        ;sz = SIZE(in, /DIM)     ;size of each dimensions
        ;dim = LONARR(8) 
        ;IF nd EQ 0 THEN dim[0] = 1 ELSE dim[0:nd-1] = sz[0:nd-1]
        ;excep_col  = {values:SIZE(e
        excep_col  = excep_col_in 
    ENDIF


    ;check validity of solid keyword if it is provided
    if n_elements(solid) ne 0 then begin
        if ~isa(solid, 'string') then begin
            message, "Keyword solid can only be set to a string equal to 'supplied', 'col_high' or 'col_low'. " 
        endif else begin
            if ~(solid eq 'supplied' or solid eq 'col_high' or solid eq 'col_low') then begin
                message, "keyword solid can only be set to 'supplied', 'col_high' or 'col_low'. " 
            endif
        endelse
    endif

    ;COLORS
    n_supplied_col = N_ELEMENTS(color_arr)
    IF n_supplied_col NE 0 THEN BEGIN
        ;colors are provided by user
        ;print warning if n_col is provided as well as color_arr
        IF N_ELEMENTS(n_col) NE 0 THEN BEGIN
            MESSAGE, 'Warning: both COLOR_ARR and N_COL were provided.', /INFORMATIONAL
            MESSAGE, 'N_COL will be ignored.', /INFORMATIONAL
        ENDIF
        ins_message=  " "                                                                                                    + newline $
                    + "Instructions: COLOR_ARR can be specified in a few ways:                                       "       + newline $
                    + "   1- An array of named colors                                                                "       + newline $
                    + "      eg: COLOR_ARR=['red','green']                                                           "       + newline $
                    + "      for two color legs, one red and one green                                               "       + newline $
                    + "   2- An array of rgb colors describing low and high values of each color leg.                "       + newline $
                    + "      eg: COLOR_ARR= [[[255,000,000],[150,000,000]],[[000,255,000],[000,150,000]]]            "       + newline $
                    + "      for two color legs.                                                                     "       + newline $
                    + "      One going from dark red to bright red, the other going from dark green to bright green. "       + newline $
                    + "      In this context, the number of elements in COLOR_ARR must be a multiple of 6.           "       + newline $
                    + "   3- An array of rgb colors describing solid colors for each color leg.                      "       + newline $
                    + "      This requires that the SOLID keyword be specified.                                      "       + newline $
                    + "      a) With SOLID='col_high' or SOLID='col_low'                                             "       + newline $ 
                    + "         COLOR_ARR must be an array of named colors                                           "       + newline $ 
                    + "         eg: COLOR_ARR=['red','green']                                                        "       + newline $
                    + "         for two color legs, one solid red, the other solid green.                            "       + newline $
                    + "      b) With SOLID='supplied'                                                                "       + newline $ 
                    + "         COLOR_ARR must be an array of rgb colors                                             "       + newline $ 
                    + "         eg: COLOR_ARR= [[255,000,000],[000,255,000]]                                         "       + newline $
                    + "         for two color legs, one solid red, the other solid green.                            "       
        ;The expected data in color_arr depends on the keyword SOLID
        IF N_ELEMENTS(solid) EQ 0 THEN BEGIN
            ;Keyword SOLID not specified
            ;determine number of colors from color_arr
            IF ISA(color_arr[0], 'STRING') THEN BEGIN
                ;named colord were provided
                n_col = n_supplied_col               ;one name per leg
            ENDIF ELSE BEGIN
                ;rbg colors provided
                IF n_supplied_col MOD 6 NE 0 THEN BEGIN
                    MESSAGE, 'ERROR: In this context, the number of elements in COLOR_ARR must be a multiple of 6', /INFORMATIONAL
                    MESSAGE, ins_message
                ENDIF
                n_col = n_supplied_col/6             ;low and high colors needed for each legs two rgb values. Thus, there are 6 elements per color.
            ENDELSE
            ;check validity of user provided colors
            PROCESS_COL, n_col, color_arr, ins_message
        ENDIF ELSE BEGIN
            ;The keyword solid is speficied, color_arr must conform to certain conditions
            IF ~ISA(solid, 'STRING') THEN BEGIN
                MESSAGE, "Keyword SOLID can only be set to 'supplied', 'col_high' or 'col_low'. " 
            ENDIF
            CASE 1 OF
                solid EQ 'supplied' : BEGIN
                                          ;user is providing solid colors
                                          IF ISA(color_arr[0], 'STRING') THEN BEGIN
                                              MESSAGE, 'ERROR: In this context, COLOR_ARR must be an array containing rgb colors.', /INFORMATIONAL
                                              MESSAGE, ins_message
                                          ENDIF
                                          n_col = n_supplied_col/3
                                          PROCESS_COL, n_col, color_arr, ins_message, SOLID=solid
                                      END
                 solid EQ 'col_high'  OR $
                 solid EQ 'col_low'  :BEGIN
                                         ;solid colors are to be taken from predefined colors
                                         IF ~ISA(color_arr[0], 'STRING') THEN BEGIN
                                             MESSAGE, 'ERROR: In this context, COLOR_ARR must be an array containing named colors.', /INFORMATIONAL
                                             MESSAGE, ins_message
                                         ENDIF
                                         n_col = n_supplied_col
                                         PROCESS_COL, n_col, color_arr, ins_message
                                      END
                ELSE :                BEGIN
                                          MESSAGE, "Keyword SOLID can only be set to 'supplied', 'col_high' or 'col_low'. " 
                                      END
            ENDCASE
        ENDELSE
        ;From this point on, color_arr contains rgb values, change name for unambiguity
        IF N_ELEMENTS(SIZE(color_arr,/DIM)) NE 2 THEN color_arr = [[color_arr],[color_arr]] ;insure compatibility of array dimension
        colors_rgb = color_arr
    ENDIF ELSE BEGIN
        ;color array not provided by user, set default colors based on n_col
        IF N_ELEMENTS(n_col) EQ 0 THEN BEGIN
            ;default mapping when no information provided
            n_col = 1   
            ins_message = 'Problem with default palette color.'
            TXT_TO_RGB, 'b_w', colors_rgb, ins_message

            ;when using default black-and-white, nodata set default color for nodata to red
            default_col_name = 'red'
            ins_message = 'Problem with default exception color'
            TXT_TO_RGB, default_col_name, default_col_rgb, ins_message, /solid

        ENDIF ELSE BEGIN
            default_cols = ['brown','blue','green','orange','red','pink','purple','yellow']
            IF n_col GT N_ELEMENTS(default_cols) THEN BEGIN
                MESSAGE, 'n_col is greater than the number of colors allowed by default'
            ENDIF
            ins_message = 'Problem with default palette color.'
            TXT_TO_RGB, default_cols[0:n_col-1], colors_rgb, ins_message
        ENDELSE
    ENDELSE
    ;determine if dark colors go at low or high end of legs
    n_pos = N_ELEMENTS(dark_pos)
    CASE n_pos OF
    0:    BEGIN
              ;by default, dark colors appear at the top
              dark_lh = REPLICATE(1, n_col)
          END
    1:    BEGIN
              ;use whatever was provided in dark_pos
              CASE dark_pos OF
              'low' : dark_lh = REPLICATE(0, n_col)
              'high': dark_lh = REPLICATE(1, n_col)
               ELSE  : BEGIN
                         MESSAGE, " dark_pos can only be set to 'low' or 'high'"
                       END
               ENDCASE
          END
    ELSE: BEGIN
               ;an array of position was provided
               ;insure adequate number of entries
               IF N_ELEMENTS(dark_pos) NE n_col THEN BEGIN
                  MESSAGE, 'the number of entries in dark_pos must be the same as n_col'
               ENDIF
               IF TOTAL((dark_pos EQ 'low') + (dark_pos EQ 'high')) NE n_col THEN BEGIN
                  MESSAGE, " entries in dark_pos can only be set to 'low' or 'high'"
               ENDIF
               dark_lh = dark_pos EQ 'high'
          END
    ENDCASE


    ;MAPPING 
    n_solid = N_ELEMENTS(solid)
    n_map   = N_ELEMENTS(map_arr)
    IF n_solid NE 0 THEN BEGIN
        IF n_map NE 0 THEN BEGIN
            MESSAGE, "Both SOLID and MAP_ARR keywords were provided", /INFORMATIONAL
            MESSAGE, "SOLID will be ignored", /INFORMATIONAL
        ENDIF ELSE BEGIN
            ;build map_arr
            CASE n_solid OF
                1:    BEGIN
                        ;mapping specified by solid for all segments
                        CASE 1 OF
                            ;with solid = supplied, the same colors are provided for low and high 
                            ;'col_low' or 'col_high' have the same outcome
                            solid EQ 'supplied' :                             map_arr = REPLICATE('col_low',n_col)  
                            solid EQ 'col_high'  OR solid EQ 'col_low'  :     map_arr = REPLICATE( solid   ,n_col)
                        ENDCASE
                      END
                ELSE: BEGIN
                        ;solid for each legs were provided insure compatibility between n_solid and n_col
                        IF n_solid NE n_col THEN BEGIN
                            MESSAGE, 'SOLID must contain 1 or n_col elements'
                        ENDIF
                        map_arr = solid
                      END
            ENDCASE
            ;update n_map
            n_map   = N_ELEMENTS(map_arr)
        ENDELSE
    ENDIF
    CASE n_map OF
        0:    BEGIN
                ;no mapping specified, default mapping is 'lin' for all segments
                map_arr = REPLICATE('lin',n_col)
              END
        1:    BEGIN
                ;one function name provided, use it for all
                map_arr = REPLICATE(map_arr,n_col)
              END
        ELSE: BEGIN
                ;mapping for each legs were provided insure compatibility between n_map and n_col
                IF n_map NE n_col THEN BEGIN
                    MESSAGE, 'map_arr must contain 1 or n_col elements'
                ENDIF
              END
    ENDCASE

    ;INTERVALS
    ;intervals boundaries for the different legs
    n_range = N_ELEMENTS(range_arr)
    ;insure 
    CASE n_range OF 
        0:    BEGIN
                 MESSAGE, 'RANGE keyword must be specified when a mapping structure is not provided'
              END
        1:    BEGIN
                 MESSAGE, 'RANGE keyword must have at least two elements'
              END
        2:    BEGIN
                ;low and high bounds of range
                range_lh = [range_arr[0], range_arr[1]]
                IF n_col GT 1 THEN BEGIN
                    ;when range contains only two values range_arr is divided evenly between low and high values
                    range_arr = FINDGEN(n_col+1)/(n_col) * (range_lh[1]-range_lh[0]) + range_lh[0]
                ENDIF 
              END
        ELSE: BEGIN
                 ;range for multiple legs were specified, insure compatibility between n_range and n_col
                 IF n_range NE n_col+1 THEN BEGIN
                     MESSAGE, 'range_arr must contain 2 or n_col+1 elements'
                 ENDIF
                 ;low and high bounds of range
                 range_lh = [range_arr[0], range_arr[n_range-1]]
              END
    ENDCASE

    ;--EXCEPTIONS--
    ;if excepception data is provided, 
    ;check that dimension match and fill required parameters with default values
    n_excep_val = N_ELEMENTS(excep_val)
    IF n_excep_val NE 0 THEN BEGIN
        ;exception tolerances
        n_excep_tol = N_ELEMENTS(excep_tol)
        IF n_excep_tol EQ 0 THEN BEGIN
            ;default value for tolerance around exception point
            excep_tol = REPLICATE(1e-3, n_excep_val)
        ENDIF ELSE BEGIN
            ;insure dimention match
            IF n_excep_tol NE n_excep_val THEN BEGIN
                MESSAGE, 'EXCEP_TOL must have same dimension as EXCEP_VAL'
            ENDIF
        ENDELSE
        ;exception colors
        n_excep_col = N_ELEMENTS(excep_col)
        IF n_excep_col EQ 0 THEN BEGIN
            ;no colors were provided, set default
            temp_in = REPLICATE(default_col_name, n_excep_val)
            ins_message = 'Problem with default exception color.'
            TXT_TO_RGB, temp_in, excep_col, ins_message, /solid
        ENDIF ELSE BEGIN 
            ;exception colors provided by user
            ;check and process them
            ins_message = " "                                                                                            + newline $
                         +"Instructions: the keyword EXCEP_COL must be "                                                 + newline $
                         +"    1- A 1D array of named colors:    eg: ['red'        ,'blue'       , ... , 'green'      ] "+ newline $
                         +"    2- A 2D array of rgb   colors:    eg: [[000,000,255],[000,255,000], ... , [000,000,255]] "+ newline $
                         +"In both cases, the number of colors represented must be the same as "                         + newline $
                         +"the number of exceptions provided in EXCEP_VAL."
            PROCESS_COL, n_excep_val, excep_col, ins_message, /solid
        ENDELSE
        ;reshape it so that it conforms to color_arr with low/high values
        ;eg col_arr that is 3x8 will become 3x2x8 with elements duplicated in the second dimension
        reshaped = BYTARR(3,2,n_excep_val)
        IF n_excep_val EQ 1 THEN BEGIN
           FOR nn=0, n_excep_val-1 DO BEGIN
               reshaped[*,0,nn] = excep_col[*,nn]
               reshaped[*,1,nn] = excep_col[*,nn]
            ENDFOR
        ENDIF ELSE BEGIN
            FOR nn=0, n_excep_val-1 DO BEGIN
               reshaped[*,0,nn] = excep_col[*,0,nn]
               reshaped[*,1,nn] = excep_col[*,1,nn]
            ENDFOR
        ENDELSE
        excep_col = reshaped
    ENDIF 
    ;Add 2 exceptions, for pts above the high range of the color bar:
    ;One for pts below palette range, another for pts above 
    n_excep_val += 2 
    ;Set default value and/or check validity of conditions to be respected above and below color mapping
    IS_OVER_UNDER_VALID, over_under, 'OVER_UNDER'
    IS_OVER_UNDER_VALID, over_high,  'OVER_HIGH', OVER_UNDER=over_under
    IS_OVER_UNDER_VALID, under_low,  'UNDER_LOW', OVER_UNDER=over_under

    ;basis structure for color mapping and exceptions
    leg     = {  desc:'', oper_high:'NA', oper_low:'NA',          $
                 col_low:BYTARR(3),       col_high:BYTARR(3),     $
                 val_low:!values.f_nan,   val_high:!values.f_nan, $
                 fct:''}

    ;build mapping structure 
    n_tot = n_col+n_excep_val
    mapping = REPLICATE(leg, n_tot)
    
    ;fill in mapping structure
    FOR nn=0, n_col-1 DO BEGIN
       ;low and high colors for each legs
       this_low_col  = colors_rgb[*, dark_lh[nn],nn]    
       this_high_col = colors_rgb[*,~dark_lh[nn],nn]

        IF n_col EQ 1 THEN BEGIN
           mapping[nn].oper_high = 'LE'
           mapping[nn].oper_low  = 'GE'
           pal_low_col           = this_low_col
           pal_high_col          = this_high_col
           range_high            = range_arr[nn+1]
           range_low             = range_arr[nn]
        ENDIF ELSE BEGIN
           CASE nn OF
               0:        BEGIN
                             mapping[nn].oper_high = 'LT'
                             mapping[nn].oper_low  = 'GE'
                             pal_low_col           = this_low_col
                             range_low             = range_arr[nn]
                         END
               n_col-1:  BEGIN
                             mapping[nn].oper_high = 'LE'
                             mapping[nn].oper_low  = 'GE'
                             pal_high_col          = this_high_col
                             range_high            = range_arr[nn+1]
                         END
               ELSE:     BEGIN
                             mapping[nn].oper_high = 'LT'
                             mapping[nn].oper_low  = 'GE'
                         END
           ENDCASE
        ENDELSE      
        mapping[nn].desc     = 'col'
        mapping[nn].col_high = this_high_col
        mapping[nn].col_low  = this_low_col
        mapping[nn].val_high = range_arr[nn+1]
        mapping[nn].val_low  = range_arr[nn]
        mapping[nn].fct      = map_arr[nn]
    ENDFOR

    ;define mapping of values below and above range of palette
    ;high values
    IF N_ELEMENTS(over_high) EQ 1 THEN BEGIN
        CASE over_high OF
        'exact':    BEGIN
                        ;values exceeding val_high are not expected
                        ;if they are found, program will complain and they will be shown in grey
                        desc_high = 'high_exact'
                        oper_high = 'GT'
                        col_high  = default_col_rgb[*,0]
                    END
        'extend':   BEGIN
                        ;extend color already associated with highest value
                        desc_high = 'high_extend'
                        oper_high = 'GT'
                        col_high  = pal_high_col
                    END
        ELSE:       BEGIN
                        MESSAGE, "OVER_HIGH set to something else than 'exact' or 'extend', this should have been caught by IS_OVER_UNDER_VALID "
                    END
        ENDCASE
    ENDIF ELSE BEGIN
        ;rgb color was provided 
        desc_high = 'high_extend'
        oper_high = 'GT'
        col_high = over_high[*,0]
    ENDELSE
    ;fill in values of the mapping operator
    mapping[n_col].desc       = desc_high
    mapping[n_col].oper_high  = oper_high
    mapping[n_col].col_high   = col_high
    mapping[n_col].val_high   = range_high
    mapping[n_col].fct        = 'col_high'

    ;low values
    IF N_ELEMENTS(under_low) EQ 1 THEN BEGIN
        CASE under_low OF
        'exact':    BEGIN
                        ;values lower than val_low are not expected
                        ;if they are found, program will complain and they will be shown in grey
                        desc_low = 'low_exact'
                        oper_low = 'LT'
                        col_low  = default_col_rgb[*,0]
                    END
        'extend':   BEGIN
                        ;extend color already associated with lowest value
                        desc_low = 'low_extend'
                        oper_low = 'LT'
                        col_low  = pal_low_col
                    END
        ELSE:       BEGIN
                        MESSAGE, "UNDER_LOW set to something else than 'exact' or 'extend', this should have been caught by IS_OVER_UNDER_VALID "
                    END
        ENDCASE
    ENDIF ELSE BEGIN
        ;rgb color was provided 
        desc_low = 'low_extend'
        oper_low = 'LT'
        col_low = under_low[*,0]
    ENDELSE
    ;fill in values of the mapping operator
    mapping[n_col+1].desc       = desc_low
    mapping[n_col+1].oper_low   = oper_low
    mapping[n_col+1].col_low    = col_low
    mapping[n_col+1].val_low    = range_low
    mapping[n_col+1].fct        = 'col_low'

    ;define other exception points
    n_excep_val -= 2       ;exceptions for low and high points have already been dealt with
    FOR nn=0, n_excep_val-1 DO BEGIN
        ind = n_col+2+nn

        v_low  = excep_val[nn] - excep_tol[nn]
        v_high = excep_val[nn] + excep_tol[nn]

        ;fill in values of the mapping operator
        mapping[ind].desc       = 'excep'
        mapping[ind].oper_high  = 'LT'
        mapping[ind].oper_low   = 'GT'
        mapping[ind].val_high   = v_high
        mapping[ind].col_low    = excep_col[*,0,nn]
        mapping[ind].val_low    = v_low
        mapping[ind].fct        = 'col_low'
    ENDFOR
ENDIF ELSE BEGIN
    ;a mapping structure was provided, complain if defining variables 
    ;are provided at the same time
    too_much_info = 0
    IF N_ELEMENTS(range_arr_in ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: RANGE_ARR  was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(dark_pos_in  ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: DARK_POS   was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(color_arr_in ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: COLOR_ARR  was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(n_col_in     ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: N_COL      was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(map_arr_in   ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: MAP_ARR    was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(solid_in     ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: SOLID      was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(over_high_in ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: OVER_HIGH  was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(under_low_in ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: UNDER_LOW  was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(over_under_in) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: OVER_UNDER was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(excep_val_in ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: EXCEP_VAL  was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(excep_tol_in ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: EXCEP_TOL  was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF N_ELEMENTS(excep_col_in ) NE 0 THEN BEGIN
        MESSAGE, 'WARNING: input keyword: EXCEP_COL  was provided at the same time as a mapping structure', /INFORMATIONAL
        too_much_info = 1
    ENDIF
    IF too_much_info EQ 1 THEN BEGIN
        MESSAGE, 'If you wanted to update an existing mapping, please set the MAP_OVERRIDE keyword to 1.', /INFORMATIONAL
    ENDIF
ENDELSE


;from here on, there is no difference wheter a mapping structure was just created of has
;been passed as an argument


;perform different checks to insure validity of mapping structure
IS_MAPPING_VALID, mapping, range_arr, over_high, col_high, under_low, col_low

;if no data provided and we are not plotting a palette, we are done
IF (N_ELEMENTS(in) EQ 0) AND (N_ELEMENTS(palette) EQ 0) THEN RETURN 

;recover low and high values being mapped
n_ticks = N_ELEMENTS(range_arr)
n_col = n_ticks-1
val_high = range_arr[n_ticks-1]
val_low  = range_arr[0]



;create data if drawing a palette
IF N_ELEMENTS(palette) NE 0 THEN BEGIN
    ;number of points in palette
    n_pts = 501

    n_prop = N_ELEMENTS(pal_prop) 
    ;if plotting a categorical palette, we want all colors to be proportional
    IF N_ELEMENTS(cat_names) NE 0 THEN BEGIN
        ;insure number of elements is good
        IF N_ELEMENTS(cat_names) NE n_col THEN BEGIN
            MESSAGE, 'The number of categories in "CAT_NAMES" should be equal to the number of colors being specified'
        ENDIF
        IF n_prop EQ 1 THEN MESSAGE, 'PAL_PROP is automatically set to "equal" when the "CAT_NAMES" keyword is defined', /INFORMATIONAL
        pal_prop='equal'
        n_prop = 1
    ENDIF

    IF n_prop EQ 0 THEN BEGIN
        ;default behavior is to show values with equal spacing from low to high
        yrange = [val_low,val_high]
        IF N_ELEMENTS(yticks) EQ 0 THEN begin
            yticks = n_col
            IF N_ELEMENTS(ytickv) EQ 0 THEN ytickv = range_arr
        endif
        ;values in palette
        ;fixme  data should be created for each legs of a palette
        if mapping[0].fct eq 'log' then begin
            in = exp(ROTATE(FINDGEN(n_pts)/(n_pts-1)*(alog(val_high) - alog(val_low)) + alog(val_low), 1))
            ylog=1
        endif else begin
            in = ROTATE(FINDGEN(n_pts)/(n_pts-1)*(val_high - val_low) + val_low, 1)
            ylog=0
        endelse

    ENDIF ELSE BEGIN
        IF n_prop EQ 1 THEN BEGIN
            IF pal_prop EQ 'equal' THEN BEGIN
                ;with PAL_PROP='equal', all color legs have the same size in the palette irrespective of the data interval
                yrange    = [0.,1]
                yticks    = n_col
                ytickv    = FLTARR(n_ticks)
                ytickname = STRARR(n_ticks)
                IF N_ELEMENTS(ytickformat) EQ 0 THEN ytickformat = '(f5.2)'
                n_pts_col = FLOOR(n_pts/n_col) + 1    ;number of points for each color
                n_pts     = n_pts_col * n_col
                in = ROTATE(FLTARR(n_pts), 1)
                FOR nn=0, n_col-1 DO BEGIN
                    this_high = range_arr[nn+1]
                    this_low  = range_arr[nn]
                    ;determine position of ticks in normalized axis
                    ytickv[nn] = 1.*(nn*n_pts_col)/(n_pts)
                    ytickname[nn]  =  STRING(range_arr[nn],FORMAT=ytickformat)
                    IF nn EQ n_col-1 THEN BEGIN
                        ;values in the range   [this_low, this_high]
                        val_arr = FINDGEN(n_pts_col)/(n_pts_col-1)*(this_high - this_low) + this_low
                        ytickv[nn+1]     = 1.
                        ytickname[nn+1]  =  STRING(range_arr[nn+1],FORMAT=ytickformat)
                    ENDIF ELSE BEGIN
                        ;values in the range   [this_low, this_high[
                        val_arr = FINDGEN(n_pts_col)/(n_pts_col  )*(this_high - this_low) + this_low
                    ENDELSE
                    in[nn*n_pts_col:nn*n_pts_col+n_pts_col-1] = val_arr
                ENDFOR
                ;erase ytickformat, without this, AXIS will not process information in ytickname
                ytickformat=''
            ENDIF ELSE BEGIN
                MESSAGE, 'The Keyword PAL_PROP can only be set to "equal"'
            ENDELSE
        ENDIF ELSE BEGIN
                MESSAGE, 'The Keyword PAL_PROP can only be set to "equal"'
        ENDELSE
    ENDELSE
ENDIF



;map data to rgb 
MAP_LEGS, in, img_out, mapping, no_excep



;Complain if high (low) value of palette were set to exact yet values were found above (below) these values
IF over_high EQ 'exact' THEN BEGIN
    IF no_excep[0] EQ -1 THEN BEGIN    
        ;no exception points were found consider all data points
        aa = WHERE(in GT val_high,count)
    ENDIF ELSE BEGIN
        ;some exception points were found consider all other data points
        aa = WHERE(in[no_excep] GT val_high,count)
    ENDELSE
    IF count GT 0 THEN BEGIN
        if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
        MESSAGE, 'Warning: high value of palette was specified as "exact" but' +newline+ $
                 'points in data were found to exceed maximum value.'          +newline+ $
                 'These points will appear in the default exception color', /INFORMATIONAL
    ENDIF
ENDIF
IF under_low EQ 'exact' THEN BEGIN
    IF no_excep[0] EQ -1 THEN BEGIN    
        ;no exception points were found consider all data points
        aa = WHERE(in LT val_low,count)
    ENDIF ELSE BEGIN
        ;some exception points were found consider all other data points
        aa = WHERE(in[no_excep] LT val_low,count)
    ENDELSE
    IF count GT 0 THEN BEGIN
        if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
        MESSAGE, 'Warning: low value of palette was specified as "exact" but' +newline+ $
                 'points in data were found smaller than minimum value.'      +newline+ $
                 'These points will appear in the default exception color', /INFORMATIONAL
    ENDIF
ENDIF


;TV output in post-script if required
IF N_ELEMENTS(tv_out) NE 0 THEN BEGIN
    ;check validity of position vector
    IS_POS_VALID, tv_out
    ;the actual TV
    LOADCT, 0, /S       ;do not remove! This is required because of a bug in IDL
    TV, img_out, tv_out[0], tv_out[1], XS=tv_out[2]-tv_out[0], YS=tv_out[3]-tv_out[1], /NORMAL, TRUE=3
ENDIF

;plot palette if required
IF N_ELEMENTS(palette) NE 0 THEN BEGIN

    ;insure that units have been defined
    IF ~KEYWORD_SET(units) THEN units='units not defined'

    ;check validity of position vector
    IS_POS_VALID, palette

    ;plot palette
    LOADCT, 0, /S       ;do not remove! This is required because of a bug in IDL
	TV, img_out, palette[0], palette[1],  XS=palette[2]-palette[0], YS=palette[3]-palette[1], /NORMAL, TRUE=3

    ;if required, draw extension triangles for high values
    thick = 1.      ;of line
    t_prop = .02    ;height of triangle in proportion of the palette height
    t_height = t_prop*(palette[3]-palette[1])
    IF N_ELEMENTS(over_high) EQ 1 THEN BEGIN
        IF over_high EQ 'exact' THEN BEGIN
            ;no extension, close box
            PLOTS, [palette[0],palette[2]], [palette[3],palette[3]], COL=0, /NORMAL, THICK=thick
            plot_trig = 0
        ENDIF ELSE BEGIN
            plot_trig = 1
        ENDELSE
    ENDIF ELSE BEGIN
        plot_trig = 1
    ENDELSE
    IF plot_trig EQ 1 THEN BEGIN
        x_triangle = [palette[0], palette[0]+(palette[2]-palette[0])/2., palette[2]]
        y_triangle = [palette[3], palette[3]+t_height, palette[3]]
        ;load color into a palette since POLYFILL does not accept true colors
        TVLCT, col_high[0], col_high[1], col_high[2]      
        ;color polygon
        POLYFILL, x_triangle, y_triangle, COL=0, /NORMAL
        ;black outline
        LOADCT, 0, /S
        PLOTS, x_triangle, y_triangle, COL=0, /NORMAL, THICK=thick
    ENDIF

    ;if required, draw extension triangles for low values
    IF N_ELEMENTS(under_low) EQ 1 THEN BEGIN
        IF under_low EQ 'exact' THEN BEGIN
            ;no extension, close box
            PLOTS, [palette[0],palette[2]], [palette[1],palette[1]], COL=0, /NORMAL, THICK=thick
            plot_trig = 0
        ENDIF ELSE BEGIN
            plot_trig = 1
        ENDELSE
    ENDIF ELSE BEGIN
        plot_trig = 1
    ENDELSE
    IF plot_trig EQ 1 THEN BEGIN
        x_triangle = [palette[0], palette[0]+(palette[2]-palette[0])/2., palette[2]]
        y_triangle = [palette[1], palette[1]-t_height, palette[1]]
        ;load color into a palette since POLYFILL does not accept true colors
        TVLCT, col_low[0], col_low[1], col_low[2]      
        ;color polygon
        POLYFILL, x_triangle, y_triangle, COL=0, /NORMAL
        ;black outline
        LOADCT, 0, /S
        PLOTS, x_triangle, y_triangle, COL=0, /NORMAL, THICK=thick
    ENDIF

    ;draw black lines on sides of palette
    LOADCT, 0, /S
    PLOTS, [palette[0], palette[0]], [palette[1], palette[3]], COL=0, /NORMAL, THICK=thick
    PLOTS, [palette[2], palette[2]], [palette[1], palette[3]], COL=0, /NORMAL, THICK=thick
    ;set up coords for axis call
	PLOT, [0.],[0.],POS=palette, /NODATA, /NOERASE, XSTYLE=4, YSTYLE=4
    ;plot ticks
    IF N_ELEMENTS(cat_names) EQ 0 THEN BEGIN
        ;normal palette axis with tick values at color intersections
        AXIS, YAXIS=1, /NOERASE, YSTYLE=1,YTICKLEN=0.1, CHARSIZE=charsize,$
              YTICKS=yticks, YTICKV=ytickv, YRANGE=yrange, YTICKFORMAT=ytickformat, YTICKNAME=ytickname, YTITLE=units, ylog=ylog
    ENDIF ELSE BEGIN
        ;categorical palette with tick values at the center of colors
        yticks -= 1
        dt     = ytickv[1]-ytickv[0]
        ytickv -= dt/2.
        ytickv = ytickv[1:N_ELEMENTS(ytickv)-1]
        AXIS, YAXIS=1, /NOERASE, YSTYLE=1,YTICKLEN=0.1, CHARSIZE=charsize,$
              YTICKS=yticks, YTICKV=ytickv, YRANGE=yrange, YTICKNAME=cat_names
    ENDELSE

    ;make palette data available on output
    in_in = in
          



ENDIF




END
