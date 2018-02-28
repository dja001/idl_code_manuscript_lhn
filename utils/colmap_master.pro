PRO COLMAP_MASTER, in, out, range, $
                   TVOUT=tvout, $
                   TRUE=true, $
                   ZEROTOL=zerotol, $
                   NODATA=nodata, $
                   PALETTE=palette, $
                   POS=pos, UNIT=unit, $
                   YTICKS=yticks, $       
                   YTICKV=ytickv, $ 
                   YTICKFORMAT=ytickformat, $ 
                   BLACK_WHITE_BH=black_white_bh, $
                   BLACK_WHITE_WH=black_white_wh, $
                   ANY_WHITE=any_white, $
                   PALEBLUE_DARKBLUE=paleblue_darkblue, $
                   PALEORANGE_DARKRED=paleorange_darkred, $
                   DBZ=dbz, $
                   TWOCOL_WITH_ZERO=twocol_with_zero, $
                   MULTI_COL_LOW_HIGH=multi_col_low_high,         $
                   LOG_PVE=log_pve,         $
                   COLV=colv, $
                   OVERSAMPLE=oversample
;map variable to idl color index

;KEYWORDS:
;    YTICKS    number of tick interval      (n + 1) thck marks
;    YTICKV    exact value for tick marks  with this keyword, yticks is automatically set
;    COLV      array of color number starting with the lowest value
;              compatible with:
;                   multi_col_low_high
;                   two_col_with_zero
;              color index:
;                   0 = white
;                   1 = black
;                   2 = blue
;                   3 = purple
;                   4 = green
;                   5 = orange
;                   6 = cherry red
;                   7 = pink
;                   8 = brown
;                   9 = bright red
;                   10 = Yellow
;       

;this array must fit plot, ovsersample for squares perfectly aligned
IF KEYWORD_SET(oversample) THEN BEGIN
    sz = SIZE(in, /DIM)  ;resample to avoid area/value problems
    in = REBIN(in, 2*sz[0], 2*sz[1], /sample)
    in = in[1:2*sz[0]-2,1:2*sz[1]-2]
ENDIF


;color RGB definition
;           dark            pale
col = [[[000, 081, 237], [169, 222, 255]], $      ;blue
       [[108, 036, 079], [196, 194, 255]], $      ;purple
       [[000, 134, 000], [134, 222, 134]], $      ;green
       [[255, 086, 000], [255, 194, 124]], $      ;cherry red
       [[158, 000, 013], [255, 190, 187]], $      ;orange
       [[220, 000, 255], [255, 217, 255]], $      ;pink
       [[096, 056, 019], [223, 215, 208]], $      ;brown
       [[215, 000, 013], [255, 215, 184]], $      ;bright red
       [[255, 167, 000], [255, 245, 169]]]        ;Yellow
;      ...

;set number of yticks if tick value are provided
IF KEYWORD_SET(ytickv) THEN yticks = N_ELEMENTS(ytickv)-1


;rename for clarity
min = range[0]
max = range[1]

;if ploting out a palette, make an array of values from min to max
IF KEYWORD_SET(palette) THEN BEGIN                      ;set input to all possible values in range
    undefined = 0
    IF N_ELEMENTS(in) NE 0 THEN BEGIN       ;store original variable to restore it at the end
        orig_in = in
    ENDIF ELSE BEGIN 
        undefined = 1                       ;set undefined variable flag
    ENDELSE
    in = ROTATE(FINDGEN(401)/400.*(max - min) + min, 1)
ENDIF

;initialize output vector
sz = SIZE(in, /DIM)
out = INTARR(sz)



;based on color table, load palette, defind special values and type of mapping
IF KEYWORD_SET(black_white_bh) THEN BEGIN          ;black and white with black are high values
    ;load default idl black and white palette
    LOADCT, 0, /S
    index_range =   [255,0]
    index_lowhigh = [255,0]
    index_nodata = 255
    maptype = 'low_high'
ENDIF
IF KEYWORD_SET(black_white_wh) THEN BEGIN          ;black and white with white are high values
    ;load default idl black and white palette
    LOADCT, 0, /S
    index_range =   [0,255]
    index_lowhigh = [0,255]
    index_nodata = 255
    maptype = 'low_high'
ENDIF
IF KEYWORD_SET(paleblue_darkblue) THEN BEGIN          ;pale blue to dark blue
    ;load palette
    RESTORE, '~/idl/palette/linear_w_zero.pal'
    TVLCT, red_values, green_values, blue_values
    index_range = [0, 62]
    index_lowhigh = [0, 62]
    index_nodata = 254
    maptype = 'low_high'
ENDIF
IF KEYWORD_SET(paleorange_darkred) THEN BEGIN          ;pale orange to dark red
    ;load palette
    RESTORE, '~/idl/palette/linear_w_zero.pal'
    TVLCT, red_values, green_values, blue_values
    index_range = [125, 187]
    index_lowhigh = [125, 187]
    index_nodata = 254
    maptype = 'low_high'
ENDIF
IF KEYWORD_SET(dbz) THEN BEGIN          ;sort of rainbow appropriate for reflectivity
    ;load palette
    ;53 black
    ;52 white
    ;51 grey 200
    ;50 grey 100
    rr=[0,0,0,0,10,20,30,30,40,30,20,10,10,0,0,0,0,0,0,50,75,75,100,118,136,154,154,173,179,186,186,192,255,250,245,245,240, $
        222,213,213,205,196,188,179,179,171,162,147,147,156,100,200,255,000];,165,174,183,183,192,201,210,219,219,228,237,246,246,255,200, 90]
    gg=[0,0,0,0,0,0,0,0,0,19,38,57,57,76,92,108,108,124,140,152,158,158,164,172,181,190,190,199,202,206,206,210,171,155,139, $
        139,124,82,71,71,61,51,41,30,30,20,10,0,0,21,100,200,255,000];,42,63,85,85,106,127,148,170,170,191,212,233,233,255,200, 90]
    bb=[75,82,90,90,104,119,133,133,148,158,168,178,178,188,175,162,162,149,136,68,34,34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,5, $
        11,17,23,28,28,34,40,149,149,157,100,200,255,000];,166,175,184,184,193,202,210,219,219,228,237,246,246,255,200, 90]
    TVLCT, rr, gg, bb
    index_range = [0, 49]
    index_lowhigh = [0, 49]
    index_nodata = 52
    maptype = 'low_high'
ENDIF
IF KEYWORD_SET(twocol_with_zero) THEN BEGIN          ;sort of rainbow appropriate for reflectivity
    IF N_ELEMENTS(colv) EQ 0 THEN BEGIN
        ;load palette
        ;                0-255 call          r       g       b
        ;rose pale       250                 237     212     237
        ;mauve fonce     188                 147     0       149
        ;rouge fonce     187                 162     10      40      
        ;orange pale     125                 255     171     0
        ;vert pale       124                 192     210     0
        ;vert fonce      63                  0       44      0
        ;bleu fonce      62                  0       0       75  
        ;bleu pale       0                   170     255     255
        ;gris           255
        ;blanc          254
        ;noir           253
        RESTORE,filename='~/idl/palette/linear_w_zero.pal'
        TVLCT, red_values, green_values, blue_values
        ind_mid = 125
        ntot = 250
        zero_val = 255
        index_nodata = 253
    ENDIF ELSE BEGIN
        ;make red blue green vectors
        ncol = N_ELEMENTS(colv)     ;number of colors
        neach = FLOOR(251./ncol)    ;number of "degrade" for each color
        ntot = neach*ncol           ;index for last color
        ;initialize colors
        in_v = FINDGEN(neach)
        rr = INTARR(256)
        gg = INTARR(256)
        bb = INTARR(256)
        ind0= 0     ;start index for each color
        ;map -ve colors
        FOR nn=0, ncol/2-1 DO BEGIN
            ;                                 dimension signification
            ;                               [rgb,  dark pale, color]
            LINMAP, in_v, dummy,[0, neach-1], [col[0,0,colv[nn]], col[0,1,colv[nn]]]     ;red
            rr[ind0:ind0+neach-1] = dummy
            LINMAP, in_v, dummy,[0, neach-1], [col[1,0,colv[nn]], col[1,1,colv[nn]]]     ;green
            gg[ind0:ind0+neach-1] = dummy
            LINMAP, in_v, dummy,[0, neach-1], [col[2,0,colv[nn]], col[2,1,colv[nn]]]     ;blue
            bb[ind0:ind0+neach-1] = dummy

            ind0 += neach
        ENDFOR
        ind_mid = ind0
        ;map +ve colors     note that pale comes first here
        FOR nn=ncol/2, ncol-1 DO BEGIN
            ;                                 dimension signification
            ;                               [rgb,  dark pale, color]
            LINMAP, in_v, dummy,[0, neach-1], [col[0,1,colv[nn]], col[0,0,colv[nn]]]     ;red
            rr[ind0:ind0+neach-1] = dummy
            LINMAP, in_v, dummy,[0, neach-1], [col[1,1,colv[nn]], col[1,0,colv[nn]]]     ;green
            gg[ind0:ind0+neach-1] = dummy
            LINMAP, in_v, dummy,[0, neach-1], [col[2,1,colv[nn]], col[2,0,colv[nn]]]     ;blue
            bb[ind0:ind0+neach-1] = dummy

            ind0 += neach
        ENDFOR

        ;index 252 grey 100
        ;index 253 grey 220
        ;index 254 white
        ;index 255 black
        rr[252:255] = [100, 220, 255, 0]
        gg[252:255] = [100, 220, 255, 0]
        bb[252:255] = [100, 220, 255, 0]
        TVLCT, rr, gg, bb
        zero_val = 253
        index_nodata = 253
    ENDELSE
    index_pve = [ind_mid, ntot-1]  
    index_nve = [0., ind_mid-1]  
    index_lowhigh = [0, ntot-1]
    maptype = 'pve_nve'
    IF ~KEYWORD_SET(zerotol) THEN zerotol=1e-6
ENDIF
IF KEYWORD_SET(multi_col_low_high) THEN BEGIN          ;sort of rainbow appropriate for reflectivity
    IF N_ELEMENTS(colv) EQ 0 THEN colv = [0, 1, 2, 3, 4]
    ;colv speficies the number ald order of colors in the palette

    ;make red blue green vectors
    ncol = N_ELEMENTS(colv)     ;number of colors
    neach = FLOOR(251./ncol)    ;number of "degrade" for each color
    ntot = neach*ncol           ;index for last color
    ;initialize colors
    in_v = FINDGEN(neach)
    rr = INTARR(256)
    gg = INTARR(256)
    bb = INTARR(256)
    ind0= 0     ;start index for each color
    FOR nn=0, ncol-1 DO BEGIN
        ;                                 dimension signification
        ;                               [rgb,  dark pale, color]
        LINMAP, in_v, dummy,[0, neach-1], [col[0,1,colv[nn]], col[0,0,colv[nn]]]     ;red
        rr[ind0:ind0+neach-1] = dummy
        LINMAP, in_v, dummy,[0, neach-1], [col[1,1,colv[nn]], col[1,0,colv[nn]]]     ;green
        gg[ind0:ind0+neach-1] = dummy
        LINMAP, in_v, dummy,[0, neach-1], [col[2,1,colv[nn]], col[2,0,colv[nn]]]     ;blue
        bb[ind0:ind0+neach-1] = dummy

        ind0 += neach
    ENDFOR

    ;index 252 grey 100
    ;index 253 grey 230
    ;index 254 white
    ;index 255 black
    rr[252:255] = [100, 230, 255, 0]
    gg[252:255] = [100, 230, 255, 0]
    bb[252:255] = [100, 230, 255, 0]
        ;force first index to zero
        rr[0] = 255
        gg[0] = 255
        bb[0] = 255

    TVLCT, rr, gg, bb
    index_range = [0, ntot-1]
    index_lowhigh = [0, ntot-1]
    index_nodata = 253
    maptype = 'low_high'
ENDIF

IF KEYWORD_SET(any_white) THEN BEGIN          ;sort of rainbow appropriate for reflectivity
    IF N_ELEMENTS(colv) EQ 0 THEN colv = [0]
    ;default color is blue

    ;make red blue green vectors
    ncol = N_ELEMENTS(colv)     ;number of colors
    neach = FLOOR(251./ncol)    ;number of "degrade" for each color
    ntot = neach*ncol           ;index for last color
    ;initialize colors
    in_v = FINDGEN(neach)
    rr = INTARR(256)
    gg = INTARR(256)
    bb = INTARR(256)
    ind0= 0     ;start index for each color
    FOR nn=0, ncol-1 DO BEGIN
        ;                                 dimension signification
        ;                               [rgb,  dark pale, color]
        LINMAP, in_v, dummy,[0, neach-1], [255, col[0,0,colv[nn]]]     ;red
        rr[ind0:ind0+neach-1] = dummy
        LINMAP, in_v, dummy,[0, neach-1], [255, col[1,0,colv[nn]]]     ;green
        gg[ind0:ind0+neach-1] = dummy
        LINMAP, in_v, dummy,[0, neach-1], [255, col[2,0,colv[nn]]]     ;blue
        bb[ind0:ind0+neach-1] = dummy

        ind0 += neach
    ENDFOR

    ;index 252 grey 100
    ;index 253 grey 200
    ;index 254 white
    ;index 255 black
    rr[252:255] = [100, 200, 255, 0]
    gg[252:255] = [100, 200, 255, 0]
    bb[252:255] = [100, 200, 255, 0]

    TVLCT, rr, gg, bb
    index_range = [0, ntot-1]
    index_lowhigh = [0, ntot-1]
    index_nodata = 252
    maptype = 'low_high'
ENDIF



;do the mapping
IF maptype EQ 'low_high' THEN BEGIN              ;regular mapping from low to high
    ;map most values
    LINMAP, in, out, range, index_range
    ;low values
    aa = WHERE(in LE min, count)
    IF count NE 0 THEN out[aa] = index_lowhigh[0] 
    ;high values
    aa = WHERE(in GT max, count)
    IF count NE 0 THEN out[aa] = index_lowhigh[1] 
    ;nodata
    IF KEYWORD_SET(nodata) THEN BEGIN
    	aa = WHERE(in EQ nodata, count) 
    	IF count NE 0 THEN out[aa] = index_nodata 
    ENDIF
    ;NAN are always put to nodata color
    aa = WHERE(~FINITE(in), count)
    IF count NE 0 THEN out[aa] = index_nodata
ENDIF
IF maptype EQ 'pve_nve' THEN BEGIN              ;mapping for palettes that change at zero
    ;map neg values
    ii = WHERE(in GE range[0] AND in LT 0.-zerotol, count)
    IF count NE 0 THEN BEGIN
        maxv = MIN([range[1], 0.-zerotol])
        LINMAP, in[ii], temp_out, [range[0], maxv], index_nve
        out[ii] = temp_out
    ENDIF
    ;map pos values
    ii = WHERE(in GT zerotol AND in LE range[1], count)
    IF count NE 0 THEN BEGIN
        minv = MAX([range[0], zerotol])
        LINMAP, in[ii], temp_out, [minv, range[1]], index_pve
        out[ii] = temp_out
    ENDIF
    ;around zero
    ii = WHERE(in GT 0.-zerotol AND in LT 0.+zerotol, count)
    IF count NE 0 THEN out[ii] = zero_val    ;grey middle values nodata
    ;above below scale
    ii = WHERE(in GT range[1], count)
    IF count NE 0 THEN out[ii] = index_lowhigh[1]     ;grey out of bounds 
    ii = WHERE(in LT range[0], count)
    IF count NE 0 THEN out[ii] = index_lowhigh[0]  
    ;nodata
    IF KEYWORD_SET(nodata) THEN BEGIN
    	aa = WHERE(in EQ nodata, count) 
    	IF count NE 0 THEN out[aa] = index_nodata 
    ENDIF
ENDIF


IF KEYWORD_SET(palette) THEN BEGIN      ;keyword palette TV and plot palette at desired location
    IF ~KEYWORD_SET(unit) THEN unit='units not defined'
    ;plot palette
	TV, out, pos[0], pos[1],  XS=pos[2]-pos[0], YS=pos[3]-pos[1], /NORMAL
    ;draw square around it
    LOADCT, 40, /S
    PLOTS, [pos[0], pos[2], pos[2], pos[0], pos[0]], [pos[1], pos[1], pos[3], pos[3], pos[1]], /NORMAL, THICK=1.5, COL=0
    ;set up coords for axis call
	PLOT, [0,1],[0,1],POS=pos, /NODATA, /NOERASE, XSTYLE=4, YSTYLE=4
    AXIS, YAXIS=1, YTICKS=yticks, YTICKV=ytickv, YRANGE=range, YLOG=log_pve, YSTYLE=1,YTICKLEN=0.1, YTITLE=unit, $
          /NOERASE, YTICKFORMAT='(f0.2)'

    IF undefined EQ 0 THEN in = orig_in     ;restore variable if it was not undefined
ENDIF

;keyword tvout TV mapped variable directly
IF KEYWORD_SET(tvout) THEN TV, out, pos[0], pos[1],  XS=pos[2]-pos[0], YS=pos[3]-pos[1], /NORMAL

IF KEYWORD_SET(true) THEN BEGIN
    TVLCT, r, g, b, /GET
    red_chan = r[out]
    green_chan = g[out]
    blue_chan = b[out]
    out = [[[red_chan]], [[green_chan]], [[blue_chan]]]
ENDIF



END
