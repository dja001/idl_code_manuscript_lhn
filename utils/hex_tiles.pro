PRO HEX_TILES, r, n_tiles, center_tile, xin, yin, xout, yout, $
               XY_TO_HEX=xy_to_hex, HEX_TO_XY=hex_to_xy,      $
               BADPTMASK=badptmask, NBADPTS=nbadpts, GOODPTMASK=goodptmask, NGOODPTS=ngoodpts 
    ;hexagonal tiles
    ;does the mapping from cartesian pts and hexagonal points and conversely
    ;xin and yin are in the same unit as s
    ;in other words, they can be but are not limited to array indexes
    
    ;r = center-side distance of a line perpendicular to a side     2r = width of hexagonal tile
    ;[xy]in= input coordinates
    ;[xy]out= output coordinates

    ;height of triangle section
    h = r/SQRT(3)
    s = 2.*h
    onethird = 1./3.
    twothird = 2./3.
    half = .5

    ;initialize output matrices
    sz = SIZE(xin, /DIM)
    xout = FLTARR(sz)
    yout = FLTARR(sz)

    IF (center_tile[0] LT 0 ) OR (center_tile[0] GT n_tiles[0]-1) OR $
       (center_tile[1] LT 0 ) OR (center_tile[1] GT n_tiles[1]-1)  THEN BEGIN  
       print, 'precedure hex_tiles'
       print, 'center must be somewhere inside the domain'
       stop
    ENDIF


    IF KEYWORD_SET(xy_to_hex) THEN BEGIN
        ;transform cartesian coords into hexagonal tile coodinate

        ;deal with offset centers
        IF TOTAL(center_tile) GT 0 THEN BEGIN           ;if center is not at 0,0
            y0 = (3.*h*center_tile[1] )     ;y distance from centre to bottom left corner
            yin_offset = yin + y0                 ;shift distance  for 0 distance at center
            x0 = (2.*r*center_tile[0] + r*(center_tile[1] MOD 2))
            xin_offset = xin + x0   
        ENDIF ELSE BEGIN
            yin_offset = yin
            xin_offset = xin
        ENDELSE


        ;add offset to avoid -ve in bottom and left edge
        yin_offset += 6.*h      ;shift by two tiles to keep validity of odd/even row rules
        xin_offset += 2.*r

        ;print, xin_offset[*,200]
        ;stop



        ;normalized distance from bottom corner
        yin_norm = yin_offset/(3.*h)
        absmody = ABS( yin_norm MOD 1. ) 

        ;search for which parts are in the rectangles which part in the triangles
        rec = WHERE( (absmody LE onethird) OR (absmody GE twothird) , nrec, $
                                            NCOMPLEMENT=ntrig, COMPLEMENT=trig)

        IF nrec NE 0 THEN BEGIN
            ;we deal with rectangle decomposition
            ;yindexes
            yout[rec] = ROUND( (yin_offset[rec])/(3.*h) )

            ;for xindices it depends if we are an even or odd row
            ;even rows
            indent = r*ABS(yout[rec] MOD 2.)      ; indent = r for odd rows      indent = 0 for even rows
            xout[rec] = ROUND( (xin_offset[rec] - indent)/(2.*r) )
        ENDIF
        IF ntrig NE 0 THEN BEGIN
            ;we deal with triangle decomposition
            ;decompose in left and right section
            absmodx = ABS( (xin_offset[trig]/(2.*r)) MOD 1.)

            yout[trig] = FLOOR(yin_norm[trig])      
            indent = r*ABS(yout[trig] MOD 2.)       ; indent = r for odd rows      indent = 0 for even rows
            xout[trig] = ROUND( (xin_offset[trig] - indent)/(2.*r) )

            even = (yout[trig] + 1) MOD 2
            yadd_e = even*( (absmody[trig] GT (absmodx*twothird)) AND (absmodx GE half) )
            xsub_e = yadd_e 
            yadd_e = yadd_e OR even*( (absmody[trig] GE (absmodx*(-1.*twothird) + twothird)) AND (absmodx LT half) )

            odd =  (yout[trig] ) MOD 2
            yadd_o = odd*( (absmody[trig] GE (absmodx*(-1.*twothird) + 1.)) AND (absmodx GE half) )
            xadd_o = yadd_o 
            yadd_o = yadd_o OR odd*( (absmody[trig] GT (absmodx*twothird + onethird)) AND (absmodx LT half) )

            yout[trig] += (yadd_e + yadd_o)
            xout[trig] -= xsub_e
            xout[trig] += xadd_o

        ENDIF


        ;remove one index to replace left and bottom edge
        yout -= 2
        xout -= 1
        ;make mask for invalid indexes
        badptmask = WHERE( (xout LT 0 ) OR (xout GT n_tiles[0]-1) OR           $
                           (yout LT 0 ) OR (yout GT n_tiles[1]-1), nbadpts, NCOMPLEMENT=ngoodpts, COMPLEMENT=goodptmask)  
        IF ngoodpts EQ 0 THEN BEGIN
            print, 'no good mapping point(s) were found '
            print, 'its likely that all distance requested are outside of tile matrix values'
            stop
        ENDIF
        ;set bad indexes to 0 to avoid problemsn
        IF nbadpts NE 0 THEN BEGIN
            xout[badptmask] = 0
            yout[badptmask] = 0
        ENDIF


    ENDIF
    IF KEYWORD_SET(hex_to_xy) THEN BEGIN

        ;given index coordinates of tile matrix, give x and y distance from bottom left corner
        yout = 3.*h*yin
        xout = 2.*r*xin + r*(yin MOD 2)        ;add r to odd rows

        ;deal with offset centers
        IF TOTAL(center_tile) GT 0 THEN BEGIN           ;if center is not at 0,0
            y0 = (3.*h*center_tile[1] )     ;y distance from centre to bottom left corner
            yout  -= y0                     ;shift distance  for 0 distance at center
            x0 = (2.*r*center_tile[0] + r*(center_tile[1] MOD 2))
            xout -= x0   
        ENDIF
    ENDIF
END

PRO hex_example, EX=ex
    ;shows how to use the hex tiles procedure
    ;set EX to number of example desired

    IF ex EQ 1 THEN BEGIN
        ;1 plot an array of values in hex coordinates

        ;array of value in an array representing hexagonal shapes
        n = 5
        hexside = 10.   ;size of the side of one hexagonal tile 
        n_tiles = [n,n]
        center_tile = [0,0]
        tile_matrix = RANDOMU(sed, n, n)*255

        nout = 400
        pos_vect = FINDGEN(nout)/(nout-1)*(2.*n*hexside)    ;position vector
        xx = REBIN(pos_vect, nout, nout)                    ;x and y position of desired output
        yy = TRANSPOSE(xx)

        HEX_TILES, hexside, n_tiles, center_tile, xx, yy, xout, yout, /xy_to_hex, BADPTMASK=BADPTMASK      ;get tile_matrix index for each xx,yy position
        img = tile_matrix[xout, yout]   ;do the mapping:  sample tile_matrix for each position in xx yy
        img[badptmask] = 255            ;map nodata pts to background color

        LOADCT, 40, /S
        TV, img, 50, 50, XS=400, YS=400
        LOADCT, 39, /S
        PLOT, [0], [0], /NODATA, XR=[0, xx[nout-1,0]], YR=[0, yy[0,nout-1]], XS=1, YS=1, COL=255, /NOERASE, POS=[50, 50, 450, 450], /DEVICE
    ENDIF


    IF ex EQ 2 THEN BEGIN
        ;2  displays a color graph where color of tile represents its distance from the center
        ;this time the center tile will not be in the bottom left corner

        ;array of value in an array representing hexagonal shapes
        hexside = 5.
        n_tiles = [7, 7]               ;odd numbers for centered stuff   ;  non-square tile matrix just for fun
        center_tile = [1, 1]            ;  index of center tile    center also not quite in the middle just to demonstrate

        ;I want x-y distance for every tile to the center tile
        ;x and y index of every point in the tile array:
        xind = REBIN(FINDGEN(n_tiles[0]), n_tiles[0], n_tiles[1])
        yind = REBIN(TRANSPOSE(FINDGEN(n_tiles[1])), n_tiles[0], n_tiles[1])

        HEX_TILES, hexside, n_tiles, center_tile, xind, yind, xdist, ydist, /hex_to_xy      ;get x-y position of tiles with respect to center

        ;distance from center of each tiles
        dist_from_center = SQRT(xdist^2. + ydist^2.)

        print, dist_from_center

        ;ploting output
        nout = 401      ;odd number for center in exactly the center
        pos_vect = (FINDGEN(nout)/(nout-1) )*(2.*hexside*20) -(2.*hexside*7)    ;position vector of image  this time some positions are -ve with distance 0 being the center tile
        xx = REBIN(pos_vect, nout, nout)          ;x and y position of pixels in output matrix
        yy = TRANSPOSE(xx)

        HEX_TILES, hexside, n_tiles, center_tile, xx, yy, xout, yout, /xy_to_hex, BADPTMASK=BADPTMASK     ;get tile_matrix index for each xx,yy position

        img_val = dist_from_center[xout, yout]   ;do the mapping:  sample tile_matrix for each position in xx yy in the image
        COLMAP_BW, img_val, img_ind, [0., MAX(img_val)]
        img_ind[badptmask] = 255

        TV, img_ind, 50, 50, XS=400, YS=400
        LOADCT, 39, /S
        PLOT, [0], [0], /NODATA, XR=[xx[0,0], xx[nout-1,0]], YR=[yy[0,0], yy[0,nout-1]], XS=1, YS=1, COL=255, /NOERASE, POS=[50, 50, 450, 450], /DEVICE
        OPLOT, [0., 0], [yy[0,0], yy[0,nout-1]], COL=0
        OPLOT, [xx[0,0], xx[nout-1,0]], [0., 0], COL=0

    ENDIF

END
