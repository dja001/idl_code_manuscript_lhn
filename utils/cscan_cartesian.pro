PRO CSCAN_CARTESIAN, data_polar, data_cartesian, MAX_DISTANCE=max_distance, SZ_FACTOR=sz_factor, NODATA=nodata

  ON_ERROR, 2
  
  IF N_ELEMENTS(data_polar) EQ 0 THEN MESSAGE, 'Incorrect number of arguments'

  IF N_ELEMENTS(sz_factor) EQ 0 THEN sz_factor=1L
  IF N_ELEMENTS(max_distance) EQ 0 THEN max_distance=120.

  IF N_ELEMENTS(nodata) EQ 0 THEN nodata=-9999.

  sz=SIZE(data_polar,/DIM)

; dim=sz[0]*sz_factor
  dim=max_distance*sz_factor

  xx=FINDGEN(dim)-dim/2
  yy=FINDGEN(dim)-dim/2

  xx=REBIN(xx,dim,dim,/SAMPLE)
  yy=TRANSPOSE(REBIN(yy,dim,dim,/SAMPLE))
  
  ycoords=ROUND(SQRT(xx^2.+yy^2.))
  ycoords=ycoords*max_distance/(dim/2-1)

  xcoords=ATAN(xx/yy)*!RADEG
  xcoords=ROTATE(xcoords,1)+90.
  xcoords[0:dim/2-1,*]=xcoords[0:dim/2-1,*]+180.

  xcoords=0 > ROUND(xcoords) < (sz[1]-1)

  ;; We lose some data
;  mm=XUNIQ(ycoords + sz[0]*xcoords)
;  tt=PRODUCT(sz)

  data_cartesian=data_polar[ycoords,xcoords]
  
  ww=WHERE(ycoords GT max_distance,nn_w)
  IF nn_w GT 0 THEN data_cartesian[ww]=nodata
END