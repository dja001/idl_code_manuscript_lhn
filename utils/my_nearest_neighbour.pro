PRO MY_NEAREST_NEIGHBOUR,z,x,y,znn,nx,ny,NODATA=nodata,EXAMPLE=example,XRANGE=xr, $
	YRANGE=yr,XRED=xred,YRED=yred,GRID_2=grid_2, TOLV=tol_v

;z: data to be interpolated
;x: x coordinates of z  (1-D vector)
;y: y coordinates of z  (1-D vector)
;znn: output array (interpolated z)
;nx: # of columns of the output array (by default, 800)
;ny: # of rows of the output array (by default, 600)
;xr,yr: range of the output array. By default, it is set to the range of x and y
;
;original code by Marc Berenguer

IF KEYWORD_SET(example) THEN BEGIN
	DEVICE,DEC=0
	headername='/data/berenguer/uhf_profiler/1992/h92152a.spc'
	filename='/data/berenguer/uhf_profiler/1992/d92152a.spc'

	headername='/data/berenguer/uhf_profiler/2003/H03152A.SPC'
	filename='/data/berenguer/uhf_profiler/2003/D03152A.SPC'

	xr=[0.,1440]
	xr=[480.,720]
	yr=[0,10000.]
	nx=1440
	ny=250

	READ_SPC,headername,filename,str_header_v,spc_struct_v,NODATA=-9999.

	z=spc_struct_v.reflectivity

	sz=SIZE(z)
	nx0=sz[1]
	ny0=sz[2]

	IF ~KEYWORD_SET(grid_2) THEN BEGIN
		x=REBIN(1440.*(JULDAY_STR(str_header_v.time_str)-JULDAY_STR(str_header_v[0].time_str)),nx0,ny0)
		y=spc_struct_v.height
	ENDIF ELSE BEGIN
		x=1440.*(JULDAY_STR(str_header_v.time_str)-JULDAY_STR(str_header_v[0].time_str))
		y=REFORM(spc_struct_v[0,*].height,N_ELEMENTS(spc_struct_v[0,*]))
	ENDELSE
	
	coords_orig=[0.,0.]

ENDIF

tt1=SYSTIME(/SEC)

IF ~KEYWORD_SET(xr) THEN xr=[MIN(x),MAX(x)]
IF ~KEYWORD_SET(yr) THEN yr=[MIN(y),MAX(y)]
IF ~KEYWORD_SET(nodata) THEN nodata=0.0
IF ~KEYWORD_SET(nx) THEN nx=800
IF ~KEYWORD_SET(ny) THEN ny=600

resol=[(xr[1]-xr[0])/nx,(yr[1]-yr[0])/ny]
xcell=LONG((x-xr[0])/resol[0])
ycell=LONG((y-yr[0])/resol[1])

dx=(xr[1]-xr[0])/nx
dy=(yr[1]-yr[0])/ny
xred=xr[0]+dx*(.5+FINDGEN(nx))
yred=yr[0]+dy*(.5+FINDGEN(1,ny))
znn=REPLICATE(nodata,nx,ny)
IF ~KEYWORD_SET(tol_v) THEN BEGIN
	szx=SIZE(x)
	nnx=szx[1]
	szy=SIZE(y)
	IF szy[0] EQ 1 THEN nny=szy[1] ELSE nny=szy[2]

	wwx=WHERE(x NE nodata)
	wwy=WHERE(y NE nodata)

	tolx=MAX(x[wwx])-MIN(x[wwx])/nnx
	toly=MAX(y[wwy])-MIN(y[wwy])/nny
	tol_v=[tolx,toly]
ENDIF

IF tol_v[0] LT 0 THEN BEGIN

	szx=SIZE(x)
	nnx=szx[1]

	wwx=WHERE(x NE nodata)

	tolx=(MAX(x[wwx])-MIN(x[wwx]))/nnx

	tol_v[0]=tolx
ENDIF

IF tol_v[1] LT 0 THEN BEGIN

	szy=SIZE(y)
	IF szy[0] EQ 1 THEN nny=szy[1] ELSE nny=sz[2]

	wwy=WHERE(y NE nodata)

	toly=(MAX(y[wwy])-MIN(y[wwy]))/nny
	tol_v[1]=toly
ENDIF

IF KEYWORD_SET(grid_2) THEN BEGIN
	pos_x=REPLICATE(-1,nx)
	FOR ii=0,nx-1 DO BEGIN
		difx=ABS(xred[ii]-x)
		mmx=MIN(difx)
		IF mmx LT tol_v[0] THEN pos_x[ii]=!C
	ENDFOR
	ww_posx=WHERE(pos_x NE -1,nww_posx)

	IF nww_posx GT 0 THEN BEGIN
		FOR ii=0,ny-1 DO BEGIN
			dify=ABS(yred[ii]-y)
			mmy=MIN(dify)
			IF mmy LT tol_v[1] THEN BEGIN
				wwy=!C
				znn[ww_posx,ii]=z[pos_x[ww_posx],wwy[0]]
			ENDIF

		ENDFOR
	ENDIF
ENDIF ELSE BEGIN
	ff_aux=FINDGEN(N_ELEMENTS(x[0,*]))

	FOR ii=0,nx-1 DO BEGIN
		difx=ABS(xred[ii]-x[*,0])
		mmx=MIN(difx)
		IF mmx LT tol_v[0] THEN BEGIN
			wwx=!C
			ymax=y[wwx[0],MAX(ff_aux*(y[wwx,*] NE nodata))]
			FOR jj=0,ny-1 DO BEGIN
				dify=ABS(yred[jj]-y[wwx[0],*])
				mmy=MIN(dify)

				IF mmy LT tol_v[1] THEN BEGIN
					wwy=!C
					znn[ii,jj]=z[wwx[0],wwy[0]]
				ENDIF
				IF yred[jj]-ymax GT tol_v[1] THEN BREAK
			ENDFOR
		ENDIF
	ENDFOR

ENDELSE

tt2=SYSTIME(/SEC)
;PRINT,tt2-tt1

;ww=WHERE(xcell GE 0 AND xcell LT nx AND ycell GE 0 AND ycell LT ny,nww)
;IF nww GT 0 THEN BEGIN
;	cell=xcell[ww]+ycell[ww]*FLOAT(nx)
;	znn[cell]=z[ww]
;ENDIF

IF KEYWORD_SET(example) THEN STOP

END
