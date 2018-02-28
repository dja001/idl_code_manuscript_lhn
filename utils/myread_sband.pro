PRO MYREAD_SBAND,work_direct,file_name,rv, $
	NRNG=nrng,NAZM=nazm,NELEV=nelev,DATE_TIME=date_v, $
	TYPE=type,EXPAND_R=expnd,NODATA=nodata,EXAMPLE=example

  tt1=SYSTIME(/SEC)

  IF KEYWORD_SET(example) THEN BEGIN
    work_direct='~/data/s-band/'
    file_name='Z_Vr_Zdr_rhohv_cscans_0604_1759_31aug05.dat'
  ENDIF

;  info=FILE_INFO(FILEPATH(file_name, ROOT_DIR=work_direct))

	CD,work_direct,CURRENT=old_dir

	IF NOT KEYWORD_SET(nrng) THEN nrng=180L
	IF NOT KEYWORD_SET(nazm) THEN nazm=360L
	
	IF N_ELEMENTS(expnd) EQ 0 THEN expnd=1

	help, file_name
	OPENR,num$un,file_name,/GET_LUN;,/COMPRESS
	CD,old_dir

	header1={dum0 : INTARR(46), $
			nlogical_rec : 0, $
			dum1 : INTARR(3), $
			vol_scan_fmt : 0, $
			dum2 : INTARR(5), $
			time : LONARR(7), $
			lat : 0., $
			lon : 0., $
			nelev : 0L}
			
	READU,num$un,header1
		
	IF header1.nelev[0] GT 50 THEN BEGIN
		header1=SWAP_ENDIAN(header1)
		sw=1
	ENDIF

	header2={elev : FLTARR(header1.nelev), $
			azimut_off : 0, $
			viraq_flag : 0, $
			clutt_filter : 0, $
			dum3 : INTARR(315), $
			met_p : 0, $
			dum4 : 0, $
			dbz_offset : 0., $
			cal_slp : 0., $
			antenna_pr : 0, $
			dum56 : INTARR(2), $
			cscan_fmt : 0, $
			rng_unfolded : 0, $
			vad_vlc_unfolded : 0, $
			n_vad_unf_pts : INTARR(header1.nelev), $
			n_rng_unf_pts : INTARR(header1.nelev), $
			rng_bins_arr_sz : 0, $
			dum7 : 0, $
			shft_csc_flg : 0, $
			shft_sp_dir : INTARR(2), $
			dum8 : FLTARR(header1.nelev), $
			dum9 : FLTARR(header1.nelev), $
			vert_grd_unfolded : 0, $
			n_vert_grd_unf_pts : INTARR(header1.nelev), $
			dum10 : 0, $
			dum11 : 0L, $
			dum12 : 0., $
			rad_grd_unfolded : 0, $
			ep1 : 0, $
			n_rad_grd_unf_pts : INTARR(header1.nelev), $
			prf1 : INTARR(header1.nelev), $
			prf2 : INTARR(header1.nelev), $	
			nyq_rng : INTARR(header1.nelev), $
			max_rng : INTARR(header1.nelev), $
			ep2 : 0,$
			nyq_vel : FLTARR(header1.nelev), $
			max_vel : FLTARR(header1.nelev), $
			usb_vel : BYTARR(header1.nelev), $
			prev_sub_area_spd : INTARR(10), $
			prev_sub_area_dir : INTARR(10), $
			dum_pad : INTARR(1164)}
				
				
	READU,num$un,header2
	IF KEYWORD_SET(sw) THEN BEGIN
		header2=SWAP_ENDIAN(header2)
	ENDIF
	
;	PRINT,header2.dbz_offset
	
;	wwrng=WHERE(header2.max_rng NE 0,nwwrng)
;	IF nwwrng NE 0 THEN nrng=MAX(header2.max_rng[wwrng])
	IF header2.dum_pad[1024] NE 0 THEN nrng=header2.dum_pad[1024]
	IF header1.nelev NE 0 THEN nelev=header1.nelev
			
	i=0

	POINT_LUN,num$un,0
	
	WHILE NOT EOF(num$un) AND N_ELEMENTS(ii) EQ 0 DO BEGIN

		IF EOF(num$un) THEN BREAK
		READU,num$un,header1

		IF header1.nelev[0] GT 50 THEN BEGIN
			header1=SWAP_ENDIAN(header1)
			sw=1
		ENDIF
		
		IF header1.nelev[0] NE 0 THEN BEGIN
			time_ii=JULDAY(header1.time[4],header1.time[3],header1.time[5], $
				header1.time[0],header1.time[1],header1.time[2])	
	
			;CALDAT_STR,time_ii,time_str_ii

			IF NOT KEYWORD_SET(time_v) THEN BEGIN
				time_v=time_ii
			ENDIF ELSE BEGIN
				time_v=[time_v,time_ii]
			ENDELSE
			
;			rngelev=TOTAL(header2.max_rng)
;			rngelev=MAX(header2.max_rng)*DOUBLE(nelev)
			rngelev=DOUBLE(nrng)*DOUBLE(nelev)

		ENDIF ELSE BREAK

		i=i+1

		POINT_LUN,num$un,(4096d +360d *rngelev)*i
		IF EOF(num$un) THEN BREAK
		
	ENDWHILE

	ss=SORT(time_v)
	uu=UNIQ(time_v[ss])
	timeu=time_v[ss[uu]]
	;CALDAT_STR,timeu,timeu_str

	nn=N_ELEMENTS(timeu)
	
	IF NOT KEYWORD_SET(date_v) THEN BEGIN
		ww=FINDGEN(N_ELEMENTS(time_v))
	ENDIF ELSE BEGIN	

		ww=[-1.]

		date_vj=JULDAY_STR(date_v)

		FOR ii=0,N_ELEMENTS(date_v)-1 DO BEGIN
			ww_ii=WHERE(ABS(time_v-date_vj[ii]) LT 1.e-8,nww_ii)
			IF nww_ii GT 0 THEN ww=[ww,ww_ii]
		ENDFOR
		
		IF N_ELEMENTS(ww) GT 1 THEN ww=ww[1:*] ELSE BEGIN
			PRINT,'No volume corresponds to the sollicited dates'
			CLOSE,num$un
			FREE_LUN,num$un
			RETURN
		ENDELSE

	ENDELSE

	nww=N_ELEMENTS(ww)
	
	ww_v=[-1]
	
	kkaux=0

  data=BYTARR(nrng,nazm,nelev)
  
  IF KEYWORD_SET(expnd) AND nrng GT 120 THEN str_data=BYTARR(120+2*(nrng-120),nazm,nelev) $
  ELSE str_data=data
  
  str_gill={ header1 : header1, header2 : header2, z_data : str_data}

  rv=REPLICATE(str_gill,nww)

	FOR kk=0,nww-1 DO BEGIN
		ctrl=0

		POINT_LUN,num$un,(4096.+FLOAT(nrng)*360.*FLOAT(nelev))*ww[kk]

		READU,num$un,header1

		IF KEYWORD_SET(sw) THEN BEGIN
			header1=SWAP_ENDIAN(header1)
		ENDIF

		IF nelev EQ 0 THEN BEGIN
			CLOSE,num$un
			FREE_LUN,num$un
			RETURN
		ENDIF

		READU,num$un,header2
		
		IF KEYWORD_SET(sw) THEN BEGIN
			header2=SWAP_ENDIAN(header2)
		ENDIF
		
		IF KEYWORD_SET(type) THEN BEGIN
			IF TOTAL(type EQ header2.met_p) EQ 0 THEN ctrl=-1
		ENDIF

		IF (nrng ne 0 AND nazm ne 0 AND nelev ne 0 AND ctrl EQ 0) THEN BEGIN
		
			ww_v=[ww_v,ww[kk]]
			
			IF NOT KEYWORD_SET(nodata) THEN BEGIN

				data[*]=0

				READU,num$un,data

				IF KEYWORD_SET(sw) THEN BEGIN
					data=SWAP_ENDIAN(data)
				ENDIF

				IF KEYWORD_SET(expnd) AND nrng GT 120 THEN BEGIN
					z_data=BYTARR(120+2*(nrng-120),nazm,nelev)
					z_data[0:119,*,*]=data[0:119,*,*]
					index=120+2*FINDGEN(nrng-120)
					z_data[index,*,*]=data[120:*,*,*]
					z_data[index+1,*,*]=data[120:*,*,*]			
				ENDIF ELSE z_data=data
	
				rv[kkaux].header1=header1
	      			rv[kkaux].header2=header2
	      			rv[kkaux].z_data=z_data
	
				kkaux=kkaux+1
			ENDIF
		ENDIF
	ENDFOR

  	IF kkaux GT 0 AND kkaux NE nww THEN rv=rv[0:kkaux-1]
  
	IF N_ELEMENTS(ww_v) GT 1 THEN BEGIN
		ww_v=ww_v[1:*]
		;CALDAT_STR,time_v[ww_v],date_v
	ENDIF ELSE date_v=''
	
	CLOSE,num$un
	FREE_LUN,num$un
	
	PRINT, 'Read s-band time ', SYSTIME(/SEC)-tt1
	
	IF KEYWORD_SET(example) THEN STOP

END
