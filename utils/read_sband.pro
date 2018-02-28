;+
; READ_SBAND read McGill S-band files 
; 
; @PARAM work_direct The directory name of the file path
; @PARAM file_name The base name of teh file path
; @PARAM rv The structure ouput result
; @KEYWORD NRNG
; @KEYWORD NAZM
; @KEYWORD NELEV
; @KEYWORD DATE_TIME
; @KEYWORD TYPE
; @KEYWORD EXPAND_R
; @KEYWORD ELEVATION
; @KEYWORD NODATA Do not read data, only return header information
;
; @AUTHOR Marc Berenguer and Bernat Puigdomenech
;-
PRO READ_SBAND,work_direct, file_name, rv, NRNG=nrng, NAZM=nazm, NELEV=nelev, DATE_TIME=date_v, $
	TYPE=type, EXPAND_R=expnd, ELEVATION=elevation, NODATA=nodata, EXAMPLE=example, VERBOSE=verbose

  IF KEYWORD_SET(example) THEN BEGIN
    work_direct='/mnt/chuck-n/data/data/mcgill_sband/clean_cscans/20111020'
    file_name='ref_dopp_cscans_201110202039.dat'
  ENDIF

  info=FILE_INFO(FILEPATH(file_name, ROOT_DIR=work_direct))

  tt=SYSTIME(/SEC)

	CD,work_direct,CURRENT=old_dir

	IF NOT KEYWORD_SET(nrng) THEN nrng=180L
	IF NOT KEYWORD_SET(nazm) THEN nazm=360L
	
	IF N_ELEMENTS(expnd) EQ 0 THEN expnd=1

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
      
;			dum3 : INTARR(315), $
			
			tid_name : BYTARR(8,15), $
			pol_gre : 0, $ ;; 0 refstd 1: polgre
			readback_elvang : FLTARR(header1.nelev), $
			prf : FLTARR(header1.nelev), $ ;; prf of each elv for Nyquist computation sigmet
			majormode : FLTARR(header1.nelev), $ ;; major_mode fft, dula prt etc sigmet
			;; Introduced 2011 10 11
      seconds_beg : ULONARR(header1.nelev), $ ;; beg time in seconds and milli seconds from 0000GMT of each elevation
      milli_seconds_beg : UINTARR(header1.nelev), $
      azimuth_beg : UINTARR(header1.nelev), $ ;; beg azimuth of each elevation 20111011
      dum3 : INTARR(14), $
      ;; End of modifications
       
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
	
;	wwrng=WHERE(header2.max_rng NE 0,nwwrng)
;	IF nwwrng NE 0 THEN nrng=MAX(header2.max_rng[wwrng])
  
  ;; TODO some files contain dum_pad incorrect values
;	IF header2.dum_pad[1024] NE 0 THEN nrng=header2.dum_pad[1024]
	IF header1.nelev NE 0 THEN nelev=header1.nelev
			
  IF nelev EQ 0 THEN BEGIN
    FREE_LUN,num$un
    RETURN
  ENDIF
			
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
	
			CALDAT, time_ii, mt, dd, yy, hh, mm, ss
      time_str_ii=STRING(dd,FORMAT='(I2.2)')+'/'+STRING(mt,FORMAT='(I2.2)')+'/'+STRING(yy,FORMAT='(I0)')+' '+$
        STRING(hh,FORMAT='(I2.2)')+':'+STRING(mm,FORMAT='(I2.2)')

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

  time_u=time_v[UNIQ(time_v,SORT(time_v))]
  n_data=N_ELEMENTS(time_v) / N_ELEMENTS(time_u)
  
	IF N_ELEMENTS(date_v) EQ 0 THEN BEGIN
		ww=LINDGEN(N_ELEMENTS(time_v))
	ENDIF ELSE BEGIN	

		ww=[-1.]
		
    IF SIZE(date_v,/TYPE) NE 7 THEN date_vj=date_v $
    ELSE date_vj=JULDAY(STRMID(date_v,3,2),STRMID(date_v,0,2),STRMID(date_v,6,4),STRMID(date_v,11,2),STRMID(date_v,14,2),STRMID(date_v,17,2))
    
		FOR ii=0, N_ELEMENTS(date_vj)-1 DO BEGIN
			ww_ii=WHERE(ABS(time_v-date_vj[ii]) LT 1.e-8,nww_ii)
			IF nww_ii GT 0 THEN ww=[ww,ww_ii]
		ENDFOR
		
		IF N_ELEMENTS(ww) GT 1 THEN ww=ww[1:*] ELSE BEGIN
			IF KEYWORD_SET(verbose) THEN PRINT,'No volume scan corresponds to the solicited dates'
			FREE_LUN,num$un
			RETURN
		ENDELSE

	ENDELSE

	nww=N_ELEMENTS(ww)
	
	ww_v=[-1]
	
	kkaux=0

  data=BYTARR(nrng,nazm,nelev)
  
  IF KEYWORD_SET(expnd) AND nrng GT 120 THEN BEGIN
    uniform_data=1
    str_data=BYTARR(120+2*(nrng-120),nazm,nelev)
  ENDIF ELSE str_data=data
  
  IF N_ELEMENTS(elevation) NE 0 THEN BEGIN
    elev_ind=WHERE(header2.elev EQ elevation,pos)
    IF pos GT 0 THEN BEGIN
      elev_offset=elev_ind 
      str_data=str_data[*,*,0]
    ENDIF
  ENDIF 

  IF N_ELEMENTS(elev_offset) NE 0 THEN data_read=REFORM(data[*,*,0],nrng,nazm,1) $
  ELSE data_read=data

  str_gill={ header1: header1, header2: header2 }
  IF ~KEYWORD_SET(nodata) THEN str_gill=CREATE_STRUCT(str_gill, 'z_data', str_data)
  
  ;; 2 headers and data [range,azimuth,elev] (size in bytes)
  lun_pos=(4096.+FLOAT(nrng)*360.*FLOAT(nelev))

  ;; Check the file size
;  WHILE info.SIZE LT nww*lun_pos DO nww--
  
  IF KEYWORD_SET(type) THEN n_rep=(nww / n_data)*N_ELEMENTS(type) $
  ELSE n_rep=nww

  rv=REPLICATE(str_gill,n_rep)
  
	FOR kk=0,nww-1 DO BEGIN
		ctrl=0

		POINT_LUN,num$un,lun_pos*ww[kk]
		
		READU,num$un,header1
		IF KEYWORD_SET(sw) THEN $
		  header1=SWAP_ENDIAN(header1)

		READU,num$un,header2
		IF KEYWORD_SET(sw) THEN $
		  header2=SWAP_ENDIAN(header2)
		
		IF KEYWORD_SET(type) THEN IF TOTAL(type EQ header2.met_p) EQ 0 THEN ctrl=-1

		IF (nrng ne 0 AND nazm ne 0 AND nelev ne 0 AND ctrl EQ 0) THEN BEGIN
		
			ww_v=[ww_v,ww[kk]]
			
			IF ~KEYWORD_SET(nodata) THEN BEGIN
			
        IF KEYWORD_SET(elev_offset) THEN $
			    SKIP_LUN, num$un, FLOAT(nrng)*360*elev_offset
        
        READU, num$un,data_read
        
  			IF KEYWORD_SET(sw) THEN $
  			  data_read=SWAP_ENDIAN(data_read)
  
  			IF KEYWORD_SET(uniform_data) THEN BEGIN
          z_data=str_data
          IF SIZE(z_data,/N_DIM) NE 3 THEN $
            z_data=REFORM(z_data,[SIZE(z_data,/DIM),1])
  				z_data[0:119,*,*]=data_read[0:119,*,*]
  				index=120+2*FINDGEN(nrng-120)
  				z_data[index,*,*]=data_read[120:*,*,*]
  				z_data[index+1,*,*]=data_read[120:*,*,*]			
  			ENDIF ELSE z_data=data_read
  	  
  	    rv[kkaux].z_data=REFORM(z_data)
  	  ENDIF
  	  
  	  rv[kkaux].header1=header1
      rv[kkaux].header2=header2
          
  	  kkaux=kkaux+1
		ENDIF
	ENDFOR

  IF kkaux GT 0 AND kkaux NE nww THEN rv=rv[0:kkaux-1]
  
  IF N_ELEMENTS(date_v) EQ 0 THEN BEGIN
    IF N_ELEMENTS(ww_v) GT 1 THEN BEGIN
      CALDAT, time_v[ww_v[1:*]], mt, dd, yy, hh, mm, ss
      date_v=STRING(dd,FORMAT='(I2.2)')+'/'+STRING(mt,FORMAT='(I2.2)')+'/'+STRING(yy,FORMAT='(I0)')+' '+$
        STRING(hh,FORMAT='(I2.2)')+':'+STRING(mm,FORMAT='(I2.2)')+':'+STRING(ss,FORMAT='(I2.2)')
    ENDIF ELSE date_v=''
  ENDIF
	
	FREE_LUN,num$un
	
	IF KEYWORD_SET(verbose) THEN BEGIN
	  PRINT, 'read time ', SYSTIME(/SEC)-tt
    PRINT, '-------------------------------------'
  ENDIF
END
