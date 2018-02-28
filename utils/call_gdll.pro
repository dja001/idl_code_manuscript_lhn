PRO call_gdll, filename, var, gem_lat, gem_lon
;takes in a standard file, and retunr lat and lon at every grid points 
;for a given variable

;output:  gem_lat, gem_lon      grids of lat and lon

;test that filename exist and is a regular file
res = FILE_INFO(filename)
IF res.exists EQ 1 THEN BEGIN
    IF res.regular NE 1 THEN BEGIN

    MESSAGE, 'File:  '+filename+'    must be a regular file'
    ENDIF
ENDIF ELSE BEGIN
    MESSAGE, 'File:  '+filename+'    does not exist'
ENDELSE

;get ni and nj to initialize gem_lat and gem_lon
unit=101
file_id=FSTOUV(UNIT=unit,FILE=filename)
IF file_id LT 0 THEN MESSAGE, 'Problem with fstouv'
ref=FSTINF(U=unit,N=var)
IF ref LT 0 THEN MESSAGE, 'Problem getting info on variable: '+var
prm=FSTPRM(R=ref)
ni = FIX(prm.ni)
nj = FIX(prm.nj)
stat = FSTFRM(U=unit)

;length of variable name
varlen = STRLEN(var)
;convert var to a standard 8 character string
dummy = '        '
var = STRMID(var+dummy,0,8)

;initialize lat and lon grids
gem_lat = FLTARR(ni,nj)
gem_lon = FLTARR(ni,nj)

;define and check fortran lib file
shared_obj = '/users/dor/arma/dja/fortran/gdll/f_call_gdll.so'
IF (FILE_TEST(shared_obj) NE 1) THEN MESSAGE, "shared_obj does not exist"

;call to external fortran library
bfilename = BYTARR(500)
name = BYTE(filename)
bfilename[0:N_ELEMENTS(name)-1] = name
bvar = BYTE(var)
stat = CALL_EXTERNAL(shared_obj, 'f_call_gdll_', $     ;call to fortran library wrapper
       ni, nj, bfilename, bvar, varlen,          $     ;input 
       gem_lat, gem_lon, /UNLOAD)                      ;output 

END
