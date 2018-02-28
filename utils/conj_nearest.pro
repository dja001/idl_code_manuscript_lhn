pro refine, xpos, ypos
;given a CJ best estimate of nearest point, search neighbor to insure a neighbor is not closer
common share1, lat0, lon0, lat_rad, lon_rad, nxo, nyo

;number of pixels to perform search eg:
;o--o--o  
;o--x--o  -> dsq=1
;o--o--o 
;
;o--o--o--o--o  
;o--o--o--o--o  
;o--o--x--o--o  -> dsq=2
;o--o--o--o--o 
;o--o--o--o--o 
;
dsq = 1
x0 = xpos-dsq
xf = xpos+dsq
y0 = ypos-dsq
yf = ypos+dsq

;insure we do not exceed bounds
if x0 lt 0     then x0 = 0
if xf gt nxo-1 then xf = nxo-1
if y0 lt 0     then y0 = 0
if yf gt nyo-1 then yf = nyo-1

;if a neighboring point is smaller, this becomes the minimum
minv = lat_lon_dg([xpos,ypos])
for ii=x0,xf do begin
    for jj=y0,yf do begin
        if ii eq xpos and jj eq ypos then continue
        dd = lat_lon_dg([ii,jj])
        if dd lt minv then begin
            ;print, 'refined', xpos, ypos, ' to ', ii,jj
            minv = dd
            xpos = ii
            ypos = jj
        endif
    endfor
endfor

end



function lat_lon_dg, coords, gradient
;computes distance of great circle on the sphere and, optionally, the gradient for finding the minimum distance
common share1, lat0, lon0, lat_rad, lon_rad, nxo, nyo

xpos = coords[0]
ypos = coords[1]
;print, 'funct', xpos, ypos

;insure coords are within array
if xpos lt 0     then xpos=0
if xpos gt nxo-1 then xpos=nxo-1
if ypos lt 0     then ypos=0
if ypos gt nyo-1 then ypos=nyo-1

;if gradient variable supplied compute x and y derivatives
if arg_present(gradient) then begin
    gradient = fltarr(2)
    ;X partial derivative
    case coords[0] of
        0   :begin
                ;forward difference
                gcirc,0,lon0,lat0,lon_rad[xpos  ,ypos],lat_rad[xpos  ,ypos], d_xplus0                         
                gcirc,0,lon0,lat0,lon_rad[xpos+1,ypos],lat_rad[xpos+1,ypos], d_xplus1                         
                gcirc,0,lon0,lat0,lon_rad[xpos+2,ypos],lat_rad[xpos+2,ypos], d_xplus2                         
                gradient[0] = -1.5*d_xplus0[0] + 2.*d_xplus1[0] - .5*d_xplus2[0]
             end
        nxo-1:begin
                ;backward difference
                gcirc,0,lon0,lat0,lon_rad[xpos  ,ypos],lat_rad[xpos  ,ypos],d_xminu0                         
                gcirc,0,lon0,lat0,lon_rad[xpos-1,ypos],lat_rad[xpos-1,ypos],d_xminu1                         
                gcirc,0,lon0,lat0,lon_rad[xpos-2,ypos],lat_rad[xpos-2,ypos],d_xminu2                         
                gradient[0] =  1.5*d_xminu0[0] - 2.*d_xminu1[0] + .5*d_xminu2[0]
             end
        else:begin
                ;centered difference
                gcirc,0,lon0,lat0,lon_rad[xpos+1,ypos],lat_rad[xpos+1,ypos],d_xplus1                         
                gcirc,0,lon0,lat0,lon_rad[xpos-1,ypos],lat_rad[xpos-1,ypos],d_xminu1                         
                gradient[0] = -.5*d_xminu1[0] + .5*d_xplus1[0]
             end
    endcase
    ;Y partial derivative
    case coords[1] of
        0   :begin
                ;forward difference
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos  ],lat_rad[xpos,ypos  ], d_yplus0                         
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos+1],lat_rad[xpos,ypos+1], d_yplus1                         
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos+2],lat_rad[xpos,ypos+2], d_yplus2                         
                gradient[1] = -1.5*d_yplus0[0] + 2.*d_yplus1[0] - .5*d_yplus2[0]
             end
        nyo-1:begin
                ;backward difference
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos  ],lat_rad[xpos,ypos  ],d_yminu0                         
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos-1],lat_rad[xpos,ypos-1],d_yminu1                         
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos-2],lat_rad[xpos,ypos-2],d_yminu2                         
                gradient[1] =  1.5*d_yminu0[0] - 2.*d_yminu1[0] + .5*d_yminu2[0]
             end
        else:begin
                ;centered difference
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos+1],lat_rad[xpos,ypos+1],d_yplus1                         
                gcirc,0,lon0,lat0,lon_rad[xpos,ypos-1],lat_rad[xpos,ypos-1],d_yminu1                         
                gradient[1] = -.5*d_yminu1[0] + .5*d_yplus1[0]
             end
    endcase
    print, 'funct gradient', gradient
endif

;return distance in radians to the point given by coords
gcirc,0,lon0,lat0,lon_rad[xpos,ypos],lat_rad[xpos,ypos],dis0                         
return, dis0[0]

end


pro conj_nearest, lat_in, lon_in, lat_out, lon_out, proj_ind, missing=missing
;use conjugate gradient to obtain projection indices
;
;proj_ind has the same size as lat_in,lon_in and gives the index of the nearest point in lat_out,lon_out

;common block
common share1, lat0, lon0, lat_rad, lon_rad, nxo, nyo

;default values
if n_elements(missing) eq 0 then missing = -9999.

;insure size of input/output are the same
sz_lat = size(lat_in,/dim)
sz_lon = size(lon_in,/dim)
if (sz_lat[0] ne sz_lon[0]) or (sz_lat[1] ne sz_lon[1]) then message, 'size of input lats and lons is not the same'
nx = sz_lat[0]
ny = sz_lat[1]

sz_lato = size(lat_out,/dim)
sz_lono = size(lon_out,/dim)
if (sz_lato[0] ne sz_lono[0]) or (sz_lato[1] ne sz_lono[1]) then message, 'size of output lats and lons is not the same'
nxo = sz_lato[0]
nyo = sz_lato[1]
print, nxo,nyo


;fill in common block
lat_rad = !dtor*double(lat_out) ;algorithms works with radians
lon_rad = !dtor*double(lon_out)

;output array
proj_ind = lonarr(nx,ny)

;tolerance and first guess for CJ algorithm
tol=.1
p_min = [0,0]

restore, filename='~/documents/idl_sav_files/conj_nearest.sav'
quick_plot, dd, [min(dd),max(dd)],'conj_nearest.ps'

;i and j refer to the input lat_lon; ii,jj refer to output lat lon
for i=0,nx-1 do begin
    for j=0,ny-1 do begin
        ;reference lat long in radians
        lat0 = !dtor*double(lat_in[i,j])
        lon0 = !dtor*double(lon_in[i,j])
        ;find closest point
        minF_conj_grad, p_min, f_min, conv_factor, func_name='lat_lon_dg', TOL=tol, /init
        while conv_factor gt 0. do begin
            minF_conj_grad, p_min, f_min, conv_factor, func_name='lat_lon_dg', TOL=tol
        endwhile

        ;CJ provides a good estimate but it is not always perfect, check if a neighbor may be closer
        xpos = long(p_min[0])
        ypos = long(p_min[1])
        refine, xpos, ypos
        proj_ind[i,j] = ypos*nx + xpos
        ;print, 'CG indexes:  ', xpos,ypos

        ;;compute solution by hand 
        ;dd = fltarr(nxo,nyo)
        ;for ii=0,nxo-1 do begin
        ;    for jj=0,nyo-1 do begin
        ;        dd[ii,jj] = lat_lon_dg([ii,jj])
        ;    endfor
        ;endfor
        ;dum = min(dd,pos)
        ;;print, 'true min lat lon: ', lat_out[pos], lon_out[pos]
        ;ypos = floor(pos/nxo)
        ;xpos = pos - nxo*ypos
        ;print, 'MA indexes:  ', xpos,ypos
        ;print, ''

    endfor
    if i mod 100 eq 0 then print, i, ' of ', nx-1
endfor
stop


;;compute solution by hand  ;for testing purposes
i = 0
j = 0
;reference lat long in radians
lat0 = !dtor*double(lat_in[i,j])
lon0 = !dtor*double(lon_in[i,j])

dd = fltarr(nxo,nyo)
for ii=0,nxo-1 do begin
    for jj=0,nyo-1 do begin
        dd[ii,jj] = lat_lon_dg([ii,jj])
    endfor
endfor
save, dd, filename='~/documents/idl_sav_files/conj_nearest.sav', /compress

quick_plot, dd, [min(dd),max(dd)],'conj_nearest.ps'

dum = min(dd,pos)
print, 'true min lat lon: ', lat_out[pos], lon_out[pos]
ypos = floor(pos/nxo)
xpos = pos - nxo*ypos
print, 'man find indexes' ,xpos, ypos
print, ''



end
