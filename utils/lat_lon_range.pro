
pro lat_lon_range, lat_d_in, lon_d_in, range_km_in, az_d_in, lat_d_o, lon_d_o
;given a position lat,lon [deg] ; a range [km] and an azimuth [deg] (meteo convention with zero towards north) 
;   give the position (lat_o, lon_o) at the end of the vector
;range is a distance following curvature of the earth

;1st rotation is toward north and is dictated by range
;
;       N
;
;       ^  - 
;       |      -  
;range  |        -    2nd rotation is around point o and is determined by azimuth
;       |         -
;       |     az   -
;       o          -  E
;        \         -
;         \       -
;          \     -
;           x< -    output is lat lon of pt x
;            

;insure data is there
if n_elements(lat_d_in)    eq 0 then message, 'Latitude  must be specified'
if n_elements(lon_d_in)    eq 0 then message, 'Longitude must be specified'
if n_elements(range_km_in) eq 0 then message, 'Range must be specified'
if n_elements(az_d_in)     eq 0 then message, 'Azimuth must be specified'

;hange var name to avoid modifying the value on output
lat_d    = lat_d_in   
lon_d    = lon_d_in   
range_km = range_km_in
az_d     = az_d_in      

;insure everything is double precision
lat_d    = double(lat_d)
lon_d    = double(lon_d)
range_km = double(range_km)
az_d     = double(az_d)

;convert angles to radians
lat_r = lat_d*!dtor
lon_r = lon_d*!dtor
az_r  =  az_d*!dtor

;radius of the earth
re = 6371.0072d    ;[km]
theta_r = range_km/re                                     ;distance [in radians] dictated by range

;1st rotation from point 0 towards the north pole 
v1 = [0d,0d,1d]                                                   ;north pole
v2 = [cos(lat_r)*cos(lon_r),cos(lat_r)*sin(lon_r),sin(lat_r)]   ;x,y,z of point given by lat lon
c1 = crossp(v1,v2)                                              ;axis of rotation
c1 /= norm(c1)                                                  ;make unit vector
mat1 = rot_mat(c1, theta_r)                                     ;rotation matrix for 1st rotation

;2nd rotation around vector given by point zero
mat2 = rot_mat(v2, az_r)
    
;combine the two rotations
mat3 = matrix_multiply(mat2,mat1)

;xyz coords of rotated point 
v5 =  matrix_multiply(mat3,v2)

;transformation of xyz into lat lon
lat_r_o = atan(v5[2],sqrt(v5[0]^2.+v5[1]^2.))
lon_r_o = atan(v5[1],v5[0])                     ;atan returns ]-pi,pi]
;conversion to degrees
rtod = 360d / (2d*!pi) 
lat_d_o = lat_r_o*rtod
;if lat_d_o le 90d then lat_d_o = 90d - lat_d_o 
;if lat_d_o gt 90d then lat_d_o = -(90d - lat_d_o)
lon_d_o = lon_r_o*rtod


end
