pro lat_lon_extend, lat_1_in, lon_1_in, lat_2_in, lon_2_in, lat_3_out, lon_3_out
;given two points (points 1 and 2) on the sphere defined with latitude and longitude,
;return a third point in the same direction (rotation on the sphere along the same vector) 
;and at the same distance than pt1 and pt2
;
;
;  pt3              output
;   ^
;    \
;     \
;      \
;      pt2          input
;       ^
;        \
;         \
;          \
;          pt1      input

;convert angles to radians
lat_1_r = lat_1_in*!dtor
lon_1_r = lon_1_in*!dtor
lat_2_r = lat_2_in*!dtor
lon_2_r = lon_2_in*!dtor

;convert lat_lon to xyz
v1 = [cos(lat_1_r)*cos(lon_1_r),cos(lat_1_r)*sin(lon_1_r),sin(lat_1_r)] 
v2 = [cos(lat_2_r)*cos(lon_2_r),cos(lat_2_r)*sin(lon_2_r),sin(lat_2_r)] 


;solve for angle
theta = acos( total(v1*v2) / (norm(v1)*norm(v2)) )

;get normal vector
v3 = crossp(v1,v2)
v3 /= -norm(v3)     ;-ve so that rotation goes from pt1 to pt2 and not the opposite

;define rotation matrix that goes from pt1 to pt2
mm = rot_mat(v3, theta)

;xyz position of result
v4 = matrix_multiply(mm,v2)

;output in degrees 
lat_3_r = atan(v4[2],sqrt(v4[0]^2.+v4[1]^2.))
lon_3_r = atan(v4[1],v4[0])                     ;atan returns ]-pi,pi]

;conversion to degrees
rtod = 360d / (2d*!pi) 
lat_3_out = lat_3_r*rtod
lon_3_out = lon_3_r*rtod

end
