pro rhi_arrow, rinfo, max_range, az, dr=dr, dist_r=dist_r, thick=thick, color=color, ar_len=ar_len
;plots an arrow and range marks to easily locate a rhis

if n_elements(dr)     eq 0 then dr     = 50.    ;range marks every 50 km by default 
if n_elements(dist_r) eq 0 then dist_r = 10.    ;total lenght [km] of distance mark
if n_elements(thick)  eq 0 then thick  = 1.    ;thickness of line
if n_elements(color)  eq 0 then color  = 0      ;color of line
if n_elements(ar_len) eq 0 then ar_len = 30.    ;length of arrow feathers

;if a rgb array is provided instead of a color index
if n_elements(color) eq 3 then begin
    tvlct,color[0],color[1],color[2]
    color_ind=0
endif else begin
    color_ind = color
endelse

;print dot at radar location
start_lat = rinfo.lat
start_lon = rinfo.lon
plots, [start_lon], [start_lat], psym=8, col=color_ind, symsize=.8

count=0
this_r = dr
while this_r lt max_range do begin
    lat_lon_range, rinfo.lat, rinfo.lon, this_r,    az,      stop_lat,  stop_lon
    lat_lon_range,  stop_lat,  stop_lon, dist_r/2., az+90., right_lat, right_lon
    lat_lon_range,  stop_lat,  stop_lon, dist_r/2., az-90.,  left_lat,  left_lon
    plots, [start_lon, stop_lon], [start_lat, stop_lat],                   th=thick, col=color_ind
    plots, [left_lon, stop_lon,right_lon], [left_lat, stop_lat,right_lat], th=thick, col=color_ind

    this_r += dr
    start_lat = stop_lat
    start_lon = stop_lon
endwhile
;last segment with arrow feathers
this_r = max_range
lat_lon_range, rinfo.lat, rinfo.lon, this_r,    az,      stop_lat,  stop_lon
lat_lon_range,  stop_lat,  stop_lon, ar_len,    az+150., right_lat, right_lon
lat_lon_range,  stop_lat,  stop_lon, ar_len,    az-150.,  left_lat,  left_lon
plots, [start_lon, stop_lon], [start_lat, stop_lat],                   th=thick, col=color_ind
plots, [left_lon, stop_lon,right_lon], [left_lat, stop_lat,right_lat], th=thick, col=color_ind

end
