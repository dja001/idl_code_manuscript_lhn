pro dev_lat_lon, sz, mat_lat, mat_lon
    ;outputs 2D matrices of latitude and longitude associated with the current device
    sz = long(sz)
    pnx = sz[0]
    pny = sz[1]
    xp = rebin(lindgen(pnx),pnx,pny)
    yp = rebin(rotate(lindgen(pny),1),pnx,pny)
    recomp = yp*pnx + xp
    ;convert 2d arrays into 1d
    aa = where(finite(xp))
    xxp = xp[aa]
    yyp = yp[aa]
    ;latitudes and longitudes of the device
    d = convert_coord(xxp,yyp, /device, /to_data,/double)
    dummy = reform(d[0,*])
    mat_lon = dummy[recomp]
    dummy = reform(d[1,*])
    mat_lat = dummy[recomp]

end
