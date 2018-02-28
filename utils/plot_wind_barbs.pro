pro windbarb, x, y, wspeed, wdirection, $
    ;procedure needed for plot_wind_barb procedure
   aspect=aspect, $
   clip=clip, $
   color=color, $
   length=length, $
   normal=normal, $
   map_rotation=map_rotation, $
   southern_hemisphere=southfeathers, $
   thick=thick

   on_error, 2

   ; check positional parameters.
   if n_params() ne 4 then begin
      print, 'required syntax: windbard x, y, speed, direction, aspect=aspect, length=length, map_rotation=map_rotation'
      message, 'incorrect number of positional parameters.'
   endif

   ; check keywords
   if n_elements(aspect) eq 0 then begin
      if total(!x.window) eq 0 then begin
         aspect = float(!d.y_size) / !d.x_size
      endif else begin
         aspect = ((!y.window[1] - !y.window[0]) * !d.y_size) / ((!x.window[1] - !x.window[0]) * !d.x_size)
      endelse
   endif
   ;force aspect ratio = 1
   if n_elements(color) eq 0 then color = intarr(n_elements(x))
   if n_elements(color) eq 1 then color = replicate(color, n_elements(x))
   if n_elements(length) eq 0 then begin
      if total(!x.window) eq 0 then begin
         length = 1.0 / 15.0
      endif else begin
         length = (!x.window[1] - !x.window[0]) / 15.0
      endelse
   endif
   if n_elements(map_rotation) eq 0 then map_rotation = 0.0
   if n_elements(thick) eq 0 then thick = 1.0

   ; initialize variables.
   sr = length * 0.25
   staff_len = length - sr
   barb_len = staff_len * 0.8
   half_len = staff_len * 0.45

    if keyword_set(normal) then begin
      xx = x
      yy = y
   endif else begin
      coord = convert_coord(x, y, /data, /to_normal)
      xx = coord[0,*]
      yy = coord[1,*]
   endelse

   ; make sure you have a clipping rectangle.
   if n_elements(clip) eq 0 then begin
      minxx = min(xx, max=maxxx)
      minyy = min(yy, max=maxyy)
      clip = [minxx-0.25, minyy-0.25, maxxx+0.25, maxyy+0.25]
   endif

   ; loop through all the elements of the array.

   for j=0l, n_elements(x) - 1 do begin

      ; if the speed is less that 2.5 knots
      if wspeed[j] lt 2.5 then continue

      ; set up directions for staff and barbs.

      dr  = (wdirection[j] + map_rotation) * !dtor
      if dr gt (2*!pi) then dr = dr - (2 * !pi)
      drb = (wdirection[j] + 60 + map_rotation) * !dtor

      if keyword_set(southfeathers) then begin
         sindr =  sin(dr)
         sindrb = -sin(drb)
         cosdr =  cos(dr)
         cosdrb = -cos(drb)
      endif else begin
         sindr =  sin(dr)
         sindrb = sin(drb)
         cosdr =  cos(dr)
         cosdrb = cos(drb)
      endelse

      ; count the number of 50 knot pennants and 10 knot barbs we will need.

      num50 = 0
      num10 = 0
      sp = wspeed[j] + 2.5 ; rounded to nearest 2.5 knots.
      while sp ge 50 do begin
         num50 = num50 + 1
         sp = sp - 50
      endwhile
      while sp ge 10 do begin
         num10 = num10 + 1
         sp = sp - 10
      endwhile

      ; draw the staff.

      x1 = clip[0] > (xx[j] + sindr * sr) < clip[2]
      y1 = clip[1] > (yy[j] + cosdr * sr * aspect) < clip[3]
      x2 = clip[0] > (x1 + sindr * staff_len) < clip[2]
      y2 = clip[1] > (y1 + cosdr * staff_len * aspect) < clip[3]
      
      plots, [x1, x2], [y1,y2], /normal, clip=clip, thick=thick, color=color[j] 

      ; draw any half-barbs.

      if sp ge 5 then begin
         x1 = x2 + sindrb * half_len
         y1 = y2 + cosdrb * half_len * aspect
         if x1 lt clip[0] or x1 gt clip[2] then continue
         if x2 lt clip[0] or x2 gt clip[2] then continue
         if y1 lt clip[1] or y1 gt clip[3] then continue
         if y2 lt clip[1] or y2 gt clip[3] then continue
         plots, [x1, x2], [y1,y2], /normal, clip=clip, thick=thick, color=color[j] 
         if (num50 eq 0) and (num10 eq 0) then begin
            x1 = x2 + sindr * half_len
            y1 = y2 + cosdr * half_len * aspect
            if x1 lt clip[0] or x1 gt clip[2] then continue
            if x2 lt clip[0] or x2 gt clip[2] then continue
            if y1 lt clip[1] or y1 gt clip[3] then continue
            if y2 lt clip[1] or y2 gt clip[3] then continue
            plots, [x1, x2], [y1,y2], /normal, clip=clip, thick=thick, color=color[j] 
         endif
      endif

      x1 = x2
      y1 = y2

      ; draw full barbs.

      for i = 1, num10 do begin
         x2 = x1 + sindr * half_len
         y2 = y1 + cosdr * half_len * aspect
         x3 = x2 + sindrb * staff_len
         y3 = y2 + cosdrb * staff_len * aspect
         if x1 lt clip[0] or x1 gt clip[2] then continue
         if x2 lt clip[0] or x2 gt clip[2] then continue
         if x3 lt clip[0] or x3 gt clip[2] then continue
         if y1 lt clip[1] or y1 gt clip[3] then continue
         if y2 lt clip[1] or y2 gt clip[3] then continue
         if y3 lt clip[1] or y3 gt clip[3] then continue
         plots, [x1, x2, x3], [y1, y2, y3], /normal, clip=clip, thick=thick, color=color[j] 
         x1 = x2
         y1 = y2
      endfor

      ; draw pennants.

      for i = 1, num50 do begin

         x0 = x1
         y0 = y1

         x2 = x1 + sindr * half_len
         y2 = y1 + cosdr * half_len * aspect
         x3 = x2 + sindrb * staff_len
         y3 = y2 + cosdrb * staff_len * aspect

         x1 = x2
         y1 = y2

         x2p = x0 + sindr * half_len
         y2p = y0 + cosdr * half_len * aspect

         x2 = x1 + sindr * half_len
         y2 = y1 + cosdr * half_len * aspect
         if x0 lt clip[0] or x0 gt clip[2] then continue
         if x2 lt clip[0] or x2 gt clip[2] then continue
         if y0 lt clip[1] or y0 gt clip[3] then continue
         if y2 lt clip[1] or y2 gt clip[3] then continue
         plots, [x2, x0], [y2, y0], /normal, clip=clip, thick=thick, color=color[j]
         polyfill, [x1, x2, x3], [y1, y2, y3], /normal, clip=clip, color=color[j] 

         x1 = x2p
         y1 = y2p

      endfor
   endfor
end


pro arrows, x, y, wspeed, wdirection, $
   ;procedure needed for plot_wind_barb procedure
   aspect=aspect, $
   clip=clip, $
   color=color, $
   length=length, $
   normal=normal, $
   map_rotation=map_rotation, $
   southern_hemisphere=southfeathers, $
   thick=thick, $
   maxwind=maxwind

   on_error, 2

   ; check positional parameters.
   if n_params() ne 4 then begin
      print, 'required syntax: windbard x, y, speed, direction, aspect=aspect, length=length, map_rotation=map_rotation'
      message, 'incorrect number of positional parameters.'
   endif

   if ~keyword_set(maxwind) then message, 'keyword maxwind must be used with the arrows procedure'

   ; check keywords
   if n_elements(aspect) eq 0 then begin
       message, 'please specify aspect ratio'
   endif
   if n_elements(color) eq 0 then color = intarr(n_elements(x))
   if n_elements(color) eq 1 then color = replicate(color, n_elements(x))
   if n_elements(length) eq 0 then begin
      if total(!x.window) eq 0 then begin
         length = 1.0 / 15.0
      endif else begin
         length = (!x.window[1] - !x.window[0]) / 15.0
      endelse
   endif
   if n_elements(map_rotation) eq 0 then map_rotation = 0.0
   if n_elements(thick) eq 0 then thick = 1.0

   ; initialize variables.
   sr = 0.;length * 0.25
   staff_len = length - sr

    if keyword_set(normal) then begin
      xx = x
      yy = y
   endif else begin
      coord = convert_coord(x, y, /data, /to_normal)
      xx = coord[0,*]
      yy = coord[1,*]
   endelse

   ; make sure you have a clipping rectangle.
   if n_elements(clip) eq 0 then begin
      minxx = min(xx, max=maxxx)
      minyy = min(yy, max=maxyy)
      clip = [minxx-0.25, minyy-0.25, maxxx+0.25, maxyy+0.25]
   endif
   ; loop
   ; loop through all the elements of the array.
   for j=0l, n_elements(x) - 1 do begin

      ;; if the speed is less that 2.5 knots
      ;if wspeed[j] lt 2.5 then begin
      ;    continue
      ;endif

      ; set up directions for staff and barbs.

      dr  = (wdirection[j] + map_rotation) * !dtor
      ar_ang = 20.*!dtor
      drd = dr + ar_ang
      drg = dr - ar_ang

      ;sines and cosines
      sindr  =  sin(dr)
      cosdr  =  cos(dr)
      sindrd =  sin(drd)
      cosdrd =  cos(drd)
      sindrg =  sin(drg)
      cosdrg =  cos(drg)

      ; draw the staff.
      this_len = staff_len*wspeed[j]/maxwind
      ;ar_len = .2*staff_len
      ar_len = .3*this_len
      x1 = clip[0] > (xx[j] + sindr * sr) < clip[2]
      y1 = clip[1] > (yy[j] + cosdr * sr * aspect) < clip[3]
      x2 = clip[0] > (x1 + sindr * this_len) < clip[2]
      y2 = clip[1] > (y1 + cosdr * this_len * aspect) < clip[3]
      plots, [x1, x2], [y1,y2], /normal, clip=clip, thick=thick, color=color[j] 
      print, this_len, aspect

      ; draw the arrowhead
      ax1 = (x1 + sindrd*ar_len)
      ax2 =  x1
      ax3 = (x1 + sindrg*ar_len)
      ay1 = (y1 + cosdrd*ar_len*aspect)
      ay2 =  y1 
      ay3 = (y1 + cosdrg*ar_len*aspect)
      plots, [ax1,ax2,ax3], [ay1,ay2,ay3], /normal, clip=clip, thick=thick, color=color[j] 
      

   endfor
end


pro plot_wind_barbs, u, v, pos=pos, wind_color=wind_color, length=length, nbarbs=nbarbs, thick=thick, nearest=nearest, maxwind=maxwind, aspect=aspect
    ;procedure to plot wind barbs
    if ~keyword_set(length) then length=0.015
    if ~keyword_set(thick) then thick=.3
    if ~keyword_set(nbarbs) then nbarbs=15

    ;if only one number assigned to nbarbs, plot same number of barbs in x and y
    if n_elements(nbarbs) eq 1 then begin
        nbarbsx = nbarbs
        nbarbsy = nbarbs
    endif else begin
        nbarbsx = nbarbs[0]
        nbarbsy = nbarbs[1]
    endelse

    ;grid indices of points where vectors are to be dislayed
    vel_sz=SIZE(u,/DIM)
    x_inter=rebin(findgen(nbarbsx)*(vel_sz[0]-1)/(nbarbsx-1),nbarbsx,nbarbsy,/sample)
    y_inter=rebin(rotate(findgen(nbarbsy)*(vel_sz[1]-1)/(nbarbsy-1),1),nbarbsx,nbarbsy,/sample)

    if keyword_set(nearest) then begin
        ;nearest neighbor 
        barb_u=u[x_inter,y_inter]
        barb_v=v[x_inter,y_inter]
    endif else begin
        ;linear interpolation instead
        barb_u=interpolate(u,x_inter,y_inter)
        barb_v=interpolate(v,x_inter,y_inter)
    endelse

    ;w_ss=wind_mstoknot(sqrt(barb_u^2.+barb_v^2.)) ;; m/s - knots
    w_ss=sqrt(barb_u^2.+barb_v^2.) ;; knots
    w_dd=atan(barb_u,barb_v)*!radeg  -180.
    print, x_inter
    print, 'a'
    print, y_inter
       
    wind_xcord=(findgen(nbarbsx) + 1.)*( (pos[2]-pos[0])/(nbarbsx+1) ) + pos[0]
    wind_xcord=rebin(wind_xcord, nbarbsx, nbarbsy)

    wind_ycord=(findgen(nbarbsy) + 1.)*( (pos[3]-pos[1])/(nbarbsy+1) ) + pos[1]
    wind_ycord=rebin(rotate(wind_ycord,1), nbarbsx, nbarbsy)

    print, 'b'
    print, wind_xcord
    print, 'b'
    print, wind_ycord

    ;windbarb, wind_xcord, wind_ycord, w_ss, w_dd, color=wind_color, length=length, thick=thick, /normal, clip=[0,0,1,1], aspect=aspect
    arrows, wind_xcord, wind_ycord, w_ss, w_dd, color=wind_color, length=length, thick=thick, /normal, clip=[0,0,1,1], maxwind=maxwind, aspect=aspect

end
