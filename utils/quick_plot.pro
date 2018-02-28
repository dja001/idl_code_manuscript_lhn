pro quick_plot, data, range, pic_name, missing=missing, data2=data2, data3=data3

if n_elements(missing) eq 0 then missing = -9999.

sz = size(data,/dim)

pic_dir = '~/documents/ps/'
pic_name = pic_dir+pic_name

;set up image coords setup 
ratio = .8
pic_w = 12    
pic_h = 10.
pal_sp = .9/pic_w
pal_w = .25/pic_w
sq_sz = 8.
rec_w = sq_sz/pic_w
rec_h = ratio*sq_sz/pic_h
sp_w = 1./pic_w
sp_h = .8/pic_h
x1 = sp_w
y1 = sp_h


;legs, range=range, color_arr='b_w', over_high='extend', under_low='white', $
;      excep_val=[str.missing,-4000.], excep_col=[[169,222,255],[160,136,113]], excep_tol=[1e-3,1e-3], $
;      mapping=mapping

legs, range=range, color_arr='b_w', excep_val=[missing], excep_col=['brown'], dark_pos='low', $
      over_under='extend',mapping=mapping



;plot image
ps_start, pic_name, pic_w, pic_h

    x0 = x1
    y0 = y1

   ;     ;color palette
   ;     pos = [x0+rec_w+pal_sp,y0,x0+rec_w+pal_sp+pal_w,y0+rec_h]
   ;     legs, mapping=mapping, palette=pos, units='radians'

    pos = [x0,y0,x0+rec_w,y0+rec_h]
    ;legs, data=data, mapping=mapping, tv_out=pos
    ;plot, [0],[0], /nodata, /noerase, /normal, pos=pos, $
    ;xs=1, xr=[0,sz[0]],$
    ;ys=1, yr=[0,sz[1]]

    maps, pos=pos, /map, /grid, loc='test'
    sz = size(data,/dim)
    for ii=0, sz[0]-1 do begin
        for jj=0, sz[1]-1 do begin
            pp = convert_coord([data2[ii,jj],data[ii,jj]], /data, /to_normal)
            if pp[0] gt pos[0] and pp[0] lt pos[2] and pp[1] gt pos[1] and pp[1] lt pos[3] then begin
                plots, data2[ii,jj],data[ii,jj], psym=8, symsize=.1, col=210
            endif
        endfor
    endfor
    loadct, 40,/s
    plots, data2[*,0],data[*,0], psym=8, symsize=.1, col=80
    plots, data2[sz[0]-1,*],data[sz[0]-1,*], psym=8, symsize=.1, col=80






ps_close, pic_name, /pdf, /del_ps, font='lmroman', /verbose


end
