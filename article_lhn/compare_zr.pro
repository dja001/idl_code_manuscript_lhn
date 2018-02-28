PRO compare_zr


    ;set up image coords setup 
    sq_w = 10.
    sq_h = .8*sq_w
    pic_h = 15.
    pic_w = 15.*3
    pal_sp = 1.3/pic_w
    pal_w = .25/pic_w
    rec_w = sq_w/pic_w
    rec_h = sq_h/pic_h
    sp_w = 2./pic_w
    sp_h = 2./pic_h
    x1 = 2.2/pic_w 
    x2 = x1 + rec_w + sp_w
    x3 = x2 + rec_w + sp_w + pal_w + sp_w
    y1 = 1./pic_h
    y2 = y1 + rec_h + sp_h
    
    y0 = y1
    pic_dir = '~/documents/ps/'
    pic_name = pic_dir+'compare_zr.ps'
    PS_START, pic_name, pic_w, pic_h

    maxref = 70.
    minref = -20.
    LOADCT, 40, /S
    PLOT, [0], [0], /NODATA, /NORMAL, /NOERASE, POS=pos, $
        XS=1, XR=[minref,maxref], XTIT='Reflectivity [dBZ]',     $
        YS=1, YR=[0.001, 100.], YTIT='Precipitation Rate [mm/hr]', /YLOG
    xx = FINDGEN(401)/(400.) * (maxref - minref) + minref
    ;OPLOT, xx, 10.0^((xx-43.1)/17.5) , COL=210
    yy1 = (10.^(xx/10.) / 200.)^(1./1.6)
    OPLOT, xx, yy1, COL=80, TH=6.
    yy2 = (10.^(xx/16.) / 27.424818)    ;MP with input in dBZ  (10.^(dBZ/10.)/200.)^(1./1.6)
    OPLOT, xx, yy2, COL=210, TH=2.

    print, yy1 - yy2






    PS_CLOSE, pic_name, /DEL_PS, FONT='lmroman', /V, /PDF

END

