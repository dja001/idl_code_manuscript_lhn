pro lhr_profiles, txtout=txtout

    profile_file = '~/documents/avg_lhr_profiles.txt'

    exp_name = 'aaa'

    missing = -9999.
    jtime=julday(07,08,2014,18,00)
    jul_to_cmc, jtime, cmc_time_t
    jtime=julday(07,08,2014,17,55)
    jul_to_cmc, jtime, cmc_time_mdt

    mod_file = '/local/drive2/arma/armadja/data/lhn_test_outputs/'+exp_name+'/2014070812_006'
    ;get_gem_data, mod_file, var_name='rdbr', values=mod_r,    cmc_timestamp=cmc_time
    get_gem_data, mod_file, var_name='PR', values=pr_t,   cmc_timestamp=cmc_time_t
    get_gem_data, mod_file, var_name='PR', values=pr_mdt, cmc_timestamp=cmc_time_mdt
    pr = (pr_t - pr_mdt)/5.*60.*1000.
    get_gem_data, mod_file, var_name='TA',   values=ta,   p_from_var=ta_p, cmc_timestamp=cmc_time_t
    ;get_gem_data, mod_file, var_name='T4',   values=t4,                    cmc_timestamp=cmc_time_t
    ;get_gem_data, mod_file, var_name='CTE',  values=cte,                   cmc_timestamp=cmc_time_t
    get_gem_data, mod_file, var_name='STE',  values=ste,                   cmc_timestamp=cmc_time_t
    get_gem_data, mod_file, var_name='ST',   values=st,                    cmc_timestamp=cmc_time_t
    ;get_gem_data, mod_file, var_name='tlha', values=tlha,                  cmc_timestamp=cmc_time_t

    sz = size(ta,/dim)
    nx = sz[0]
    ny = sz[1]
    nz = sz[2]

    seed = systime(/s)

    ;set up image coords setup 
    sq_w = 4.
    sq_h = 10.
    pic_h = 15.
    pic_w = 60.
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

    ;bounds for defining profiles
    ;bounds = [0.01,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.]*10.
    bounds = [1.5,3.,6.12,12.5,25.,50.,100.,250]
    np = n_elements(bounds)
    ;heights for averages
    minh = 1100.
    maxh = 0.
    dh = 100.
    interval, maxh, minh, dh, n_bin=nh, val_arr=hvals, bound_arr=hbounds, mm=m, bb=b
    ;make interval for counting bins and outputs the formula for mapping indexes
    ;index = floor(m*val) + b
    ;accumulators
    bstr = {val:dblarr(nh),num:lonarr(nh)}
    acc = replicate(bstr,np-1)

    ;get average profiles
    process_typical, avg_p=avg_p, cat=cat, values=values


    ;open record file
    if keyword_set(txtout) then begin
        openw,f_id, profile_file, /get_lun
        pformat_str = '('+string(nh+1,format='(i03)')+'f11.3)'
        vformat_str = '('+string(nh,format='(i03)')+'e11.3)'
        printf,f_id, 'number of elements:', nh
        printf,f_id, string(hvals,format=vformat_str)
    endif
    
    y0 = y1
    x0 = x1
    pic_dir = '~/documents/ps/'
    pic_name = pic_dir+'lhr_profiles_'+exp_name+'.ps'
    ps_start,pic_name, pic_w, pic_h

        ;;test one profile
        ;pp = 0
        ;aa = where(pr gt 25., naa)
        ;if naa le 0 then message, 'no precip found'
        ;ptpos = aa[0]
        ;jj = floor(ptpos/nx)
        ;ii = ptpos - jj*nx
        ;ta_varr   = reform(ta[ii,jj,*])
        ;t4_varr   = reform(t4[ii,jj,*])
        ;cte_varr  = reform(cte[ii,jj,*])
        ;ste_varr  = reform(ste[ii,jj,*])
        ;st_varr   = reform(st[ii,jj,*])
        ;tlha_varr = reform(tlha[ii,jj,*])
        ;parr      = reform(ta_p[ii,jj,*])
        ;;figure position
        ;x0 =  x1 + (rec_w + sp_w)*pp
        ;pos = [x0,y0,x0+rec_w,y0+rec_h]
        ;minr = -.02
        ;maxr = .02
        ;loadct, 40, /s
        ;plot, [0], [0], /nodata, /normal, /noerase, pos=pos,col=0, $
        ;title=tit_str,                                             $
        ;xs=1, xr=[minr,maxr], xtit='rate of lhr',                  $
        ;ys=1, yr=[1000.,300.], ytit='height [hpa]', /ylog

        ;;oplot, ta_varr,parr, col=250
        ;;oplot, t4_varr,parr, col=200
        ;oplot, ta_varr,parr, col=254
        ;oplot, cte_varr,parr, col=120
        ;oplot, ste_varr,parr, col=80
        ;oplot, st_varr,parr, col=210
        ;print, max(pr)


        ;for each precipitation categories
        for pp=0,np-2 do begin
            low_p  = bounds[pp]
            high_p = bounds[pp+1]

            ;title
            tit_str = string(low_p,high_p,format="(f6.2,'-',f6.2)")
            if keyword_set(txtout) then printf,f_id, tit_str+'  mm/hr'

            ;

            ;plot profiles
            ;figure position
            x0 =  x1 + (rec_w + sp_w)*pp
            pos = [x0,y0,x0+rec_w,y0+rec_h]
            minr = -.02
            maxr = .02
            loadct, 40, /s
            plot, [0], [0], /nodata, /normal, /noerase, pos=pos,col=0, $
            title=tit_str,                                             $
            xs=1, xr=[minr,maxr], xtit='rate of lhr',                  $
            ys=1, yr=[1050.,200.], ytit='height [hpa]', ytickinterval=50.

            aa = where(pr gt low_p and pr le high_p, naa)
            if naa ne 0 then begin
                ;reference profile
                print, naa, ' points in ',tit_str
                ;for each precipitation point in a category
                for nn=0, naa-1 do begin
                    jj = floor(aa[nn]/nx)
                    ii = aa[nn] - jj*nx
                    ;varr = ta[ii,jj,*]
                    varr = ste[ii,jj,*]
                    parr = ta_p[ii,jj,*]
                    loadct, 0, /s
                    oplot, varr,parr, col=200

                    ;accumulate values
                    ;for each model level
                    hindex = floor(m*parr) + b
                    ;limit upper and lower bounds of hindex 
                    bb = where(hindex lt 0, nbb)
                    if nbb ne 0 then hindex[bb] = 0
                    bb = where(hindex ge nh, nbb)
                    if nbb ne 0 then hindex[bb] = nh-1
                    for hh=0, nz-1 do begin
                        acc[pp].val[hindex[hh]] += varr[hh]
                        acc[pp].num[hindex[hh]] += 1
                    endfor
                endfor

                ;average accumulated points in category
                for hh=0, nh-1 do begin
                    if acc[pp].num[hh] eq 0 then begin
                        acc[pp].val[hh]  = 0.
                    endif else begin
                        acc[pp].val[hh] /= acc[pp].num[hh] 
                    endelse
                endfor
                ;plot average
                loadct, 40, /s
                oplot, acc[pp].val, hvals, col=210
                oplot, values[*,pp+1],values[*,0], col=120
                oplot, acc[pp].val*0., hvals, col=0
                ;print average
                if keyword_set(txtout) then begin
                    printf,f_id, '      '+string(acc[pp].val,format=vformat_str)
                    printf,f_id, ''
                endif

            endif else begin
                print, 0l, ' points in ',tit_str
            endelse
        endfor



    ps_close, pic_name, /del_ps, font='lmroman', /v, /pdf


    if keyword_set(txtout) then free_lun, f_id
end

