pro plot_tree, ii, nn, exp_struc, missing,  $ ;base info
               yorder, ps                     ;image position info

    ;get data
    file = '/local/raid/armadja/data/test/merged_tree.fst'
    get_gem_data, file, VAR_NAME='TREE', values=tree,    cmc_timestamp=exp_struc.time_t[ii,nn,0], is_there=is_there 
    ;get_gem_data, exp_struc.file_t[ii,nn,0], VAR_NAME='TREE', values=tree,    cmc_timestamp=exp_struc.time_t[ii,nn,0], is_there=is_there 
    if is_there eq 0 then begin
        tree = replicate(missing, exp_struc.nx[0], exp_struc.ny[0])
    endif else begin
        on_model_grid, tree, exp_struc.nx[0], exp_struc.ny[0], missing
    endelse



    ;full tree with all possibilities
    col_arr = [ [134, 255, 134],$  
                [000, 129, 000],$ 
                [000, 199, 144],$ 
                                $
                [255, 255, 255],$
                [255, 255, 169],$
                [255, 205, 124],$
                [255, 159, 071],$
                [255, 119, 051],$
                [164, 053, 000],$
                [104, 010, 000],$
                                $
                [220, 114, 255],$
                [204, 249, 255],$
                [220, 220, 220],$
                [100, 100, 100]]
    ;categorical palette
    cat_names = [  'scaling (min ratio)',           $
                        'scaling',                  $
                             'scaling (max ratio)', $
                                 'avg no-effect',   $
                                       'avg 1.5-3 mm/h', $
                                               'avg 3-6 mm/h', $
                                                     'avg 6-12 mm/h', $
                                                          'avg 12-25 mm/h', $
                                                                'avg 25-50 mm/h', $
                                                                      'avg 50+ mm/h', $
                                                                           'reduce lh profiles',    $
                                                                               'no rain / no rain', $
                                                                                     'no radar',    $
                                                                                          'no lhn']
    ;              0.9  1.0  1.1  2.0  2.1     2.2   2.3  2.4   2.5   2.6  3.0  4.0  5.0  6.0
    legs, range=[.8, .95, 1.05, 1.5, 2.05, 2.15, 2.25, 2.35, 2.45, 2.55, 2.65, 3.5, 4.5, 5.5, 6.5], $
          color_arr=col_arr, solid='supplied', $
          mapping=mapping_tree, $
          excep_val=[missing], excep_col=['grey_220']


    ;change tree numbers for order of palette
    new_tree = fltarr(size(tree,/dim))
    aa = where(tree gt .8 and tree lt 1.2, naa)
    if naa ne 0 then new_tree[aa] = 4.
    aa = where(tree ge 1.2 and tree lt 2.05, naa)
    if naa ne 0 then new_tree[aa] = 1.
    aa = where(tree ge 2.05 and tree lt 2.9, naa)
    if naa ne 0 then new_tree[aa] = 3.
    aa = where(tree ge 2.9 and tree lt 3.5, naa)
    if naa ne 0 then new_tree[aa] = 2.
    aa = where(tree ge 3.5 and tree lt 4.5, naa)
    if naa ne 0 then new_tree[aa] = 1.
    aa = where(tree ge 4.5 and tree lt 5.5, naa)
    if naa ne 0 then new_tree[aa] = 0.
    aa = where(tree ge 5.5 , naa)
    if naa ne 0 then new_tree[aa] = missing

    ;reduced tree for conference presentation
    col_arr = [[220, 220, 220],$  ;grey
               [240, 255, 255],$  ;light blue
               [255, 114, 255],$  ;pink
               [255, 154, 000],$  ;orange
               [003, 119, 183]]  ;blue
    ;col_arr = [ [086, 191, 001],$  ;vert
    ;            [209, 060, 061],$  ;rouge
    ;            [141, 001, 255],$  ;mauve
    ;            [240, 255, 255],$  ;light blue
    ;            [220, 220, 220]]   ;grey
     cat_names = ['no radar',        $
                  'do nothing',      $
                  'reduce profiles', $
                  'Typical profiles',$
                  'scale profiles']
    legs, range=[-.5, .5, 1.5, 2.5, 3.5, 4.5], $
          color_arr=col_arr, solid='supplied', $
          mapping=mapping_reduced_tree, $
          excep_val=[missing], excep_col=[[220,220,220]]

    ;project data
    ;apply_proj, tree, exp_struc.proj_ind[*,*,0], proj_tree, missing=missing
    apply_proj, new_tree, exp_struc.proj_ind[*,*,0], proj_tree, missing=missing

    ;apply color mapping
    ;legs, data=proj_tree,     mapping=mapping_tree,    img_out=img_tree
    legs, data=proj_tree,     mapping=mapping_reduced_tree,    img_out=img_tree

    ;figure position
    y0 = ps.y1 + yorder*(ps.rec_h + ps.sp_h)
    x0 = ps.x1 + nn*(ps.rec_w + ps.sp_w)
    pos = [x0,y0,x0+ps.rec_w,y0+ps.rec_h]

    ;plot images
    loadct,0,/s
    tv, img_tree, pos[0], pos[1], xs=pos[2]-pos[0], ys=pos[3]-pos[1], /normal, true=3

    ;overlay grid
    MAPS, POS=pos, /GRID, /MAP, LOC=exp_struc.loc
    nx = exp_struc.nx
    ny = exp_struc.ny
    PLOT_BORDER, MAT_LAT=exp_struc.lat[0:nx-1,0:ny-1,0], MAT_LON=exp_struc.lon[0:nx-1,0:ny-1,0]

    ;color rectangle for title
    sx0 = pos[0]+.02*(pos[2]-pos[0]) 
    sy0 = pos[1]+.87*(pos[3]-pos[1]) 
    spos = [sx0,sy0,sx0+ps.rec_w/2.+.3/ps.pic_w,sy0+.8/ps.pic_h]
    ;tvlct, r_arr[*,nn], g_arr[*,nn], b_arr[*,nn]
    loadct, 0, /s
    tv,[255], spos[0], spos[1], xs=spos[2]-spos[0], ys=spos[3]-spos[1], /normal
    ;title text
    loadct, 40, /s
    xyouts, sx0+.2/ps.pic_w, sy0+.2/ps.pic_h, 'LHN decision tree',  /normal,charsize=1.4, col=0

    ;palette
    nexp = exp_struc.nexp
    if nn eq nexp-1 then begin
        x0 = x0 + ps.rec_w + ps.pal_sp
        pos = [x0,y0,x0+ps.pal_w,y0+ps.rec_h]
        ;legs, mapping=mapping_tree, palette=pos, pal_prop='equal', cat_names=cat_names
        legs, mapping=mapping_reduced_tree, palette=pos, pal_prop='equal', cat_names=cat_names
    endif
end
