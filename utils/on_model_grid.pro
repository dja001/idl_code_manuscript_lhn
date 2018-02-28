PRO ON_MODEL_GRID, field, mod_nx, mod_ny, missing
    ;insures that a 2D of 3D field had x-x dimensions mod_nx-mod_ny
    ;this routine is mostly usefull for gem physical outputs that are provided on a smaller 
    ;grid than dynamical variables


    ;size of input grid
    in_sz = SIZE(field, /DIM)
    n_dim = N_ELEMENTS(in_sz)
    IF (n_dim LT 2) OR (n_dim GT 3) THEN MESSAGE, 'Field must be a 2D or 3D grid'

    ;needed displacement 
    lag_x = (mod_nx-in_sz[0])/2.
    lag_y = (mod_ny-in_sz[1])/2.
    IF (lag_x LT 0) OR (lag_y LT 0) THEN MESSAGE, '-ve lag, something is going wrong'

    CASE N_ELEMENTS(in_sz) OF
        2:BEGIN
              tmp_out = REPLICATE(missing,mod_nx,mod_ny)
              tmp_out[lag_x:mod_nx-lag_x-1,lag_y:mod_ny-lag_y-1] = field
          END
        3:BEGIN
              tmp_out = REPLICATE(missing,mod_nx,mod_ny,in_sz[2])
              tmp_out[lag_x:mod_nx-lag_x-1,lag_y:mod_ny-lag_y-1,*] = field
          END
        ELSE: MESSAGE, 'Field must be a 2D or 3D grid'
    ENDCASE

    field = tmp_out

END
