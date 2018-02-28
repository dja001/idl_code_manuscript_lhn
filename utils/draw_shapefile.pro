;+
; The DRAW_SHAPEFILE procedure plots vectors, points or filled polygons on the current graphics device reading the content of
; a shape file. You can also project the data using the keyword MAP_STRUCTURE (passing a map projection structure from MAP_PROJ_INIT).
;-
PRO DRAW_SHAPEFILE, file, MAP_STRUCTURE=map_str, POLYFILL=polyfill, $
  LON_OFFSET=lon_offset, LAT_OFFSET=lat_offset, LIMIT=limit, _EXTRA=extra

  ; Extra Graphics Keywords
  CATCH, error
  IF error EQ 0 THEN BEGIN
  
    CD, '.', CURR=curr
    
    ;; Check for compression
    ext=STRUPCASE(STRMID(file,STRPOS(file,'.',/REVERSE_SEARCH)+1,99))
    IF ext EQ 'ZIP' THEN BEGIN
      SPAWN, 'unzip '+file
      file=FILEPATH(FILE_BASENAME(file),ROOT_DIR=curr)
    ENDIF
    
    ;; Shape information
    shp=OBJ_NEW('IDLffShape')
    aux=shp->Open(file)
    entity=shp->GetEntity(/ALL,/ATTRIBUTES)
    
    FOR i=0L, N_ELEMENTS(entity)-1 DO BEGIN
    
      curr_entity=entity[i]
      cuts = [*curr_entity.parts, curr_entity.n_vertices]
      
      FOR j=0L, curr_entity.n_parts-1 DO BEGIN
        xx=REFORM((*curr_entity.vertices)[0, cuts[j]:cuts[j+1]-1])
        yy=REFORM((*curr_entity.vertices)[1, cuts[j]:cuts[j+1]-1])
        
        IF KEYWORD_SET(lon_offset) THEN xx+=lon_offset
        IF KEYWORD_SET(lat_offset) THEN yy+=lat_offset
        
        IF KEYWORD_SET(limit) THEN BEGIN
          IF TOTAL(yy LT limit[0] OR yy GT limit[2]) GE 1 THEN CONTINUE
          IF TOTAL(xx LT limit[1] OR xx GT limit[3]) GE 1 THEN CONTINUE
        ENDIF
        
        IF KEYWORD_SET(map_str) THEN BEGIN
          res=MAP_PROJ_FORWARD(xx,yy,MAP_STRUCTURE=map_str)
          
          xx=res[0,*]
          yy=res[1,*]
        ENDIF
        
        IF KEYWORD_SET(polyfill) THEN POLYFILL, xx, yy, _STRICT_EXTRA=extra $
        ELSE PLOTS, xx, yy, _STRICT_EXTRA=extra
      ENDFOR
      
      shp->DestroyEntity, curr_entity
    ENDFOR
    
  ENDIF ELSE BEGIN
    ;; Error
    MESSAGE, 'Error drawing the file '+file+'. '+!error_state.MSG, /INFO
  ENDELSE
  CATCH, /CANCEL
  
  ;; Delete extracted file
  IF ext EQ 'ZIP' THEN FILE_DELETE, file
  
  IF OBJ_VALID(shp) THEN OBJ_DESTROY, shp
END
