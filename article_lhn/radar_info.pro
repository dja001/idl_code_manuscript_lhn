pro radar_info, radar, id=id
;returns structure with information on all or one radar
;keyword id is used to get info on one specific radar

radar_db_file = '~/documents/radar_db.txt'
nl = file_lines(radar_db_file)
nhead=5

radar = {id:'',name:'',area:'',lat:0.,lon:0.,alt:0.,tower_h:0.,beamwidth:0.,freq:0.,callsign:'',phone:''}

if keyword_set(id) then begin
    this_id = id
endif else begin
    this_id = 'all'
    radar = replicate(radar,nl-nhead)
endelse

found =0
openr, f_id, radar_db_file, /get_lun
    dum= ' '
for nn=0, nhead-1 do readf, f_id, dum   ;skip header
for nn=0, nl-nhead-1 do begin
    dum= ' '
    readf, f_id, dum

    dum = strsplit(dum,',',/extract )
    if strtrim(dum[0]) eq this_id then begin
        radar.id            = strtrim(dum[0],2)
        radar.name          = strtrim(dum[1],2)
        radar.area          = strtrim(dum[2],2)
        radar.lat           = strtrim(dum[3],2)
        radar.lon           = strtrim(dum[4],2)
        radar.alt           = strtrim(dum[5],2)
        radar.tower_h       = strtrim(dum[6],2)
        if strmid(radar.id,0,2) ne 'US' then begin
            radar.beamwidth = strtrim(dum[7],2)
            radar.freq      = strtrim(dum[8],2)
            radar.callsign  = strtrim(dum[9],2)
            if n_elements(dum) ge 10 then radar.phone = strtrim(dum[10],2)
        endif
        found=1
        break
    endif
    if this_id eq 'all' then begin
        radar[nn].id            = strtrim(dum[0],2)
        radar[nn].name          = strtrim(dum[1],2)
        radar[nn].area          = strtrim(dum[2],2)
        radar[nn].lat           = strtrim(dum[3],2)
        radar[nn].lon           = strtrim(dum[4],2)
        radar[nn].alt           = strtrim(dum[5],2)
        radar[nn].tower_h       = strtrim(dum[6],2)
        if strmid(radar[nn].id,0,2) ne 'US' then begin
            radar[nn].beamwidth = strtrim(dum[7],2)
            radar[nn].freq      = strtrim(dum[8],2)
            radar[nn].callsign  = strtrim(dum[9],2)
            if n_elements(dum) ge 11 then radar[nn].phone = strtrim(dum[10],2)
        endif
    endif
endfor
free_lun, f_id

if this_id ne 'all' and found eq 0 then message, 'radar: '+id+' not found'


;code for conversion of minute second to decimal degrees
    ;if strmid(strtrim(dum[0]),0,2) eq 'US' then begin
    ;    lat = strtrim(dum[3],2)
    ;    lon = strtrim(dum[4],2)
    ;    lat = strmid(lat,0,2) + strmid(lat,2,2)/60. + strmid(lat,4,2)/3600.
    ;    lon = strmid(lon,0,3) + strmid(lon,3,2)/60. + strmid(lon,5,2)/3600.
    ;    alt = strtrim(dum[5],2)
    ;    print, string(lat,-lon,alt*.3048,format='(3f15.6)')
    ;endif

end
