pro mv_files
;check if files appear on cetus and move them if they are more than a couple hours old
;this is to insure that I don't runs out pf space on cetus. 

base_dir = '/fs/cetus/fs3/mrb/arma/armadja/maestro_archives/'
dir_list = file_search(base_dir+'/*',count=ndirs)

dest_dir = '/local/raid/armadja/data/lhn_runs_outputs/'


while 1 do begin
    for dd=0, ndirs-1 do begin
        this_dir = dir_list[dd]
        these_files = file_search(this_dir+'/*',count=nfiles)
        for ff=0, nfiles-1 do begin
            now = systime(/seconds)
            this_file = these_files[ff]
            info = file_info(this_file)
            el_h = (now - info.mtime)/3600.
            
            ;if file has been there for more than 3 hours, then move it out
            if el_h gt 3. then begin
                dest_file = dest_dir+file_basename(this_dir)+'/'+file_basename(this_file)
                print, this_file
                print, dest_file
                print, ''
                file_move, this_file, dest_file, /v, /overwrite
            endif
        endfor
    endfor

    wait, 60
endwhile

end
