pro separate_lhn_out
;takes a bunch of archives .ca files and separate the lhn output 

;search archive files
data_dir  = '/local/raid/armadja/data/lhn_runs_outputs/lhn_006_post/'
output_dir= '/local/raid/armadja/data/lhn_pr_p0_outputs/'

this_experiment = strmid(file_basename(data_dir),0,7)

;;automatically do every cases found
ca_list = file_search(data_dir+'*.ca', count=nca)
if nca eq 0 then message, 'No .ca fields found in: '+data_dir

    ;;manual seletion of cases
    ;ca_list = '/local/raid/armadja/data/lhn_runs_outputs/lhn_002_post/2014070200.ca'
    ;nca = n_elements(ca_list)

;write directives file
SPAWN, 'echo $RANDOM', rand
rand_str = STRING(rand,FORMAT='(i06)')
tmpdir = GETENV('TMPDIR')
directives_file = tmpdir+'/'+rand_str+'directives.txt'
openw, lun, directives_file, /get_lun
printf, lun, " desire(-1,['P0','PR','^^','>>'])"
free_lun, lun

;for each archive file
for nn=0, nca-1 do begin
    ;get forecast date
    this_archive = ca_list[nn]
    print, this_archive
    this_date = strmid(file_basename(this_archive),0,10)

    ;;create output directory if it is not already there
    this_out_dir = output_dir+'/'+this_experiment+'/'+this_date+'/'
    if file_test(this_out_dir) then begin

        message, 'directory skipped: '+this_out_dir, /informational
        continue


        ;message, 'output_directory already exists: '+this_out_dir

        ;print, 'output_directory already exists'
        ;print, 'its content will be deleted and replaced'
        ;cmd = 'rm -f '+this_out_dir+'/*'
        ;spawn, cmd
    endif else begin
        file_mkdir, this_out_dir
    endelse

    ;change directory 
    cd, this_out_dir, current=current
    ;dearchive all lhn.* files
    cmd = 'cmcarc -f '+this_archive+' -x gridpt.prog.lhn.*'
    print, cmd
    spawn, cmd
    ;back to original directory 
    cd, current

    ;filter output
    f_list= file_search(this_out_dir+'*',count=nf)
    for ff=0, nf-1 do begin
        this_source = f_list[ff]
        f_basename = file_basename(this_source)
        f_dirname  = file_dirname(this_source)
        new_name = strmid(f_basename,16,strlen(f_basename)-16)
        this_dest = f_dirname+'/'+new_name
        ;filter output file
        cmd = 'editfst -s '+this_source + $
                     ' -d '+this_dest   + $
                     ' -i '+directives_file
        spawn, cmd
        file_delete, this_source
    endfor
    
endfor

file_delete, directives_file

end
