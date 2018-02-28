pro multi_score_img
;script called to run verification routines for 
;    many forecasts hours
;    many experiments
;all at once


t0 = julday(07, 02, 2014, 12, 00, 00)
tf = julday(07, 13, 2014, 00, 00, 00)
analy_dt   = 12d ;hours
count=0d
this_time = t0
while this_time le tf do begin
    ;time of nowcast is offet with analysis time
    caldat, this_time, mo, dd, yy, hh, mi, ss
    if count eq 0 then begin
        analy_hours_txt = string(yy,mo,dd,hh,format='(i04,3i02)')
    endif else begin
        analy_hours_txt = [analy_hours_txt,string(yy,mo,dd,hh,format='(i04,3i02)')]
    endelse
    count ++
    this_time = t0 + count*analy_dt/24d 
endwhile


;analy_hours_txt = ['2014070812', $
;                   '2014070300', $
;                   '2014070312']



exp_list = ['/local/raid/armadja/data/lhn_pr_p0_outputs/lhn_005/', $
            '/local/raid/armadja/data/lhn_pr_p0_outputs/lhn_006/']
prev_time = 3d      ;hours of forecast before analysis time
fcst_time = 9d     ;hours of forecast after  analysis time

;initialize control structure
n_fcsts = n_elements(analy_hours_txt)
n_exp   = n_elements(exp_list)

;noedit---------------
ctl = {fcst_time_txt:''         , $
       t0:0d,  tf:0d            , $
       exp_list  : strarr(n_exp), $
       loc       : ''           , $
       exp_desc  : strarr(n_exp), $
       model_res : 0d           , $
       pan_dt    : 0d           , $
       nat_dt    : 0d           , $
       pr_dt     : 0d           , $
       img_dt    : 0d           , $
       diff      : 0            , $
       comp_score: 0            , $
       plot_score: 0            , $
       no_pic    : 0            , $
       num_cpus  : 0d}
;---------------------

ctl.loc        = 'can_us_radars'
;ctl.loc        = 'mid_us_closeup'
ctl.exp_desc   = ['lhn_moistf=.5','profiles=.5']
ctl.model_res  = 10.    ;model resolution in km
ctl.pan_dt     = 1d
ctl.nat_dt     = 5d     ;model temporal resolution
ctl.pr_dt      = 5d     ;accumulation time for precip
ctl.img_dt     = 10d    ;temporal resolution of scores and figures in minutes
ctl.diff       = 0      ;switch to activate diff mode
ctl.num_cpus   = 36     ;number of cpus for parallel computation

;call anim_pandom for each forecast time
for tt=0, n_fcsts-1 do begin
    this_time_txt = analy_hours_txt[tt]
    yy = long(strmid(this_time_txt,0,4))
    mo = long(strmid(this_time_txt,4,2))
    dd = long(strmid(this_time_txt,6,2))
    hh = long(strmid(this_time_txt,8,2))
    print, yy, mo, dd, hh
    this_time_jul = julday(mo, dd, yy, hh, 0, 0)
    ctl.t0 = this_time_jul-prev_time/24d
    ctl.tf = this_time_jul+fcst_time/24d
    ctl.exp_list = exp_list+this_time_txt+'/'
    ctl.fcst_time_txt = this_time_txt

    ctl.comp_score = 1      ;switch to compute verif scores
    ctl.plot_score = 0      ;switch to display previously computed scores
    ctl.no_pic     = 1      ;switch to prevent the making of figures
    anim_pandom, ctl=ctl, serial=0, from_sav=0

    ;ctl.comp_score = 0      ;switch to compute verif scores
    ;ctl.plot_score = 1      ;switch to display previously computed scores
    ;ctl.no_pic     = 0      ;switch to prevent the making of figures
    ;anim_pandom, ctl=ctl, serial=0, from_sav=0;, /noerase;, /proj_gen


endfor

end
