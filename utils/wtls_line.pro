;. ------------------ subfunction ------------------------------------------
FUNCTION chialpha, alpha
    uk2=!ux^2.*SIN(alpha)^2.+!uy^2.*COS(alpha)^2.
    u2=1./MEAN(1./uk2)
    w=u2/uk2
    xbar=MEAN(w*!xx)
    ybar=MEAN(w*!yy)
    p=ybar*COS(alpha)-xbar*SIN(alpha)
    chi=TOTAL((!yy*COS(alpha)-!xx*SIN(alpha)-p)^2./uk2)
    return, chi
END
; ----------- end of subfunction ------------------------------------------


PRO wtls_line, xin, yin, uxin, uyin, COEF_A=a, COEF_B=b, ALPHA=alpha, P=p, CHIOPT=chiopt, CAB=cab, CALPHAP=calphap

; weighted total least squares (wtls) fit of a straigth line 
; to a set of points with uncertainties in both coordinates
;
; input:        xin     abscissa vector
;               yin     ordinate vector
;               uxin    (standard) uncertainties of xin, same size as xin
;               uyin    (standard) uncertainties of yin, same size as yin
;
; output:       a, b    usual straight line parameters
;                       y=a*x+b
;               alpha,p more stable parametrisation
;                       y*cos(alpha)-x*sin(alpha)-p=0
;                       alpha: slope angle in radians
;                       p: distance of straight line from (0,0)
;                       conversion: a=tan(alpha),b=p/cos(alpha)
;               chiopt  minimum chisquare found
;               Cab     covariances, [var(a),var(b),cov(a,b)]
;               Calphap covariances, [var(alpha),var(p),cov(alpha,p)]
;
; algorithm:    M. Krystek & M. Anton
;               Physikalisch-Technische Bundesanstalt Braunschweig, Germany
;               Meas. Sci. Tech. 18 (2007), pp3438-3442
;
; tested for Matlab 6 and Matlab 7
; testdata: script file pearson_york_testdata.m
;
; 2007-03-08
;
;IDL translation Dominik Jacques 2009
; call example with only usefull parameters
;wtls_line, xin, yin, uxin, uyin, COEF_A=a, COEF_B=b, CAB=cab


tol=1e-7; %"tolerance" parameter of fnimbnd, see there
pi =  3.14159265

DEFSYSV, '!xx', xin
DEFSYSV, '!yy', yin
DEFSYSV, '!ux', uxin
DEFSYSV, '!uy', uyin


; force column vectors
;x=xin(:);
;y=yin(:);
;ux=uxin(:);
;uy=uyin(:);

; "initial guess"
p0 = REGRESS(!xx,!yy, CONST=const)
p0 = [p0, const]
alpha0=ATAN(p0[0])

; one-dimensional search, use p=p^
ss = min_f_golden(FNAME='chialpha', BOUNDS=[alpha0-pi/2.,alpha0+pi/2.],TOL=tol)
alphaopt = ss[0]
chiopt = ss[1]

; get optimum p from alphaopt
alpha=alphaopt
uk2=!ux^2.*SIN(alpha)^2.+!uy^2.*COS(alpha)^2.
u2=1./MEAN(1./uk2)
w=u2/uk2
xbar=MEAN(w*!xx)
ybar=MEAN(w*!yy)
p=ybar*COS(alpha)-xbar*SIN(alpha)

; convert to a, b parameters !yy=a*x+b
a=sin(alpha)/cos(alpha)
b=p/cos(alpha)

; --- uncertainty calculation, covariance matrix = 2*inv(Hessian(chi2)) ---
n=N_ELEMENTS(!xx)
vk=!yy*COS(alpha)-!xx*SIN(alpha)-p
vka=-!yy*SIN(alpha)-!xx*COS(alpha)
vkaa=-vk-p
fk=vk*vk
fka=2.*vk*vka
fkaa=2.*(vka^2.+vk*vkaa)
gk=uk2
gka=2.*SIN(alpha)*COS(alpha)*(!ux^2.-!uy^2.)
gkaa=2.*(!ux^2.-!uy^2.)*(COS(alpha)^2.-SIN(alpha)^2.)
Hpp=2.*n/u2
Halphap=-2.*TOTAL((vka*gk-gka*vk)/gk^2.)
Halphaalpha=TOTAL(fkaa/gk-2.*fka*gka/gk^2.+2.*gka^2.*fk/gk^3.-gkaa*fk/gk^2.)
NN=2./(Hpp*Halphaalpha-Halphap^2.)
var_p=NN*Halphaalpha
var_alpha=NN*Hpp
cov_alphap=-NN*Halphap
Calphap=[var_alpha,var_p,cov_alphap]

; ------ convert to a & b covariance matrix, following DIN 1319 (4)
var_a=var_alpha/COS(alpha)^4.
var_b=(var_alpha*p*p*SIN(alpha)^2.+var_p*COS(alpha)^2.+ 2.*cov_alphap*p*SIN(alpha)*COS(alpha))/COS(alpha)^4.
cov_ab=(var_alpha*p*SIN(alpha)+cov_alphap*COS(alpha))/COS(alpha)^4.
Cab=[var_a,var_b,cov_ab]
; ------ end of uncertainty calculation -----------------------------------

END
