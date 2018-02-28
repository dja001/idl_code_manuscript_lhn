pro test_gem
;test content of standard files

file='/local/drive2/arma/armadja/data/tmp/operation.ensemble.ens.regmodel/2012070100_048_001'
jul = JULDAY(07,03,2012,00,00,00)
JUL_TO_CMC,jul,cmc
GET_GEM_DATA, file,   VAR_NAME='HU',VALUES=hu, P_FROM_VAR=pres, CMC_TIMESTAMP=cmc

print, pres[100,100,*]

end
