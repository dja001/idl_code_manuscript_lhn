PRO LINMAP, in, out, range_in, range_out
;does a linear mapping of in values to out values given specified ranges
;in "in" values below and above initial range are set to lowest/highest range_out values

;range_in MUST be [low, high] while range_out can be anything



x1 = double(range_in[0])
x2 = double(range_in[1])

y1 = double(range_out[0])
y2 = double(range_out[1])

m = (y2 -y1)/(x2 - x1)
b = (x2*y1 - x1*y2)/(x2-x1)

out = m*in + b

aa = WHERE(in LT x1, count)
IF count NE 0 THEN BEGIN
    ;print, "linmap.pro        in values below x1 check that this is ok"
    out[aa] = y1
ENDIF
aa = WHERE(in GT x2, count)
IF count NE 0 THEN BEGIN
    ;print, "linmap.pro        in values below x1 check that this is ok"
    out[aa] = y2
ENDIF






END
