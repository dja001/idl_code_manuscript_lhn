function median_val, data, ind=ind
;returns median of an array of values
;it is assumed that all data points are good

if n_elements(data) eq 0 then message, 'must provide some data '
sz = size(data,/dim)
n_dim = n_elements(sz)
if n_dim gt 1 then message, 'median_val only works with 1D arrays'

;size of input array
n_elem = sz[0]

bb = sort(data)
ind  = bb[n_elem/2]              ;index of selected darta point
return,   data[bb[n_elem/2]]     ;median 

end
