function rot_mat, c, alpha
;constructs a rotation matrix (on the sphere) using:
;
;   c       a [x,y,z] point on a UNIT sphere. 
;   alpha   a rotation angle in radians

return,     cos(alpha)  * diag_matrix(replicate(1d,3))     + $
       (1d -cos(alpha)) * matrix_multiply(c,c,/btranspose) + $
            sin(alpha)  * [[0.,-c[2],c[1]],[c[2],0.,-c[0]],[-c[1],c[0],0.]]
end

