PRO LOAD_RMN_LIB
    ;load EC RMN lib for IDL based on idl version
    IF (!VERSION.RELEASE GE 8.1) THEN BEGIN
        initrmnlib 
    ENDIF ELSE BEGIN 
        new_initrmnlib   
    ENDELSE
END
