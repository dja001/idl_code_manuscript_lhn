PRO ERR_CATCH, unit, key
    IF key LT 0 THEN BEGIN
        ;-ve key, something went wrong
        stat = FSTFRM(U=unit)
        MESSAGE, '-ve key, requested field does not exist. Aborting.'
    ENDIF
END
