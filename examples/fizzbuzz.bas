PRINT "How many iterations? "
INPUT max_iter

LET i = 1

WHILE i <= max_iter REPEAT
    IF i / 3 == 1 THEN
        IF i / 5 == 1 THEN
            PRINT "fizzbuzz"
            GOTO loop_end
        ENDIF

        PRINT "fizz"
        GOTO loop_end
    ENDIF

    IF i / 5 == 1 THEN
        PRINT "buzz"
        GOTO loop_end
    ENDIF

    PRINT i

    LABEL loop_end
    LET i = i + 1
ENDWHILE
