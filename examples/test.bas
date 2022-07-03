# Grab some input
PRINT "How \b many fibonacci numbers do you want?"
INPUT nums # test end of line comment

# Initialize the generator
LET a = 0 # hey ho test end of line comment here
LET b = 1

# Generate numbers until we're supposed to stop

WHILE nums > 0 REPEAT
    PRINT a
    LET c = a + b
    LET a = b
    LET b = c
    LET nums = nums - 1
ENDWHILE