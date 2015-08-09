ECHO OFF

echo    ASSEMBLE                    >  __ssx.s
echo        NIBASC  /HPHP48-X/          >> __ssx.s
echo        INCLUDE __head.a            >> __ssx.s
echo        LISTM                   >> __ssx.s
echo    RPL                     >> __ssx.s
echo    INCLUDE ssx.s                >> __ssx.s
echo    ASSEMBLE                    >> __ssx.s
echo        CLRLIST INCLUDE             >> __ssx.s
echo        INCLUDE __hash.a            >> __ssx.s
echo        INCLUDE __end.a             >> __ssx.s
echo    RPL                     >> __ssx.s

echo Assembling rpl..
RPLCOMP __ssx.s __ssx.a __ssx.ext

echo    TITLE STKSOLVE Library            > __mkssx.mn
echo    OUTPUT __xx.o               >> __mkssx.mn
echo    LLIST __xx.lr               >> __mkssx.mn
echo    CONFIGURE SScfg            >> __mkssx.mn
echo    NAME STKSOLVE             >> __mkssx.mn
echo    ROMPHEAD __head.a           >> __mkssx.mn
echo    REL __ssx.o               >> __mkssx.mn
echo    TABLE __hash.a              >> __mkssx.mn
echo    FINISH __end.a              >> __mkssx.mn
echo    END                 >> __mkssx.mn

echo Building tables..
MAKEROM __mkssx.mn __ssx.m

echo Assembling code..

SASM -E __ssx

echo    TITLE STKSOLVE Library        > __mkssx.m
echo    OUTPUT STKSOLVE.LIB     >> __mkssx.m
echo    OPTION CODE         >> __mkssx.m
echo    LLIST  __SSX.LR         >> __mkssx.m
echo    SUPPRESS XR         >> __mkssx.m
echo    SEARCH ENTRIES.o    >> __mkssx.m
echo    REL __SSX.O           >> __mkssx.m
echo    CK LIB60E SYSEND60E     >> __mkssx.m

echo Linking library.. 
SLOAD -H __mkssx.m


