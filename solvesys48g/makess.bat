ECHO OFF

echo    ASSEMBLE                    >  __ss.s
echo        NIBASC  /HPHP48-X/          >> __ss.s
echo        INCLUDE __head.a            >> __ss.s
echo        LISTM                   >> __ss.s
echo    RPL                     >> __ss.s
echo    INCLUDE ss.s                >> __ss.s
echo    ASSEMBLE                    >> __ss.s
echo        CLRLIST INCLUDE             >> __ss.s
echo        INCLUDE __hash.a            >> __ss.s
echo        INCLUDE __end.a             >> __ss.s
echo    RPL                     >> __ss.s

echo Assembling rpl..
RPLCOMP __ss.s __ss.a __ss.ext

echo    TITLE SOLVESYS Library            > __mkss.mn
echo    OUTPUT __xx.o               >> __mkss.mn
echo    LLIST __xx.lr               >> __mkss.mn
echo    CONFIGURE SScfg            >> __mkss.mn
echo    NAME SOLVESYS             >> __mkss.mn
echo    ROMPHEAD __head.a           >> __mkss.mn
echo    REL __ss.o               >> __mkss.mn
echo    TABLE __hash.a              >> __mkss.mn
echo    FINISH __end.a              >> __mkss.mn
echo    END                 >> __mkss.mn

echo Building tables..
MAKEROM __mkss.mn __ss.m

echo Assembling code..

SASM -E __ss

echo    TITLE SOLVESYS Library        > __mkss.m
echo    OUTPUT SOLVESYS.LIB     >> __mkss.m
echo    OPTION CODE         >> __mkss.m
echo    LLIST  __SS.LR         >> __mkss.m
echo    SUPPRESS XR         >> __mkss.m
echo    SEARCH ENTRIES.o    >> __mkss.m
echo    REL __SS.O           >> __mkss.m
echo    CK LIB60E SYSEND60E     >> __mkss.m

echo Linking library.. 
SLOAD -H __mkss.m

type __SS.LR

ECHO Copying file to EMULATOR

copy solvesys.lib d:\HP48\HP48EMU\

ECHO Press CTRL+C to abort
PAUSE
