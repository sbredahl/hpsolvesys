*  SOLVESYS 49 Copyright 1999,2000 Sune Bredahl 
*
*  SOLVESYS 49 is free software: you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  SOLVESYS 49 is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with SOLVESYS 49. If not, see <http://www.gnu.org/licenses/>.
*
*  The original, unmodified, source code goes here:

RPL ( Ensure RPLCOMP and the syntax highlighter are in RPL mode )
( test.s, part of the test.s project, created by <> on 22-09-99 )

INCLUDE solve49.H

* !!!!! unsupported entries !!!!!!

ASSEMBLE

EQ>             EQU #2F25E
~FASTMAT/       EQU #0520C2   ( XLIB 194 82 or ROMPTR 0C2 052 )
^NUMMAT/        EQU #0049003  ( flashptr )
^MATLSQ         EQU #002B003  ( flashptr )
~SORTLIST       EQU #0160E8 

* Variable/LAM aliases

RPL

* global LAM's used in solver, Inputform etc.

DEFINE getF       LAM F
DEFINE putF       ' LAM F STOLAM
DEFINE getcos     LAM c
DEFINE putcos     ' LAM c STOLAM
DEFINE getdx      LAM d
DEFINE putdx      ' LAM d STOLAM
DEFINE geteqlist  LAM E
DEFINE puteqlist  ' LAM E STOLAM
DEFINE geteqeval  LAM E EVAL
DEFINE getvarlist LAM v
DEFINE putvarlist ' LAM v STOLAM
DEFINE getfullvarlist LAM l
DEFINE putfullvarlist ' LAM l STOLAM
DEFINE geteqIDX   LAM r
DEFINE puteqIDX   ' LAM r STOLAM

* nulllams used only in SOLVER (do not exist outside)

DEFINE getf       1GETLAM         ( SOLVER )
DEFINE putf       1PUTLAM         ( SOLVER )
DEFINE geteqnum   12GETLAM        ( # number of equations )
DEFINE puteqnum   12PUTLAM
DEFINE getfold2   10GETLAM         ( SOLVER )
DEFINE putfold2   10PUTLAM         ( SOLVER )
DEFINE puttmp     9PUTLAM         ( SOLVER )
DEFINE gettmp     9GETLAM         ( SOLVER )
DEFINE getalam2   8GETLAM         ( SOLVER )
DEFINE putalam2   8PUTLAM         ( SOLVER )
DEFINE getalam    7GETLAM         ( SOLVER )
DEFINE putalam    7PUTLAM         ( SOLVER )
DEFINE getP       6GETLAM         ( SOLVER )
DEFINE putP       6PUTLAM         ( SOLVER )
DEFINE getXold    5GETLAM         ( SOLVER )
DEFINE putXold    5PUTLAM         ( SOLVER )
DEFINE getf2      4GETLAM         ( SOLVER )
DEFINE putf2      4PUTLAM         ( SOLVER )
DEFINE getmaxstep 3GETLAM         ( SOLVER )
DEFINE putmaxstep 3PUTLAM         ( SOLVER )
DEFINE putfold    2PUTLAM
DEFINE getfold    2GETLAM
DEFINE getgrad    11GETLAM          ( SOLVER )
DEFINE putgrad    11PUTLAM          ( SOLVER )
DEFINE getvarnum  13GETLAM
DEFINE putvarnum  13PUTLAM
DEFINE getX       14GETLAM
DEFINE putX       14PUTLAM

DEFINE SOLVEVARS  FOURTEEN        ( number of LAMS to be cached )

* shortcuts

DEFINE SORT{}     ROMPTR SORTLIST
DEFINE SetNum     THREE SetSysFlag
DEFINE ClrNum     THREE ClrSysFlag         ( set symbolic results )
DEFINE SetApprox  FLASHPTR CLREXACT
DEFINE ClrApprox  FLASHPTR SETEXACT
DEFINE #>%        UNCOERCE
DEFINE %>#        COERCE
DEFINE %SQ        DUP %*
DEFINE MATABS     FLASHPTR MATFNORM
DEFINE PULLEL     FLASHPTR PULLEL[S] ( both cmp & real )
DEFINE XEQ>ARRAY  FLASHPTR XEQ>ARRY
DEFINE MSGBOX     FLASHPTR Ck&DoMsgBox
DEFINE ClrSilent  BINT120 ClrSysFlag

NULLNAME CONFIG :: #60E TOSRRP ; (  autoattach lib to HOME )
NULLNAME EXTPRG :: DUP #5= NOT?SEMI SWAP { "SOLVESYS 1.2" xSOLVESYS } >TCOMP SWAP ;

* NULLNAME Messages ARRY [
* "SOLVESYS 49 1.2 BETA 02\0ACopr. 1994-2000\0A  Sune Bredahl" ]

xNAME SOLVESYS
::
CK0

* make sure EQ{} exists!

 ::
 EQUATION                        ( fecth EQ if existing )
 NOTcase NULL{}                 ( if not, exit and return NULL{} )
 DUPTYPESYMB?                ( if EQ is ok, check if list )
 case ONE{}N
 DUPTYPELIST?                ( if EQ is ok, check if list )
 NOTcasedrop NULL{}             ( if not - exit. drop EQ and return NULL{} )
 ;
 xSTEQ

* start the EQ manager

' ::    SIXTY #=casedrop TrueTrue  ( reply "fullscreen" )
        SIXTYONE #=casedrop TrueTrue
        SIXTYTWO #=casedrop :: EQUATION DROP LENCOMP TRUE ;
        BINT80 #=casedrop :: EQUATION DROPSWAP NTHCOMPDROP TRUE ;
        BINT83 #=casedrop                   ( specify menu )
        ::
        ' ::  NoExitAction
        { { "ADD" ADDKEY }
          { "EDIT" EDITKEY }
          { "DEL" DELKEY }
          { "COPY" COPYKEY }
          { "CLEAR" CLEARKEY }
          { "OK" OKKEY } }
          ;
        TRUE
        ;
        BINT85 #=casedrop :: 12GETLAM MAKEHL 15PUTLAM TRUE ;
        BINT96 #=casedrop :: EDITKEY
        SetDA2aBad
        SetDA2bBad
        SetDA3Bad
        FalseTrue ;        ( ENTER not allowed because it copies obj to stk )
        DROPFALSE                       ( further msgs not handled )
  ;                                     ( end if msg handler )

"Equations:"                   ( "current equations" )
FOUR
NULL{}
ONE                             ( focus pos )
ROMPTR Choose
DROP
;

* keyobjects for the equation editor

NULLNAME ADDKEY
::
FLASHPTR EQW3                   ( start eq writer )
NOT?SEMI                       ( exit if cancelled )
DUPTYPESYMB?                   ( check if input is an alg )
NOTcaseDROP                    ( if not, drop obj and exit )
EQUATION DROPSWAP >TCOMP xSTEQ  ( else append to EQ )
12GETLAM #1+ 12PUTLAM          ( and ITEMS )
15GETLAM 12GETLAM >TCOMP 15PUTLAM ( update picked )
;

NULLNAME EDITKEY
::
12GETLAM #0=case DoBadKey
18GETLAM GetEqN DROP           ( get n'th eqn )
FLASHPTR EQW3Edit
NOT?SEMI
DUPTYPESYMB?
NOTcaseDROP                   ( if not alg, drop notalg and exit )
18GETLAM EQUATION DROP       ( 'editeq' #higlight {eqlist} )
PUTLIST      ( replace equation in list )
xSTEQ           ( save new list )
* SetDA2aBad
* SetDA2bBad
* SetDA3Bad
;

NULLNAME DELKEY
:: 
12GETLAM #0=case DoBadKey
EQUATION DROP 18GETLAM               ( {eqlist} #highligt )
REMOVE{}                        ( remove subject )
xSTEQ
                ( reinit all highlights )
* 19PUTLAM                        ( update list )
* 15GETLAM DUP                       ( retrieve highlight list )
* 18GETLAM #=POSCOMP
* REMOVE{} 15PUTLAM

12GETLAM #1-DUP 12PUTLAM           ( update #items' count )
MAKEHL 15PUTLAM
ONEONE 6PUTLAM 18PUTLAM         ( update focus )
;

NULLNAME COPYKEY
::
15GETLAM LENCOMP #0=case DoBadKey
EQUATION DROPDUP                                    ( eqlist eqlist )
18GETLAM NTHCOMPDROP                                 ( eqlist eqn )
>TCOMP xSTEQ                                      ( add eq to list and save )
12GETLAM #1+ 12PUTLAM                           ( store items_list' )
15GETLAM 12GETLAM >TCOMP 15PUTLAM ( update picked )
SetDA2aBad
SetDA2bBad
;

NULLNAME CLEARKEY
::
15GETLAM LENCOMP #0=case DoBadKey
"DELETE ALL?" AskQuestion NOT?SEMI
NULL{} xSTEQ       ( fetch item_list )
ZERO 12PUTLAM
NULL{} 15PUTLAM
ONEONE 6PUTLAM 18PUTLAM ( forgot this in version 4.02... )
SetDA2aBad
SetDA2bBad
;

NULLNAME OKKEY
:: 15GETLAM LENCOMP #0=case DoBadKey 15GETLAM GUISHELL ;

NULLNAME REMOVE{}
:: DUP#0=csDROP SWAP INNERDUP #2+ROLL OVERSWAP #- #2+ROLL DROP #1-{}N ;

*************** END OF EQUATION EDITOR SECTION ***********************

xNAME ABOUTSS :: CK0 "SOLVESYS 49 1.2\0Abuild: 17/07/00\0ACopr. 1994-2000\0A  Sune Bredahl"
MSGBOX ;

********************* end of Xnames ****************************************

NULLNAME GUISHELL ( 1: list of picked eqs )
::
SaveSysFlags
0LASTOWDOB! ( Clear last command xSTEQ )
* DEPTH #1-
* DUP ' ID YY STO
* stack is 2: eqlist, 1: depth#-1

DEPTH #1- ZEROZEROZERO ZEROZEROZERO % 1E-5 % 1E-3 DUP %10*
{ LAM r LAM D LAM l LAM c LAM d LAM E LAM F LAM v LAM o LAM p LAM q } BIND                ( save globals )

           ( save globals )



*** old getvars method ***

NULL{} 1LAMBIND                                   ( save list to hold vars )
geteqIDX LENCOMP #1+_ONE_DO (DO)
geteqIDX INDEX@ NTHCOMPDROP GetEqN
DROP DUPTYPESYMB? NcaseTYPEERR
INNERCOMP                                     ( if so, then decomp )
    #1+_ONE_DO (DO)                               ( and update varlist ... )
    DUPTYPEIDNT?
    :: NOTcaseDROP 1GETSWAP apndvarlst 1PUTLAM ;
   LOOP                                          ( next atomic )
LOOP                                              ( next eq )
1GETABND                                          ( get final list )
SORT{}                                          ( optional - sort the list )
DUPNULL{}? caseSIZEERR                 ( no vars then error )

*** NEW getvars method ****

* LOOP geteqIDX LENCOMP {}N

* ClrNum ClrApprox
* FLASHPTR USERLIDNT                      ( return sorted list of vars )
* DUPNULL{}? caseSIZEERR                 ( no vars then error )
* FLASHPTR []TO{}

putvarlist
* DROP                       ( equation list not needed )
* puteqlist                ( update basic vars and simple checks )
* end of eq var detection

* ClrDA1IsStat                                                            ( suspend clock disp. updates )
* BINT72 ClrSysFlag       ( normal stack font )
NEWGUI
ABND
RestoreSysFlags
;

***********************************************************************************
*  PACKEQ ( -> )
* - combines 'EQ' list into single seco object
* - resolves userdefined constants and non-user constants. eg. PI or CONST reference
* - replaces integers with reals
* - Minor checks for empty or obsolete equations

NULLNAME PACKEQ
::
THREE ClrSysFlag                                ( 'alg' -> 'alg' )
TWO SetSysFlag                                  ( 'pi*alg' -> '3.14*alg' )
SIXTYONE SetUserFlag                            ( make sure 'CONST' -> %val )

geteqIDX LENCOMP
#1+_ONE_DO (DO)
geteqIDX INDEX@ NTHCOMPDROP GetEqN DROP
getvarlist
XEQSHOWLS       ( eval everything but vars )
EQ> x-                                         ( rewrite 'rhs=lhs' as 'rhs-lhs' )
DUPTYPESYMB? NOTcasedrop :: #A02 ERROROUT ;     ( if not still symbolic, then error! )
* replaces integers with reals
        INNERDUP 1LAMBIND
        #1+_ONE_DO (DO)
        INDEX@ PICK
        TYPEZINT? IT :: INDEX@ ROLL FLASHPTR Z>R INDEX@ UNROLL ;
        LOOP
        1GETABND ::N
* [#] DOCOL CHANGETYPE
LOOP
geteqIDX LENCOMP ::N puteqlist                           ( store eq as SECO object )
;

************************************************************************************************
NULLNAME INFOKEY
::      
"m/n=" geteqIDX LENCOMP #>$ &$ CHR_/ >T$ 15GETLAM LENCOMP #>$ &$ NEWLINE$&$
"›x=" getdx DUPTYPEBINT? ITE_DROP :: a%>$ &$ ; NEWLINE$&$ &$
"eq=" getF DUPTYPEBINT? ITE_DROP :: MATABS a%>$ &$ ; NEWLINE$&$ &$
"lsq=" getcos DUPTYPEBINT? ITE_DROP :: a%>$ &$ ; &$
MSGBOX
;  

***************************************************************************

NULLNAME TOLKEY
::
NULL$ ( #BF26 )
{
{ "XTOL" :: 'DROPFALSE NULL$ FOUR { % 1E-1 % 1E-3 % 1E-5 } TWO ROMPTR Choose NOT?SEMI ' LAM p STO ; }
{ "EQTOL" :: 'DROPFALSE NULL$ FOUR { % 1E-3 % 1E-5 % 1E-7 } TWO ROMPTR Choose NOT?SEMI ' LAM o STO ; }
{ "LSQTOL" :: 'DROPFALSE NULL$ FOUR { % 0.1 % 0.01 % 0.001 } TWO ROMPTR Choose NOT?SEMI ' LAM q STO ; }
}
FLASHPTR RunChooseSimple NOT?SEMI
EVAL
;

******************************************************************************
NULLNAME SOLVER ( [xinit] -> )
::
ZERO SOLVEVARS NDUPN ROMPTR nNullBind ( ' NULLLAM CACHE )
putX
* begin initpart
geteqlist LENCOMP puteqnum
getvarlist LENCOMP putvarnum
getX StoreX                                  ( save start values, 1:[X0] )
MATABS getvarnum #>% %MAX %100 %* putmaxstep  ( maxstp=10E2*max[X0] )
FVEC                                    ( compute [F] and %f )
getF FLASHPTR CKNUMARRY
DispEqVal
* end initpart

BEGIN
getF
JACOBI                            ( F TRN[J] )
geteqnum getvarnum #=ITE

* FLASHPTR MATTRAN
* :: DUP3PICK FLASHPTR MAT* putgrad DUPROT FLASHPTR MAT* SWAPDUP FLASHPTR MATTRAN FLASHPTR MAT* ;
* SWAPOVER                               ( b A b A )
* ERRSET
* :: ROMPTR FASTMAT/ SWAPDROP ;    ( try fast linear solve first )
* ERRTRAP
* ::
* ERROR@ #305 #<>case ERRJMP
* CHR_+ CHR>$ DISPLASTROWBUT1
* SWAPDROPSWAP FLASHPTR MATLSQ ( failed? > use pseudoinverse )
* ;
::
        FLASHPTR MATTRAN SWAPOVER
        ERRSET                               ( b A b A )
        :: ROMPTR FASTMAT/ SWAPDROP ;    ( try fast linear solve first )
        ERRTRAP
        ::
        ERROR@ #305 #<>case ERRJMP
        CHR_+ CHR>$ DISPLASTROWBUT1
        SWAPDROPSWAP FLASHPTR MATLSQ ( failed? > use pseudoinverse )
        ;
;
:: DUP3PICK FLASHPTR MAT* putgrad FLASHPTR MATTRAN FLASHPTR MATLSQ ;

putP                                    ( store Newton direction [-P] )
getf putfold                            ( update fold for linesearch )
getX putXold                            ( update Xold for linesearch )
LINESEARCH                              ( updates X and f and F )
DispEqVal
TESTTOL                                                 ( check result )
AGAIN
;

*********************************************************************************************

NULLNAME LINESEARCH ( -> )
::
getP DUP                                        ( [-P] [-P] )
MATABS DUP getmaxstep                   ( [-P] %newlen %newtlen %maxstep )
%> ITE                                  ( Newtlen > maxstep? ) 
:: getmaxstep SWAP %/ FLASHPTR MAT*SCL putP  ;      ( if so [-P]=[maxstep/newlwn]*[-P] )
2DROP                                           ( else drop: [-P] %newtlen )

%1 putalam                                      ( k=1 for the first step )

BEGIN
getalam % 1E-7 %< IT :: TESTTOL #A01 ERROROUT ;
getXold                                 ( [Xold] )
getP getalam FLASHPTR MAT*SCL          ( [Xold] k*[-P] )
FLASHPTR MAT-                                    ( [Xold]-k*[-P] )
StoreX putX                            ( store X in globals, leaves [X] on stack )
FVEC                                    ( compute F and f at new X )
** NO NONSENSE EXIT TEST FOR f=0 !!!

getf %0=case :: DROP #A04 ERROROUT ;    ( in fact DUP%0=case - exit test for f=0 !!! )

* EQUALcase :: 2DROP TESTTOL #A01 ERROROUT ;
*       DUP                                     ( f fold fold )
getfold

* getvarnum geteqnum #<> ?SKIP
* :: DUP % 1E-4 %* getalam %* %- ; not a good test for lsq probs
%>


WHILE                                           
        getalam 
        %1 EQITE                                ( quad or cubic backtrack? )
       ::
                CHR_* CHR>$ DISPLASTROWBUT1         ( k=-slope/[2*{f-fold-slope}] )
                getf getfold %/ %1+ %1/         ( slope=-2fold -> k=1/[fnew/fold+1] )
                                                        ( store in TMP lambda )
        ;
        ::
        "**" DISPLASTROWBUT1                    ( display cubic backtrack )
        getf getalam %2 %* %1-                  ( rhs1=f-fold-k1*slope..., slope=-2*fold )
                getfold %* %+                   ( ie. rhs1=f+fold*[2k-1] ) 
        
                getf2 getfold2 %-               ( rhs2=f2-fold2-alam2*slope ) 
                getfold %2 %*                   ( ie. rhs2=f2-fold2+2*alam2*fold )
                getalam2 %* %+

                SWAP2DUP                        ( rhs2 rhs1 rhs2 rhs1 )
                getalam %SQ %/                  ( rhs2 rhs1 rhs2 rhs1/k1^2 )
                SWAP getalam2 %SQ %/ %-         ( rhs2 rhs1 rhs1/k1^2-rhs2/k2^2 )                                       
                getalam getalam2 %- %/          ( rhs2 rhs1 [rhs1/k1^2-rhs2/k2^2]/[k1-k2] )
                3UNROLL                         ( a rhs2 rhs1 )

                getalam2 %* %CHS 
                getalam %SQ %/                  ( a rhs2 -k2*rhs1/k1^2 )
                SWAP getalam %*                 ( a -k2*rhs1/k1^2 k1*rhs2 )
                getalam2 %SQ %/ %+              ( a -k2*rhs1/k1^2+k1*rhs2/k2^2 )
                getalam getalam2 %- %/          ( a b )         
                SWAP                           ( b a )
                %0=case                         ( b a )
                :: ( DROP ) getfold SWAPDROPSWAP %/ ;    ( k=-slope/2b = fold/b )
                SWAP %CHS 2DUP %SQ SWAP      ( a -b b^2 a )
                %6 %* getfold %* %+     ( b^2-3*a*slope = b^2+6*a*fold ) 
                %SQRT %+SWAP %3 %* %/  ( -b+sqrtd/3a )
        ;
        getalam %2 %/ %MIN
        puttmp                  ( if tmplam>alam/2 then tmplam=alam/2 )
        getalam putalam2
        getf putf2
        getfold putfold2
        gettmp getalam %10 %/ %MAX putalam
REPEAT                  
;               

*******************************************************************************************
NULLNAME DispEqVal :: "eq=" getf %2 %* %SQRT a%>$ &$ DISPLASTROWBUT1 ;

* FVEC ( -> )
* makes the F vector of f errors as well overall error f=½absF^2

NULLNAME FVEC
::
        geteqlist EVAL                         ( fn ... f1 )
        geteqnum #>% %1 TWO{}N XEQ>ARRAY       ( [ fn ... f1 ] )
        DUP putF                               ( update [F] )
        MATABS %SQ %2 %/ putf                  ( update %f )
;

*******************************************************************************************

NULLNAME StoreX ( 1:[X] -> 1:[X] )
::
DUP TYPERARRY?
?SKIP
:: FLASHPTR COMPLEXON TURNMENUOFF ;
getvarlist >R                           ( put variable list in returnstack )
getvarnum                                ( [X] #n )
#1+_ONE_DO (DO)                         ( for all variable do )
        INDEX@ PULLEL RSWAP 'R XEQSTOID RSWAP              ( store i'th var )
LOOP                                    ( next var )
RDROP                                   ( drop varlist in rstk )
;
******************************************************************************************

NULLNAME TESTTOL
::                      
* remark: overall f=0 test is also in LINESEARCH routine
getX getXold FLASHPTR MAT- MATABS
getX MATABS %1 %MAX %/ DUP putdx                 ( save relative difference )
LAM p %> ?SEMI                             ( deltaX is too large => skip further tests... )
getf %2 %* %SQRT LAM o %< IT :: #A04 ERROROUT ;  ( test for f=0 )
geteqnum getvarnum #= ?SEMI                      ( skip further test if this is a zeroing problem )
getgrad FLASHPTR MATTRAN getP FLASHPTR MAT* MATABS ( otherwise compute gradient )
getfold %2 %* %/ %SQRT DUP putcos
LAM q %> ?SEMI                          ( cos test not satiesfied => exit )
#BB0D ERROROUT                          ( else mimimum )
;
*****************************************************************************************


NULLNAME JACOBI ( -> TRN[J] )
::
*  Computes J^t using forward diff. jacobian approx. f'(x) = [f(x+h)-f(x)]/h
*  dF1/dX1 = [ F1(x1+h,x2,x3...)-F1(x1,x2,x3....) ]/h
*  h=eps*sign(x)*max[abs(Xi),1]
*  Note that 'h' used in denominator is actually (x+h)-x = h (better accurary)

getvarnum                                        (  #n )
#1+_ONE_DO (DO)                                 ( for all vars do )
        getvarlist INDEX@ NTHCOMDDUP              (  #n )
        EVAL DUPDUP DUP TOTEMPOB 4UNROLL       ( 'Xi' %x* %x %x %x )
* note %x* is the originaæ value and is not changed by REPLACE
        DUPTYPEREAL? ITE 
        :: %0=case %1+ DUP %SGN SWAP %ABS %1 %MAX %* ; ( h=eps*sign[x]*[max{ABS[x],1}] )
*       :: DUP %SGN SWAP %ABS %1 %MAX %* ;
        :: C%ABS %1 %MAX ;
        % 1E-6 %*                                ( h=eps*sign[x]*[max{ABS[x],1}] )
        x+                                       ( 'X' %x* %x %x+step )
        SWAP                                     ( 'X' %x* *x+step %x )
        REPLACE                                  ( 'X' %x* *x+step )
        DUP3PICK                                ( 'X' %x* *x+step x+step )
        %-                                      ( refind h as x+h-x - also works if x is complex )
        geteqlist >R                            ( put eq in rstk )
        geteqnum
        #1+_ONE_DO (DO) 
                RSWAP 'R RSWAP                  ( 'Xi' %x* %x %step EQi )
                5PICK OVER                      ( 'Xi' %x* %x %step eqi Xi eqi )
                ::
                matchob? NOTcase2drop %0                 ( not in eq, drop eqi and x1 )
                EVAL                            ( else eval EQi - %x' %x %step f+h )
                getF INDEX@ PULLEL SWAPDROP     ( %x' %x %step f+h [f] f )
                x- OVER x/                      ( %x' %x %step [f+h-f]/h )
                ;
                5UNROLL                               ( f+h-f/h %x' %x %step )
        LOOP                                            ( next Fi )
       RDROP                                           ( remove eq from rstack )
        DROP                                             ( drop step %x' %x )
        REPLACE 2DROP                           ( ... ) 
LOOP
getvarnum geteqnum UNCOERCE2            ( 2:%n,1:%m )
TWO{}N XEQ>ARRAY                                        ( TRN[J] )
;

NULLNAME NEWGUI
::
SetApprox
SetNum
getvarlist putfullvarlist               ( varlist may be modified )
' ::    SIXTY #=casedrop TrueTrue  ( reply "fullscreen" )
        SIXTYONE #=casedrop TrueTrue
        SIXTYTWO #=casedrop :: getfullvarlist LENCOMP TRUE ;
        BINT80 #=casedrop
        :: getfullvarlist SWAP NTHCOMDDUP   ( var var )
        SAFE@_HERE ?SKIP :: %1 DUP3PICK XEQSTOID ;
        SWAP ID>TAG
        TRUE
        ;
        BINT83 #=casedrop                   ( specify menu )
        ::
        ' ::  NoExitAction
        { { "EDIT" EDITKEY2 }
          { "RESET" RESETKEY }
          { "INFO" INFOKEY }
          { "TOL" TOLKEY }
          { "STK" TOSTK }
          { "SOLVE" OKKEY2 } }
          ;
        TRUE                            ( signal msg handled )
        ;
        BINT85 #=casedrop :: 12GETLAM MAKEHL 15PUTLAM TRUE ;
        BINT96 #=casedrop :: EDITKEY2 SetDA2aBad
SetDA2bBad
SetDA3Bad
FalseTrue ;        ( ENTER not allowed because it copies obj to stk )
        DROPFALSE                       ( further msgs not handled )
  ;                                     ( end if msg handler )

#BF0C                    ( "variables:" )
FOUR                                    ( use STD mode )
NULL{}
ONE                             ( focus pos )
ROMPTR Choose
DROP
;

NULLNAME OKKEY2
::
15GETLAM LENCOMP #0=case DoBadKey  ( no items picked, no solve )

"SOLVE WITH N‹M?" 
15GETLAM LENCOMP geteqIDX LENCOMP                ( $ #n #m )
#=ITE DROPTRUE AskQuestion NOT?SEMI

ERRSET
::
15GETLAM LENCOMP
#1+_ONE_DO (DO)
getfullvarlist 15GETLAM INDEX@ NTHCOMPDROP NTHCOMPDROP
LOOP

15GETLAM LENCOMP {}N putvarlist
getvarlist LENCOMP
#1+_ONE_DO (DO)
getvarlist INDEX@ NTHCOMPDROP SAFE@_HERE
?SKIP %1
LOOP
ClrSilent                       ( force complex messages )
THREE SetSysFlag                ( set numeric results )
SetApprox                       ( approx mode )
TWENTYTWO ClrSysFlag            ( 1/0 -> error )
getvarlist LENCOMP UNCOERCE %1 TWO{}N XEQ>ARRAY
FLASHPTR CKNUMARRY
DUP TYPECARRY? ITE FLASHPTR SETCOMPLEX FLASHPTR CLRCOMPLEX
* putX ( store in vector )
FIVE DOFIX
TURNMENUOFF
"Wait" DISPLASTROWBUT1
PACKEQ
SOLVER          ( 1: [xinit] )
;
ERRTRAP
        ::
               DEPTH LAM D #- NDROP
               ERROR@ JstGETTHEMSG                   ( else get the message )
               DUPNULL$? ITE_DROP FLASHPTR DoAlert    ( DISPLASTROWBUT1 )
        ;
SetDA2aBad
SetDA2bBad
;

NULLNAME EDITKEY2
::
* TakeOver
getfullvarlist 18GETLAM NTHCOMDDUP       ( var var )
SAFE@_HERE NOTcaseDROP                     ( "X" value )
FLASHPTR FLOAT? ?SKIP :: DROP %1 ;               ( only edit float )
FLASHPTR EQW3Edit                       ( edit value)
NOTcaseDROP                             ( aborting edit does not return val )
EVALCRUNCH
SWAP XEQSTOID
;

NULLNAME MAKEHL ( 1: #  -> 1: HLlist)
:: DUP#0=csedrp NULL{} #1+_ONE_DO (DO) INDEX@ LOOP DUP {}N ;

NULLNAME RESETKEY
::
15GETLAM LENCOMP #0=case DoBadKey
"RESET SELECTED VARS?" AskQuestion NOT?SEMI
15GETLAM LENCOMP #1+_ONE_DO (DO)
%1 getfullvarlist 15GETLAM INDEX@ NTHCOMPDROP NTHCOMPDROP XEQSTOID
LOOP
SetDA2aBad
SetDA2bBad
;

NULLNAME TOSTK
::
TakeOver
getF TYPEBINT? caseDoBadKey
THREE ClrSysFlag
getF FLASHPTR MATTRAN
* getvarlist getX FLASHPTR MATTRAN FLASHPTR MATRIX2LIST COMPEVAL x=
getvarlist DUP COMPEVAL getvarlist LENCOMP {}N x=
LAM D #2+ ' LAM D STOLAM
;
