                        STITLE STKSOLVE

**********************************************************************
* ROMID Declaration
**********************************************************************

xROMID 60E ( 1550 )
ASSEMBLE
=SSROMID   EQU #60E
RPL

***************************************************************************
* Config Object                                                           *
***************************************************************************

ASSEMBLE
=SScfg
RPL
::
[#] SSROMID TOSRRP ( autoattach lib )
;

***************************************************************************
* External Declarations                                                   *
***************************************************************************

EXTERNAL FIXCONST
EXTERNAL FINITE
EXTERNAL JACOBI
EXTERNAL CORESX
EXTERNAL KERNELSX
* EXTERNAL xSTKSOLVE
* EXTERNAL xABOUTSS

***************************************************************************
* unsupported entries                                                     *
***************************************************************************

ASSEMBLE

MATRE          EQU #35F8F
XEQ>VECTOR     EQU #1D02C
CLRNUM         EQU #53B88
SETNUM         EQU #53B9C
XEQPURGELIST   EQU #20F35
PULLEL         EQU #3558E
@STACKMRK      EQU #113C2
!STACKMRK      EQU #113D2
SYMDER         EQU #54954
xABS           EQU #1AA1F
STO+           EQU #204AA

RPL

DEFINE getiter    15GETLAM  ( holds #max. iterations used in stack mode )
DEFINE putiter    15PUTLAM
DEFINE getjacobi  1GETLAM   ( 1LAM holds jacobimatrix )
DEFINE putjacobi  1PUTLAM
DEFINE getvarnum  9GETLAM   ( 2LAM holds number of variables )
DEFINE putvarnum  9PUTLAM
DEFINE getvarlist 10GETLAM  ( 3LAM holds list of variables )
DEFINE putvarlist 10PUTLAM
DEFINE geteqlist  12GETLAM  ( list of equations )
DEFINE puteqlist  12PUTLAM
DEFINE getdepth   13GETLAM  ( holds stackmark used in errors )
DEFINE putdepth   13PUTLAM
DEFINE geteqnum   11GETLAM  ( holds number of equations )
DEFINE puteqnum   11PUTLAM
DEFINE geterror   14GETLAM  ( holds the user-tolerance in %% form )
DEFINE puterror   14PUTLAM
DEFINE getconlist 8GETLAM   ( holds list of latest 2 linesearch constanst )
DEFINE putconlist 8PUTLAM
DEFINE getabslist 7GETLAM   ( holds list of latest 3 absF values )
DEFINE putabslist 7PUTLAM
DEFINE getsn      6GETLAM   ( holds the the SN vector )
DEFINE putsn      6PUTLAM
DEFINE getlastx   5GETLAM   ( holds the last good solution X vector )
DEFINE putlastx   5PUTLAM
DEFINE getlastf   4GETLAM   ( holds last 2 good «absF^2 vals )
DEFINE putlastf   4PUTLAM
DEFINE getmaxstep 3GETLAM   ( max step size )
DEFINE putmaxstep 3PUTLAM
DEFINE getcnstr   2GETLAM   ( list of constrainers { { %low %hi #pos } ... )
DEFINE putcnstr   2PUTLAM

****************************************************************************
* User commands ( STKSOLVE and ABOUTSS)                                    *                                  *
* **************************************************************************

****************************************************************************
* ABOUTSS - version information                                            *
* Syntax: none                                                             *
****************************************************************************

ASSEMBLE
                    CON(1)  8
RPL
xNAME ABOUTSS
::
CLEARLCD
"    STKSOLVE 3.0a" BIGDISPROW1
"      Copr. 1996" BIGDISPROW2
"     Sune Bredahl" BIGDISPROW3
VERYVERYSLOW
;

ASSEMBLE
                    CON(1)  8
RPL
xNAME STKSOLVE
::
CK4&Dispatch #5111
    ::
    PATHDIR RCLSYSF NULLLAM DUP TWO{}N BIND
    ::
    SWAP COERCE ONE{}N SWAP %1 %=
    KERNELSX
    getvarlist XEQPURGELIST ABND
    ;
    2GETLAM COMPEVAL 1GETABND DOSTOSYSF
    ;
;

***************************************************************************
* Hidden library objects                                                  *
***************************************************************************

NULLNAME KERNELSX  ( { eq } %tol { sysflag } t/f )
::
* In case of stack args. additional args are 7: { initvals }  6: { vars }

* List of USERFLAGS used
*
* 41 SET if initial values have been created
* 42 SET if LINEAR selected else CLR
* 43 SET if REALS ONLY
* 44 SET if numeric jacobian should be used, CLR if symbolic
* 46 CLR if hybrid model is used, SET if ordinary
* 47 If SET, constants have been stored

* process system settings - not bound to lams

        FORTYTHREE SWAP                     ( process complex check )
        ITE ClrUserFlag SetUserFlag

        COMPEVAL
        FORTYFOUR
        FORTYTWO
        ROT

        ::
        DUPONE #= casedrop :: ClrUserFlag ClrUserFlag ;
        DUPTWO #= casedrop :: ClrUserFlag SetUserFlag ;
        :: DROP SetUserFlag SetUserFlag ;
        ;

* setup all stuff to be CACHEd

        %>%%                                ( 14 is %%tolerance )
        @STACKMRK                           ( 13 is stackmark )

* check the eq list given - none or empty?

        ROT

        COMPEVAL 5UNROLL 4UNROLL

        DUPNULL{}? IT SETTYPEERR

        ZERO 4UNROLL                        ( 15 is #iter )
        DUPDUP                              ( make copy )
        DOSTOE                              ( store in EQ )

        LENCOMP 6ROLL
         
        DUPNULL{}? IT SETTYPEERR            ( 10 holds varlist )

        DUPLENCOMP                          ( 9 holds #_vars )
        %%1 DUP TWO{}N                      ( 8 holds k1 and k0 )
        %MAXREAL %>%% DUP TWO{}N            ( 7 holds ABS )
        MINUSONE                            ( 6 holds sN  vector )

*        MINUSONE                            ( 5 holds lastxOK vector )
*        %MAXREAL %>%% DUP TWO{}N            ( 4 holds lastf values )
*        MINUSONE                            ( 3 lam holds maxstep )

        2DUP SWAPOVER

        NULL{}                              ( 2 lam holds constraint list )

        FOURTEEN ' NULLLAM CACHE


* note: 1 lam will be jacobi matrix when computed


    ERRSET
    CORESX
    ERRTRAP

::
getdepth !STACKMRK DropSysObs       ( remove junk obs from stack)
{ ZERO #501 }
ERROR@ EQUALPOSCOMP
#0=ITE
ERROR@
#A03
DUP ERRORSTO #>HXS
;

;

**************************** END OF KERNEL **********************************
NULLNAME CORESX
::
HOMEDIR

*  If LINEAR model was chosen (userflag 42 set), use %1 as init value
*  and skip initial values dialog box,
*  else prompt for initial values unless in stack mode stk1={ init val }

    FORTYTWO UserITE

    :: DROP %1 getvarnum NDUPN UNCOERCE XEQ>VECTOR ;
    :: COMPEVAL getvarnum getvarlist
     FIXCONST SWAP putcnstr DUPLENCOMP putvarnum putvarlist ;

*  Display init message and make vector of init values
*  create maxstep variable, maxstep = 100*max(abs(X),n)
*  Update stack mark - used in error handling
*  randomize used in interval handling

   %1 DORANDOMIZE

*  FIXCONST returns 3:[iterants'], 2: { constrainers } 1:{vars'} or does it?

  getvarnum geteqnum #> IT :: #E404 ERROROUT ;
  
  DUP xABS getvarnum UNCOERCE %MAX %100 %* putmaxstep

*   Initialize systemflags:
*   CLRNUM forces equations to be evaluated as functions, f=0
*   sysflags 20,21 ensures overflow and underflow will not cause errors
*   sysflag 22 ensures matrix div produces infinite result if singular
*   and is set (perturbation) if constraints are used, else cleared

    CLRNUM

*   Make symbolic jacobian only if nonexistent (1LAM is a #) or if
*   numeric derivatives are used

    FORTYFOUR TestUserFlag ?SKIP JACOBI

*   Newton iteration part starts here! -  uses BEGIN WHILE REPEAT structure

BEGIN

::

*   A guess [X] in stk1 is expected, the vars are updated and F(X) is found
*   if ABS[F(X)] > ABS[F(Xprev)] then backtrack is used
*   a [X] and a F[X] is passed to next part

    SETNUM ( numeric results allows eq being evaluated as functions )

*   BEGIN { compute Xnew using backtrack } UNTIL { f < fold }
*   only when true, the next full newton step is calculated

    BEGIN

    ::

* Ensure 1/0 -> ok!
* Reset last 1/0 errorflag

TWENTYTWO SetSysFlag
TWENTYSIX ClrSysFlag

* do constraints check - if (numbers of eq)*0.1 > abs(f) -  skip this step
* for complex numbers - skip this step
* if no constraints defined - skip this step
* when constraints are in fact used, the lastf is reinitilized!

FORTYTHREE TestUserFlag SWAPOVER IT MATRE SWAP
getlastf CARCOMP %%.1 geteqnum UNCOERCE%% %%* %%>

AND IT

::    getcnstr DUPNULL{}? caseDROP
      LENCOMP

      #1+_ONE_DO (DO)

      getcnstr INDEX@ NTHCOMPDROP COMPEVAL 4UNROLLROT
      4PICK PULLREALEL DUP 5PICK %< SWAP4PICK %> OR
      ITE
      :: UNROTOVER %- %RAN %* %+ ROT PUTREALEL %MAXREAL %>%% DUP TWO{}N putlastf ;
      4UNROLL3DROP

      LOOP
;

* store guess

    getvarnum
    #1+_ONE_DO (DO)
    INDEX@ PULLEL getvarlist INDEX@ NTHCOMPDROP STO
    LOOP

* Make F(X) vector and abs(F) value for this step and update the abs F list

    geteqlist INNERCOMP
    ZERO_DO (DO) COMPEVAL ISTOP@ ROLL LOOP
    geteqnum UNCOERCE XEQ>VECTOR

    DUP xABS %>%%

    DUP getabslist CARCOMP TWO{}N putabslist

*   Current stack:  3:[X], 2: [F(X)], 1:%%abs(F)
*   define f = 1/2*abs[F]^2
*   test for non-convergence fnew=fold in very extreme situations

    DUP %%* %%2 %%/ DUP
    getlastf CARCOMP

    2DUP 2%%>% %= getiter FORTY #> OR IT :: #A01 ERROROUT ;

    DUP %%.1 %%4 %%^ %%*  getconlist CARCOMP %%* %%-

*   if f < fold + c*10*E-4*slope (slope = 2*fold!!) - update lastf and lastx
*   or if std method is selected then ok
*   else backtrack using [ Xold ], in 'lastx', and [ p ] in 'sn' variable
*   lastf list, conlist and abslist

    %%< ITE

    :: getiter #1+ putiter getlastf CARCOMP TWO{}N putlastf OVER putlastx TRUE %%1 ;

    ( returns Xnew Fnew true K )

    ::

*   discard X F and f
*   first backtrack is quadratic backtrack (last c=1), subsequent are cubic

       3DROP getconlist CARCOMP
       %%1 EQITE

       ::

*      if (f - fold) < 0.001*F and c is < 0.001 then assume local minimum

       getlastf COMPEVAL OVER %%- %%ABS SWAP %%.1 %%3 %%^ %%* %%<
       getconlist COMPEVAL SWAPDROP %%.1 %%3 %%^ %%<
       AND IT :: #A01 ERROROUT ;

*      quad bactrack: c'=max{INV[(ABS[F(X1)]/ABS[F(X0)])^2+1],1/10}

       getabslist COMPEVAL %%/ DUP %%* %%1 %%+ %%1/ %%.1 %%MAX ;


       ::   
*           compute rhs1 = *f-fold-L1*slope
*           compute rhs2 = f2-fold2-L2*slope

            getabslist COMPEVAL SWAPDROPDUP %%* %%2 %%/
            getlastf CARCOMP getconlist COMPEVAL SWAPDROP %%2 %%* %%1 %%- %%* %%+

            ( %%- getlastf CARCOMP %%2 %%* getconlist COMPEVAL SWAPDROP %%* %%+ )
            
            getabslist CARCOMP DUP %%* %%2 %%/
            getlastf CARCOMP getconlist CARCOMP %%2 %%* %%1 %%- %%* %%+

            ( %%- getlastf CARCOMP %%2 %%* getconlist CARCOMP %%* %%+ )

*           compute a = [ rhs1/sq(L1) - rhs2/sq(L2) ] / [ L1 - L2 ]

            2DUPSWAP getconlist COMPEVAL ROTSWAP DUP %%* %%/ UNROTDUP %%* %%/ %%-
            getconlist COMPEVAL SWAP %%- %%/ 3UNROLL

*           compute b = [ (-L2*rhs1)/sq(L1) + (L1*rhs2)/sq(L2) ] / [ L1 - L2 ]

            SWAP getconlist COMPEVAL DUP %%*UNROT %%CHS %%*SWAP %%/
            SWAP getconlist COMPEVAL ROT %%* SWAPDUP %%* %%/ %%+
            getconlist COMPEVAL SWAP %%- %%/

            OVER ( fecth a from stack if 0 then quadratic )
            %%>% %7 RNDXY %0= ITE
            :: getlastf CARCOMP ROTDROPSWAP %%/ ;
            :: %%CHS 2DUP DUP %%*SWAP getlastf CARCOMP %%* %%3 %%2 %%* %%* %%+ %%SQRT %%+ SWAP %%3 %%* %%/ ;

            getconlist CARCOMP %%10 %%/ %%MAX
            getconlist CARCOMP %%2 %%/ 2%%>% %MIN %>%%
            ;

      DUP %%>%
      getsn
      getlastx SWAPROT x* x-
      FALSE ROT                          ( returns Xnew false K )
            ;

getconlist CARCOMP TWO{}N putconlist     ( update conlist with last val )
;


UNTIL

* save the ABS for next iteration and check if smaller than user-error

getabslist CARCOMP
geterror %%>

;

WHILE

* return jacobian matrix elements according to flag 44 and

FORTYFOUR UserITE FINITE getjacobi EVAL

( eval jacobi::, doesnt cause probs with numeric elems from num jac )

geteqnum getvarnum UNCOERCE2 TWO{}N XEQ>ARRAY


getvarnum geteqnum #=?SKIP

:: DUP MATTRN DUPROT x* SWAPROT x* SWAP ;

x/

TWENTYSIX TestSysFlag TWO getiter #> OR IT
:: %MAXREAL %>%% DUP TWO{}N putlastf ;

* scale: if maxstep > abs[p] then: [p_new] = (maxstep/abs[p])*[p]
* and save to variable used in the linesearch routine
* compute new solution
* Note: ITE_DROP executes :: semi ; if stk1=FALSE else DROPs,

DUP xABS DUP getmaxstep %< ITE_DROP :: getmaxstep %/ x/ ;

DUP putsn

x-

REPEAT

* if ABS[f(x)] < usererror program continiues here
* drop some args and recall and tag soltutions
* pass TRUE to error trap below

2DROP
getvarnum
#1+_ONE_DO (DO)
getvarlist INDEX@ NTHCOMDDUP EVAL SWAP ID>TAG
LOOP
#A04 DUP ERRORSTO #>HXS
;

****************************************************************************
* Name:         JACOBI                                                     *
* Purpose:      computes analytic jacobian (once and for all)              *
* Syntax:       none,  stores result as :: RPN obj ; in 1LAM               *
* Last revised: April 28                                                   *
****************************************************************************

NULLNAME JACOBI
::
getvarlist XEQPURGELIST                     ( purge var )
geteqnum
#1+_ONE_DO                                  ( for all eqs do... )
    geteqlist                               ( get eqs )
    INDEX@                                  ( get counter )
    NTHCOMPDROP                             ( fetch nth eq from list )
    getvarnum                               ( get number of vars )
    #1+_ONE_DO                              ( start new FOR 1 to size loop )
        DUP                                 ( copy )
        getvarlist                          ( get var list )
        INDEX@                              ( get nth eq )
        NTHCOMPDROP
        2DUPSWAP                                ( extra args )
        matchob? ITE                        ( is variable in EQ? )
        :: SYMDER symcomp INNERCOMP ::N ;   ( find der )
        :: 3DROP %0 ;                       ( else drop args )
    SWAPLOOP                                ( next var )
DROPLOOP                                    ( next eq )
getvarnum geteqnum #* ::N putjacobi         ( store jacobian )
;

***************************************************************************
* Name:         FINITE                                                    *
* Purpose:      computes forward-diff jacobian approx used in each iter   *
* Syntax:       1: [ F(xi) ] -> n*m elements (not the matrix itself)      *
* Last revised: April 28                                                  *
***************************************************************************

NULLNAME FINITE  ( [F] -> n*m elements - matrix itself is not created )
::
*  forward difference jacobian approximations
*  formula f'(x) = [f(x+h)-f(x)]/h
*  for example
*  dF1/dX1 = [ F1(x1+h,x2,x3...)-F1(x1,x2,x3....) ]/h
*  for each variable Xi in each iteration, h=eps*abs(Xi), or eps if Xi is 0.

DUP
geteqnum #1+_ONE_DO (DO)

*   get Fn(X) from F vector in stk1 - PULLEL returns 2:[F] 1:Fn
*   fecth n'th equation

    INDEX@ PULLEL
    geteqlist INDEX@ NTHCOMPDROP
  
    getvarnum
    #1+_ONE_DO (DO)

*       fecth m'th variable and make extra copies used in variable update and
*       restoration

        getvarlist INDEX@ NTHCOMPDROP    ( get var )

        2DUPSWAP matchob? ITE            ( if var present in eq )
        ::                               ( find forward diff )
        DUP EVAL 2DUP

*       find h = eps*ABS[value],eps, if h=0 then h=eps eps=sqrt(accuracy) = 1E-
*       update variable as x+h
*       sto+ syntax: 3:id 2:oldval 1:addition

        DUP xABS % 1E-6 %* %10 RNDXY DUP%0= :: casedrop % 1E-6 ;
        DUP 6UNROLL STO+

*       stack is now 4:eq, 3:h, 2:id 1:xvalue
*       evals F(Xi+h)
*       saves original value in x

        4PICK COMPEVAL
        SWAPROT STO
        4PICK x- SWAP x/
        ;

        :: 2DROP %0 ;                       ( else drop args)

        4UNROLL

    LOOP

2DROP

LOOP

DROP
;

*****************************************************************************
* Name:         FIXCONST                                                    *
* Purpose:      Stores constants, updates varlist, makes intervals holders  *
* Syntax:       n initvals, 2:count, 1 { vars } -> [x1], { constr },{ var'} *  
* v3.0 rev.     April 28                                                    *
*****************************************************************************

NULLNAME FIXCONST
::
NULL{} DUP 4NULLLAM{} BIND

( 4LAM is #values, 3LAM is varlist, 2GTELAM is vallist, 1lam is newvar )

4GETLAM reversym
NULL{} 4PUTLAM
#1+_ONE_DO (DO)
DUPTYPEARRY?
ITE

:: ONE PULLEL SWAPDROP 3GETLAM INDEX@ NTHCOMPDROP XEQSTOID ;

::
   1GETLAM 3GETLAM INDEX@ NTHCOMPDROP >TCOMP 1PUTLAM

   DUPTYPECMP? FORTYTHREE TestUserFlag AND

   IT  :: C%>% %MAXorder SWAP2DUP 1GETLAM LENCOMP THREE{}N
          4GETLAM SWAP >TCOMP 4PUTLAM
          SWAPOVER %- %RAN %* %+
       ;
   
   ( for a midpoint estimate, replace the last line with: %+ %2 %/ )

   2GETLAM SWAP >TCOMP 2PUTLAM ;

LOOP
2GETLAM INNERCOMP UNCOERCE XEQ>VECTOR
4GETLAM
1GETABND
;

