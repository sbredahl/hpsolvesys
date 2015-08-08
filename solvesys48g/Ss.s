*    SOLVESYS for the HP48G Series, Copyright 1994-1999 Sune Bredahl 
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*    The original, unmodified, source code for SOLVESYS 4.03 goes here:

STITLE SOLVESYS40

* ROMID Declaration

xROMID 60E ( 1550 )
ASSEMBLE
=SSROMID   EQU #60E
RPL

* Config Object                                                           

ASSEMBLE
=SScfg
RPL
::
[#] SSROMID TOSRRP ( autoattach lib )
;

* External/NULLNAME Declarations

EXTERNAL FIELDCHK
EXTERNAL FVEC
EXTERNAL StoreX
EXTERNAL GETVARS
EXTERNAL MAKEINIT
EXTERNAL SOLVENV
EXTERNAL JACOBI
EXTERNAL SOLVER
EXTERNAL STARTMSG
EXTERNAL LINESEARCH                     ( linesearch routione )
EXTERNAL PACKEQ
EXTERNAL GUISHELL
* EXTERNAL SETUP
* EXTERNAL SETUPMSG
* EXTERNAL TOLMSG
EXTERNAL TESTTOL
EXTERNAL TOSTK
EXTERNAL INFO
EXTERNAL ADDKEY
EXTERNAL EDITKEY
EXTERNAL COPYKEY
EXTERNAL DELKEY
EXTERNAL CLEARKEY
EXTERNAL OKKEY
EXTERNAL TOL
EXTERNAL TOLSTRING

* unsupported entries

ASSEMBLE

MAT*            EQU #3644E
MAT%*           EQU #362DC
&any_$          EQU #1ACBB
MATABS          EQU #369E9
MATCONJ         EQU #35F30            
XEQPURGELIST    EQU #20F35
MAT-            EQU #36278
PULLEL          EQU #3558E
@STACKMRK       EQU #113C2
!STACKMRK       EQU #113D2
%SQ             EQU #1B47B
TYPECOL         EQU #03FB3
EQ>             EQU #1CEE3
NoEqErr         EQU #10F74
xEVAL		EQU #1A3BE
REMOVE{}	EQU #0E4DE	
ARRY>		EQU #1D0AB
		
* Variable/LAM aliases

RPL

DEFINE getF       19GETLAM        ( linesearch routine )
DEFINE putF       19PUTLAM		( EQ vector )
DEFINE getf       18GETLAM		( norm of F )
DEFINE putf       18PUTLAM
DEFINE getfold2   17GETLAM
DEFINE putfold2   17PUTLAM
DEFINE puttmp     16PUTLAM        ( temporary variable )
DEFINE gettmp     16GETLAM
DEFINE getalam2   15GETLAM         ( list of latest 2 linesearch constanst )
DEFINE putalam2   15PUTLAM
DEFINE getalam    14GETLAM         ( list of _latest_ 3 absF values )
DEFINE putalam    14PUTLAM
DEFINE getP       13GETLAM         ( latest P vector )
DEFINE putP       13PUTLAM
DEFINE getXold    12GETLAM         ( last _good_ solution X vector )
DEFINE putXold    12PUTLAM
DEFINE getf2      11GETLAM         
DEFINE putf2      11PUTLAM
DEFINE getmaxstep 10GETLAM       ( %max step size )
DEFINE putmaxstep 10PUTLAM
DEFINE getX       9GETLAM        ( initial values list/array )
DEFINE putX       9PUTLAM
DEFINE putcos     8PUTLAM      ( CORE execution time in ticks )                
DEFINE getcos     8GETLAM
DEFINE putfold    7PUTLAM        ( NOTE!!! )
DEFINE getfold    7GETLAM
DEFINE getconnum  7GETLAM      ( number of constants )
DEFINE putconnum  7PUTLAM
DEFINE getvarlist 6GETLAM      ( list of variables )
DEFINE putvarlist 6PUTLAM
DEFINE getvarnum  5GETLAM      ( #number of variables )
DEFINE putvarnum  5PUTLAM
DEFINE geteqnum   4GETLAM      ( # number of equations )
DEFINE puteqnum   4PUTLAM
DEFINE getdx	  3GETLAM       ( not used... )
DEFINE putdx	  3PUTLAM
DEFINE puteqlist  2PUTLAM       ( list of equations )
DEFINE geteqlist  2GETLAM
DEFINE geteqeval  2GETEVAL
DEFINE getgrad    1GETLAM
DEFINE putgrad    1PUTLAM

DEFINE IterFlag  FORTYTHREE
DEFINE ComplexF  FORTYTWO       

***************************************************************************
* Flags used (for reference purposes )                                    *
* User flags (previous settings are not restored):                        *
* 42 Complexflag                                                                       
* 43 iterflag, Clear if ITER=0 else SET for iter>0      
* System flags (previous settings are restored):                          *
* 22 SET to allow 1/0 in matrix computations                              *
* 26 clears 1/0 -> error flag                                             *
***************************************************************************

*************************************************************************************************************
xNAME SOLVESYS
::
CK0
RCLSYSF RCLUSERF TWO{}N 1LAMBIND        ( save flags )
  ' ::
       SIXTY #=casedrop TrueTrue
       SEVENTY #=casedrop

      :: 12GETLAM #0=ITE #609 #608 JstGETTHEMSG
	  DUPLEN$ #1-1SWAP SUB$ CHR_s >T$ TRUE ;

       83 #=casedrop
	::
	  ' ::  NoExitAction
		{ { "ADD" ADDKEY }
*	          { :: TakeOver 12GETLAM #0=ITE NULL$ "EDIT" ; EDITKEY }
*		  { :: TakeOver 12GETLAM #0=ITE NULL$ "DEL" ; DELKEY }
*		  { :: TakeOver 12GETLAM #0=ITE NULL$ "COPY" ; COPYKEY }
*		  { :: TakeOver 12GETLAM #0=ITE NULL$ "CLEAR" ; CLEARKEY }
*                 { :: TakeOver 12GETLAM #0=ITE NULL$ "OK" ; OKKEY } }
	          { "EDIT" EDITKEY }
		  { "DEL" DELKEY }
		  { "COPY" COPYKEY }
		  { "CLEAR" CLEARKEY }
                  { "OK" OKKEY } }
	    ;
	    TRUE
	;
	DROPFALSE
    ;
    SPACE$                      	( title - handled by msg 70 )
    FOUR                            	( decompile format )
    ::                              	( initial value found below... )
    EQUATION                        	( fecth EQ if existing )
    NOTcase NULL{}                  	( if not, exit and return NULL{} )
    DUPTYPELIST?                    	( if EQ is ok, check if list )
    NOTcasedrop NULL{}              	( if not - exit. drop EQ and return NULL{} )
    ;
    ONE                             	( focus pos )
    ROMPTR Choose
    DROP        
1GETABND DOSTOALLF                  	( restore flags )
;

***********************************************************************************************************

NULLNAME ADDKEY
::
TakeOver
"EQ" 12GETLAM #1+ #:>$ &$               ( prompt )
NULLID DO>STR
#TWO#ONE                          ( cursor pos )
						     ( insert mode )
TWO                                 ( algebraic entry )
ZERO                                ( alpha key )
{ <DelKey >DelKey TogInsertKey }
ONE
FALSE
ONE
InputLine NOT?SEMI                       ( exit if cancelled )
SWAPDROP                       ( remove unparsed in stk2 )
DUPTYPESYMB?                   ( check if input is an alg )
NOTcaseDROP                    ( if not, drop obj and exit )
19GETLAM SWAP >TCOMP 19PUTLAM  ( else update CHOICES )
12GETLAM #1+ 12PUTLAM          ( and ITEMS )
ClrDAsOK                       ( redraw disp )
;

***********************************************************************************************************

NULLNAME EDITKEY
::
TakeOver
12GETLAM #0=case DoBadKey
"EQ" 18GETLAM #:>$ &$                   ( prompt )
19GETLAM 18GETLAM NTHCOMPDROP           ( get eqn )
EDITDECOMP$                             ( convert )
#TWO#ONE                                ( cursor pos )
				        ( insert mode )
TWO                                 ( algebraic entry )
ZERO                                ( alpha key )
{ <DelKey >DelKey TogInsertKey }
ONE                           ( disp first menurow )
TRUE
ONE
InputLine
NOT?SEMI
SWAPDROP       ( remove unparsed obj in level 2 )
DUPTYPESYMB?
NOTcaseDROP                   ( if not alg, drop notalg and exit )
18GETLAM 19GETLAM   	      ( 'editeq' #higlight {eqlist} )
PUTLIST			      ( replace equation in list )
19PUTLAM		      ( save new list )     	
ClrDAsOK		      ( redraw display )  	
;

**********************************************************************************************************		

NULLNAME DELKEY
:: 
TakeOver
12GETLAM #0=case DoBadKey
19GETLAM 18GETLAM 			( {eqlist} #highligt )
REMOVE{} 				( remove subject )
19PUTLAM 	( update list )
12GETLAM #1- 12PUTLAM           	( update #items' )
ONEONE 6PUTLAM 18PUTLAM         	( update focus )
ClrDAsOK
;

***********************************************************************************************************

NULLNAME COPYKEY

:: 
TakeOver
12GETLAM #0=case DoBadKey
19GETLAM DUP                                    ( eqlist eqlist )
18GETLAM NTHCOMPDROP                                    ( eqlist eqn )
>TCOMP 19PUTLAM                                         ( add eq to list and save )
12GETLAM #1+ 12PUTLAM                           ( store items_list' )
*              ONEONE 6PUTLAM 18PUTLAM 
ClrDAsOK
;

***********************************************************************************************************

NULLNAME CLEARKEY 

:: 
TakeOver 
12GETLAM #0=case DoBadKey	
NULL{} 19PUTLAM                                  ( fetch item_list )
ZERO 12PUTLAM
ONEONE 6PUTLAM 18PUTLAM				( forgot this in version 4.02... )
ClrDAsOK
;

************************************************************************************************************

NULLNAME OKKEY 
:: 12GETLAM #0=case DoBadKey 19GETLAM DOSTOE GUISHELL ClrDAsOK ;

*************************************************************************************************************

xNAME FITEQ
::
CK3&Dispatch #961
	::
	COERCEDUP
	4NULLLAM{} BIND
	3GETLAM ?PURGE_HERE
*	4GETLAM ONE{}N GETVARS XEQPURGELIST
	1GETLAM  
	#1+_ONE_DO (DO)
	INDEX@ UNCOERCE 3GETLAM XEQSTOID
	4GETLAM uncrunch COMPEVAL
	LOOP
	3GETLAM ?PURGE_HERE
	1GETABND {}N DOSTOE
	;
;

***********************************************************************************************************

xNAME ABOUTSS
::
" SOLVESYS 4.03\0ACopr. 1994-1999  Sune Bredahl" ROMPTR 0B1 E
;

***********************************************************************************************************

NULLNAME GUISHELL
::
@STACKMRK % 1E-5 % 1E-3 DUP %10* { LAM d LAM o LAM p LAM q } BIND                ( save stackmark )
MINUSONE EIGHTEEN NDUPN ' NULLLAM CACHE  ( create 19 nullvars )  	

EQUATION ?SKIP NoEqErr DUP DUPLENCOMP puteqnum puteqlist 		( update basic vars and simple checks )
GETVARS DUPNULL{}? caseSIZEERR 								
DUPLENCOMP putvarnum putvarlist								
 
ClrDA1IsStat                                                            ( suspend clock disp. updates )                 
IterFlag ClrUserFlag                    				( iter=0 )     
                                                  
     
BEGIN
ERRSET 
	::
		SOLVENV 						( display init value box )                                                        
		TWO DOFIX
		MAKEINIT						( save constants )
		
		getvarnum FIVE #>?SKIP :: Clr8-15 BlankDA2 ;		( various display stuff... )
		TURNMENUOFF 						
		ZERO FIFTYSIX "PRESS [ON] TO ABORT" ROMPTR 0B0 0A5 XYGROBDISP 
		"Wait" DISPROW7
		
		geteqlist TYPECOL? getconnum #0= AND ?SKIP		( invoke PACKEQ?? )      	
		PACKEQ                                             	( make final EQ executable )
		SOLVER							( start the engine )
	;
	ERRTRAP 
	::
		LAM d !STACKMRK DropSysObs				( drop excessive stack objs )
		ERROR@ #501 #=ITE ZERO ERROR@ 				( 501 error if ON/CANCEL pressed )
		JstGETTHEMSG            ( else get the message )
		DUPNULL$?                                                       ( check for empty string )
		ITE_DROP ROMPTR 0B0 091                                 ( display the message )
		{ ZERO #A01 #A04 #304 #305 #12F #BB0D }     		( non-termination errors #e404 removed )
		ERROR@ EQUALPOSCOMP #0=         			( check if errors allowable )
	;
UNTIL									( exit if serious error )
	getvarlist                      ( fecth var{} )
	IterFlag UserITE                ( variables initialized? )
	XEQPURGELIST                    ( yes, then purge vars )
	DROP                            ( no, just drop varlist )
	ABND                            ( purge cached variables )
ABND                                    ( purge DEPTH variable )	( goodbye...)
* SysDisplay                              ( restore screen )
;

*********************************************************************************************

NULLNAME SOLVENV ( -> )
::
getvarnum TWELVE #/ SWAP #0=?SKIP #1+   ( page number: ceil[varnum/12] )

#1+_ONE_DO (DO)
	getdx					(  )
	geteqnum				
	getvarnum                               (  )
	ISTOP-INDEX #1=                         ( T/F -> true if single screen only )
	getf 		                        ( current absF value )
	getcos                                  ( current LSQ value )
	getvarlist                              ( master varlist )
	INDEX@ #1- TWELVE #* #1+                ( startvalue: 12*[n-1]+1 )
	getvarnum INDEX@ TWELVE #* #MIN         ( endvalue: min[varnum,n*12] )
	SUBCOMP                                 ( varlist' )
	DUPLENCOMP                              ( varnum' )
	{ LAM x LAM E LAM m LAM s LAM f LAM c LAM v LAM n } BIND
	LAM v INNERCOMP                         ( make field var list )
	ZERO_DO  (DO) 
		ISTOP@ ROLL 
		":" &any_$ 
	LOOP 
	LAM n {}N

*   4 fields/line and fieldlenght=1, if 12 vars or less then 3 f/l and fl=2

	ONE LAM n
	::
	DUP FIVE #< casedrop ONE                  ( find best var/row fit )
	DUP NINE #< casedrop TWO
	DUP THIRTEEN #< casedrop THREE
	DUP SEVENTEEN #< casedrop FOUR
	DROP FIVE
	;
	ONE
	ROMPTR 0B1 01E
	INNERCOMP
	ZERO_DO (DO)
		ISTOP@ ROLL 
		COMPEVAL
		' FIELDCHK 
		5UNROLL
		ONE
		#ZERO#ONE #THREE#FOUR FOUR {}N    	( allowable user types )
		FOUR					( decompile object is FIX mode )
		"ENTER GUESS OR [CONST]"  
		MINUSONE DUP
		ELEVEN {}N
	LOOP
	LAM n {}N
	' STARTMSG
	"CURRENT VALUES"                        
	  :: %1 LAM n NDUPN {}N ;	    ( MINUSONE )        
	IterFlag UserITE                    ( iter=0 then FALSE )               
	:: LAM v COMPEVAL LAM n {}N ;       ( eg { X Y Z } -> { 4 2 1 } )
	:: LAM v INNERCOMP                      ( first time... )       
	#1+_ONE_DO (DO)
	:: SAFE@_HERE       ( recall var )
	NOTcase %1          ( if nonexist, exit and return 1 )
	DUPTYPEREAL?        ( else test if var is real or complex valued )
	SWAP DUPTYPECMP?
	ROT OR
	NOTcasedrop %1      ( if not, drop var contents and return %1 )
	{ %1 } XEQ>ARRAY    ( else a real value )
     ;
    ISTOP-INDEX UNROLL
    LOOP
    LAM n reversym {}N
;

ROMPTR 0B1 22
DoInputForm
ABND
?SKIP :: 2LIST ERROROUT ;
LOOP
getvarnum {}N putX                      ( store initial values vector )
;

************************************************************************************************

NULLNAME STARTMSG ( -> menu true )
:: 
FIFTEEN #<>case FALSE
' NoExitAction 
ROMPTR IFMenuRow1 CARCOMP 	    ( key1 - extract edit menu )
NullMenuKey			    ( key2 - not used )	
{ "TOL" :: TakeOver TOL ; }	    ( key3 - prefences )	
{ "INFO" :: TakeOver INFO ; }	    ( key4 - info ) 	
{ "STK" :: TakeOver TOSTK ; }      ( key5 - to stack )
FIVE {}N                            ( make list of keydefs )     
{ :: TakeOver ROMPTR DoKeyOK ; }    ( sixth key depends on number of vars )  
LAM s ITE "SOLVE" "MORE" >HCOMP ONE{}N &COMP
2Ob>Seco TRUE
;

***********************************************************************************************

NULLNAME FIELDCHK ( obj -> flag true )
:: 	
FORTYSIX #<>case FALSE 			( handle #46 only! )
DUPTYPEARRY? ITE
:: ARSIZE #1= ;
DROPTRUE 				( signal message handled )						
TRUE
;

**********************************************************************************************

NULLNAME INFO
::	
"m/n: " 
LAM E #>$ &$ CHR_/ >T$  
LAM n #>$ &$
NEWLINE$&$ 
"›x : " LAM x MINUSONE #=?SKIP :: LAM x a%>$ &$ ; NEWLINE$&$ &$
"eq : " LAM f MINUSONE #=?SKIP :: LAM f %2 %* %SQRT a%>$ &$ ; NEWLINE$&$ &$
"lsq: " LAM c MINUSONE #=?SKIP :: LAM c a%>$ &$ ; NEWLINE$&$ &$
ROMPTR 0B1 E 
;  

**********************************************************************************************

NULLNAME TOSTK
::
FORTYTHREE TestUserFlag NOT caseDoBadKey                 
LAM n 
#1+_ONE_DO (DO) 
LAM v INDEX@ NTHCOMDDUP 
EVAL SWAP ID>TAG 
LOOP  
AtUserStack @STACKMRK ' LAM d STO 
;             

**********************************************************************************************

NULLNAME SOLVER ( -> )
::
MINUSONE putcos MINUSONE putf		( initialize values )
THREE SetSysFlag                        ( set numeric results )
* TWENTYTWO ClrSysFlag                    ( 1/0 -> error )
TWENTYTWO SetSysFlag			( 1/0 -> peturb )
getX 
FORTYTWO OVER                      ( recal seed vector )
TYPECARRY? ITE SetUserFlag ClrUserFlag  ( check for complex mode, 1:[X0] )                                  
StoreX                                  ( save start values, 1:[X0] )
MATABS getvarnum #>% %MAX %100 %* putmaxstep  ( maxstp=10E2*max[X0] )
FVEC                                    ( compute [F] and %f )
* FORTYTWO getF TYPECARRY? IT SetUserFlag ( if F0 is complex, set complex mode )
IterFlag SetUserFlag 			( first iteraiton )
 
BEGIN   
*	DEPTH #>$ DISPROW6
	getF JACOBI                     ( F TRN[J] ) 
	geteqnum getvarnum #>ITE        
	:: DUP3PICK MAT* putgrad ;      ( F TRN[J], gradient=TRN[J]*F )
	MATTRN   ( F J )
	ComplexF TestUserFlag IT MATCONJ
	getvarnum geteqnum #=?SKIP      	( if #eq>#var... )
	:: DUPROT MAT* SWAPDUP MATTRN MAT* ; ( A=J^T*J, b=J^T*F )       
*	SWAPOVER                               ( b A b A )
*	FLUSHKEYS
*	ERRSET  
*	:: ROMPTR 0C2 053 SWAPDROP ;    ( try fast linear solve first )
	ROMPTR 0C2 053
*	SWAPDROP
*	ERRTRAP 
*	:: 
*	ERROR@ #1=case ERRJMP			( fix for Insuff mem during / ) 
*	CHR_+ CHR>$ DISPROW7 SWAPDROPSWAP ROMPTR 0C2 022 ( failed? > use pseudoinverse )
*	;	
	putP                                            ( store Newton direction [-P] )
	getf putfold                            ( update fold for linesearch )
	getX putXold                            ( update Xold for linesearch )
	LINESEARCH                                      ( updates X and f and F )
	getvarnum FIVE #>?SKIP
	::      
		getvarlist >R                           ( put variable list in returnstack )
		getvarnum                                       ( [X] #n )
		#1+_ONE_DO (DO)                         ( for all variable do )
		RSWAP 'R RSWAP
		DUP EVAL PromptIdUtil INDEX@ #1+ DISPN
		LOOP RDROP                                      ( next var )
	;
	' ID eq getf %2 %* %SQRT PromptIdUtil DISPROW7             
	TESTTOL	               					( check result )
AGAIN
;

*********************************************************************************************

NULLNAME LINESEARCH ( -> )
::
getP DUP                                        ( [-P] [-P] )
MATABS DUP getmaxstep                   ( [-P] %newlen %newtlen %maxstep ) 
%> ITE                                  ( Newtlen > maxstep? ) 
:: getmaxstep SWAP %/ MAT%* putP  ;      ( if so [-P]=[maxstep/newlwn]*[-P] )
2DROP                                           ( else drop: [-P] %newtlen )
%1 putalam                                      ( k=1 for the first step )                                      

BEGIN
*	getalam ID ALAM %MIN ' ID ALAM STO
	getXold                                 ( [Xold] )
	getP getalam MAT%*                      ( [Xold] k*[-P] )
	MAT-                                    ( [Xold]-k*[-P] )
	StoreX                                  ( store globals, leaves [X] on stack )        
	putX                                    ( store local [X] )   
	getX getXold MAT- MATABS getX MATABS %1 %MAX %/	putdx 		( save relative difference )
	FVEC                                    ( compute F and f at new X )
	getf   					( %f  )
	%0=case :: #A04 ERROROUT ;  		( in fact DUP%0=case - exit test for f=0 !!! ) 
	getfold	2DUP				(  f fold f fold )
	EQUALcase :: TESTTOL #A01 ERROROUT ;
                           			( f fold )
*	geteqnum getvarnum #>?SKIP
*	::
*	2DUPSWAP %1+ SWAP %1+ %/ % .999 %>               ( [fnew+1]/[fold+1] > 0.999 ? )        
	getalam % 1E-10 %< IT :: TESTTOL #A01 ERROROUT ; 

*	DUP                                     ( f fold fold )
*	% 1E-4 %* getalam %* %-         ( test for f<fold-1E-4*k1*2*fold )
	%>                                      ( false=exit if fnew<fold )
WHILE                                           
	getalam 
	%1 EQITE                                ( quad or cubic backtrack? )
       ::
		CHR_* CHR>$ DISPROW7		( k=-slope/[2*{f-fold-slope}] )
		getf getfold %/ %1+ %1/         ( slope=-2fold -> k=1/[fnew/fold+1] )
		                       			( store in TMP lambda )
	;
	::
	"**" DISPROW7				( display cubic backtrack )
	getf getalam %2 %* %1-          	( rhs1=f-fold-k1*slope..., slope=-2*fold )
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
		SWAP                         	( b a )
		%0=case				( b a )
		:: getfold SWAPDROPSWAP %/ ;    ( k=-slope/2b = fold/b )
		SWAP %CHS 2DUP %SQ SWAP      ( a -b b^2 a )
		%6 %* getfold %* %+     ( b^2-3*a*slope = b^2+6*a*fold ) 
		%SQRT %+SWAP %3 %* %/  ( -b+sqrtd/3a )
	;
*	DUP ID TMP SWAP x+ ' ID TMP STO
	getalam %2 %/ %MIN	 
	puttmp                  ( if tmplam>alam/2 then tmplam=alam/2 )
	getalam putalam2
	getf putf2
	getfold putfold2
	gettmp getalam %10 %/ %MAX putalam
REPEAT                  
;               

*****************************************************************************************************

NULLNAME JACOBI ( -> TRN[J] )
::

*  Computes J^T using forward diff. jacobian approx. f'(x) = [f(x+h)-f(x)]/h
*  dF1/dX1 = [ F1(x1+h,x2,x3...)-F1(x1,x2,x3....) ]/h
*  h=eps*sign(x)*max[abs(Xi),1]

getvarlist >R                                   ( ... %x %y %z )
getvarnum                                               (  #n ) 
#1+_ONE_DO (DO)                                 ( for all vars do )
	RSWAP 'R RSWAP                          ( swap count, get 'xi' from rstack )                                                    
	DUP EVAL DUPDUP DUP CKREF 4UNROLL       ( Xi %x' %x %x %x )
	DUPTYPEREAL? ITE 
	:: %0=case %1+ DUP %SGN SWAP %ABS %1 %MAX %* ; 
*	:: DUP %SGN SWAP %ABS %1 %MAX %* ; 
	:: C%ABS %1 %MAX ; 
	% 1E-6 %*                                ( h=eps*sign[x]*[max{ABS[x],1}] )
	DUP4UNROLL                             ( Xi %x' %step %x %x %step )
	x+                                      ( Xi %x' %step %x %x+step )
	SWAP REPLACE SWAP                               ( Xi %x' %x %step )
	geteqlist >R                            ( put eq in rstk )
	geteqnum 
	#1+_ONE_DO (DO) 
		RSWAP 'R RSWAP                  ( Xi %x' %x %step eq )
		5PICK OVER                              ( Xi %x' %x %step eq Xi eqi )
		::
		matchob?                                ( drops x and eq if true, else drops qi )
		NOTcase2drop %0                 ( not in eq, drop eqi and x1 ) 
		EVAL                                    ( %x' %x %step f+h )
		getF INDEX@ PULLEL SWAPDROP     ( %x' %x %step f+h f )
		x- OVER x/                      ( %x' %x %step f+h-f/h )
		;
		5UNROLL                         ( f+h-f/h %x' %x %step )
	LOOP                                            ( next Fi )
	RDROP                                           ( remove eq from rstack )
	DROP                                            ( drop step %x' %x )
	REPLACE 2DROP                           ( ... ) 
LOOP
RDROP                                                   ( remove varlist from rstack )
getvarnum geteqnum UNCOERCE2            ( 2:%n,1:%m )
TWO{}N XEQ>ARRAY                                        ( TRN[J] )
;

*******************************************************************************************

NULLNAME MAKEINIT ( -> )
* saves constants in respective variables etc
::
getvarnum				( for all vars do )
#1+_ONE_DO (DO)                        
	getX INDEX@ NTHCOMPDROP		( fecth n'th init )
        :: 
		DUPTYPEARRY? NOTcaseDROP        ( exit and drop if not arry )
		ONE PULLEL SWAPDROP   	( store value  )          
		getvarlist INDEX@  		
		NTHCOMPDROP XEQSTOID	( store value in HOME )                                         
		getvarlist INDEX@ REMOVE{} putvarlist 	( update varlist )
		getX INDEX@ REMOVE{} putX	( update initlist )
		INDEX@ #1- INDEXSTO		( adjust index )
		ISTOP@ #1- ISTOPSTO		( adjust stop value )
	;                                       
LOOP                                		( next init )
getX LENCOMP 					( #n )
DUP#0=csedrp :: #E405 ERROROUT ;                ( error if len<0 )
DUP geteqnum #>case :: #E404 ERROROUT ;          ( error if var>eq, #>case doesn't exist )
DUP getvarnum #- putconnum putvarnum		( else update varnum )
getX INNERCOMP UNCOERCE %1 TWO{}N XEQ>ARRAY putX ( store in vector )
;

*******************************************************************************************

NULLNAME GETVARS ( eq_list -> var_list ) 
::
NULL{} 1LAMBIND                                   ( save list to hold vars )
INNERCOMP                                         ( decomp eq list )
#1+_ONE_DO (DO)                                   ( for each eq )
    DUPTYPESYMB? NcaseTYPEERR                 	  ( check if symbolic )
    INNERCOMP                                     ( if so, then decomp )
    #1+_ONE_DO (DO)                               ( and update varlist ... )
	DUPTYPEIDNT?
	:: NOTcaseDROP 1GETSWAP apndvarlst 1PUTLAM ;
    LOOP                                          ( next atomic )
LOOP                                              ( next eq )
1GETABND                                          ( get final list )
ROMPTR 0E8 016                                    ( sort the list )
;

*******************************************************************************************

NULLNAME FVEC ( -> )
* makes the F vector of f errors as well overall error f=½absF^2
::
	geteqeval                              ( fn ... f1 )
	geteqnum UNCOERCE %1 TWO{}N             
	XEQ>ARRAY DUP putF                      ( update [F] )
	MATABS %SQ %2 %/ putf                   ( update %f )
;

*******************************************************************************************

NULLNAME StoreX ( 1:[X] -> 1:[X] )
::
	:: 
	FORTYTWO TestUserFlag ?SEMI     ( exit if complex OK )
	DUP TYPERARRY? ?SEMI            ( exit if real array )
	#12F ERROROUT                   ( otherwise error! )
	;                                       
getvarlist >R                           ( put variable list in returnstack )
getvarnum                               ( [X] #n )
#1+_ONE_DO (DO)                         ( for all variable do )
	INDEX@ PULLEL                   ( 2:[x0],1:xi )  
	RSWAP 'R STO RSWAP              ( store i'th var )
LOOP                                    ( next var )
RDROP                                   ( drop varlist in rstk )
;

******************************************************************************************

NULLNAME PACKEQ ( -> )

* Purpose (in random order)
* 1. Transforms 'EQ' list into self-exploding seco object used instead of EQ itself 
* 2. resolves non-user constants. eg. PI
* 3. Minor checks for empty or obsolete equations 

::
THREE ClrSysFlag                                ( 'alg' -> 'alg' )
TWO SetSysFlag                                  ( 'pi*alg' -> '3.14*alg' )
SIXTYONE SetUserFlag                            ( make sure 'CONST' -> %val )

geteqnum
#1+_ONE_DO (DO)                                    ( for each EQ do ... ) 
INDEX@ GetEqN ?SKIP NoEqErr			( get n'th eq from ID EQ )
getvarlist XEQSHOWLS
EQ> x-                                          ( rewrite 'rhs=lhs' as 'rhs-lhs' )
DUPTYPESYMB? NOTcasedrop :: #A02 ERROROUT ;     ( if not still symbolic, then error! )
TYPECOL CHANGETYPE                              ( make 'alg' > SECO conversion )
LOOP            
geteqnum ::N puteqlist                          ( store eq as SECO object ) 
;

*****************************************************************************************
NULLNAME TOL
::
'DROPFALSE
#BF26
SEVENTEEN
{
{ "XTOL" :: 'DROPFALSE NULL$ FOUR TOLSTRING THREE ROMPTR Choose NOT?SEMI ' LAM p STO ; }
{ "EQTOL" :: 'DROPFALSE NULL$ FOUR TOLSTRING FIVE ROMPTR Choose NOT?SEMI ' LAM o STO ; }
{ "LSQTOL" :: 'DROPFALSE NULL$ FOUR TOLSTRING TWO ROMPTR Choose NOT?SEMI ' LAM q STO ; }
}
ONE
ROMPTR Choose
NOT?SEMI
TWO NTHCOMPDROP
EVAL
;

*****************************************************************************************

NULLNAME TOLSTRING
* Generates list of error tolerances eg. { 0.1 .... 1E-7 }
:: %1 SEVEN #1+_ONE_DO (DO) %10 %/ DUP LOOP DROP SEVEN {}N ;  

*****************************************************************************************

* { "1E-1" "1E-2" "1E-3" "1E-5" "1E-7" "1E-9" }

* NULLNAME SETUP ( -> )
* ::
* Define text labels to be placed in the input form, "MSG" xpos ypos

* "›xTOL: " ONE TEN
* "EQTOL: " ONE TWENTY
* "LSQTOL: " ONE THIRTY

* define ZERO field

* ' TOLMSG
* THIRTY EIGHT      		( x,y = 30,9 )
* ONEHUNDRED NINE                ( l,h = 28,9 )
* ONE
* ZERO
* ONE{}N
* FOUR
* NULL$
* MINUSONE DUP			( choose info )
* % 1E-3 
* LAM p 

* define lsqtol field

* ' TOLMSG
* THIRTY EIGHTEEN              ( x,y = 16,27 )
* ONEHUNDRED NINE              ( l,h = 28,9 )
* ONE
* ZERO
* ONE{}N
* FOUR
* NULL$
* MINUSONE DUP
* % 1E-5 
* LAM o 

* define steptol field

* ' TOLMSG
* THIRTY TWENTYEIGHT            ( x,y = 16,27 )
* ONEHUNDRED NINE              ( l,h = 28,9 )
* ONE
* ZERO
* ONE{}N
* FOUR
* NULL$
* MINUSONE DUP
* % 1E-2 
* LAM q
*
* THREE DUP                        
* ' SETUPMSG
* #BF26
* DoInputForm ?SKIP ABORT
* ' LAM q STO ' LAM o STO ' LAM p STO
* ;

* NULLNAME SETUPMSG
* :: FIFTEEN #<>case FALSE
*  ' NoExitAction
*   ROMPTR IFMenuRow1
*   {
*   NullMenuKey
*   NullMenuKey
*   { "OK" :: TakeOver ROMPTR DoKeyOK ; }
*   }
*   &COMP 2Ob>Seco
*   TRUE
* ;
***************************************************************************************************

* NULLNAME TOLMSG
* :: FORTYSIX #<>case FALSE %0> TRUE ;

***************************************************************************************************

NULLNAME TESTTOL
::                      

* remark: overall f=0 test is also in LINESEARCH routine
 
getdx LAM p %> ?SEMI				 ( deltaX is too large => skip further tests... )
getf %2 %* %SQRT LAM o %< IT :: #A04 ERROROUT ;  ( test for f=0 )
geteqnum getvarnum #= ?SEMI 			 ( skip further test if this is a zeroing problem )
getgrad MATTRN getP MAT* MATABS 		 ( otherwise compute gradient )
getfold %2 %* %/ %SQRT DUP putcos 
LAM q %> ?SEMI 				( cos test not satiesfied => exit )
#BB0D ERROROUT                          ( else extremum! )
;

***********************************************************************************************