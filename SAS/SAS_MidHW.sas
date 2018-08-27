PROC IMPORT OUT= WORK.coil 
            DATAFILE= "C:\Users\shrey\Downloads\S4.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc contents data=work.coil;
run;

proc freq data=coil;
run;

%FreqReport(coil);
options mprint;
%DissGraphMakerLogOdds(coil,10,pwapar,resp);
options nomprint;

data temp;
set  coil;
SqrMoshoo=(Moshoo-6)**2;
DifMoshoo=abs(Moshoo-6);
run;


proc logistic data=temp;
model resp=Moshoo;
title "Logistic Model for Moshoo";
run;
proc logistic data=temp;
model resp=DifMoshoo;
title "Logistic Model for DifMoshoo";
run;
proc logistic data=temp;
model resp=SqrMoshoo;
title "Logistic Model for SqrMoshoo";
run;
title;


data coil2(keep=seqnum--MGODPR mrelge MFALLE MFWEKI MOPLHO 
mska--mzpart 
minkge--PMOTSC PAANHA
AWAPAR--AMOTSC resp respholdout)
;
set  coil ;
rand=ranuni(092765);
     if rand <=.7 then RespHoldout=.;
else if rand  >.7 then do;
   RespHoldout=Resp;
   Resp=.
   ;
end;
run;


data coil3;
set  coil2;
array orig[10](0, 1, 2,  3,  4,  5,   6,   7,    8,    9);
array new[10] (0,25,75,150,350,750,3000,7500,15000,30000);
retain orig1-orig10 new1-new10; 
do i=1 to dim(orig); 
 if pwapar=orig[i] then pwapar2=new[i];
 if PAANHA=orig[i] then PAANHA2=new[i];
 if PPERSA=orig[i] then PPERSA2=new[i];
end;
drop orig1--orig10 new1--new10 i; 
run;

proc freq data=coil3;
tables 	pwapar*pwapar2
		PAANHA*PAANHA2
		PPERSA*PPERSA2/list;
run;

data coil3;
set  coil3;
drop pwapar paanha ppersa;
run;




data coil4;
set  coil3;
array orig[10](0,  1, 2, 3, 4, 5, 6, 7, 8,  9);
array new[10] (0,5.5,17,30,43,56,69,82,94,100);
retain orig1-orig10 new1-new10; 
do i=1 to dim(orig); 
if MGODRK =orig[i] then MGODRK2 =new[i];
if MGODPR =orig[i] then MGODPR2 =new[i];
if MRELGE =orig[i] then MRELGE2 =new[i];
if MFALLE =orig[i] then MFALLE2 =new[i];
if MFWEKI =orig[i] then MFWEKI2 =new[i];
if MOPLHO =orig[i] then MOPLHO2 =new[i];
if MSKA   =orig[i] then MSKA2 =new[i];
if MSKB1  =orig[i] then MSKB12 =new[i];
if MSKB2  =orig[i] then MSKB22 =new[i];
if MSKC   =orig[i] then MSKC2 =new[i];
if MHHUUR =orig[i] then MHHUUR2 =new[i];
if MAUT1  =orig[i] then MAUT12 =new[i];
if MAUT2  =orig[i] then MAUT22 =new[i];
if MAUT0  =orig[i] then MAUT02 =new[i];
if MINKGE =orig[i] then MINKGE2 =new[i];
end;
drop orig1--orig10 new1--new10 i; 
run;


proc freq data=coil4;
tables
MGODRK*MGODRK2
MGODPR*MGODPR2
MRELGE*MRELGE2
MFALLE*MFALLE2
MFWEKI*MFWEKI2
MOPLHO*MOPLHO2
MSKA*MSKA2
MSKB1*MSKB12
MSKB2*MSKB22
MSKC*MSKC2
MHHUUR*MHHUUR2
MAUT1*MAUT12
MAUT2*MAUT22
MAUT0*MAUT02
MINKGE*MINKGE2
/list;
run;


data coil5;
set  coil4;
drop
MGODRK
MGODPR
MRELGE
MFALLE
MFWEKI
MOPLHO
MSKA
MSKB1
MSKB2
MSKC
MHHUUR
MAUT1
MAUT2
MAUT0
MINKGE
;
run;


data indep;                                                                   
set  coil5 (drop=resp seqnum moshoo mostyp);                                                 
run;                                                                          

 
%ObsAndVars(indep);                                                           
%varlist(indep);                                                              
%macro GraphLoop;     
options mprint; 
 %do i=1 %to &nvars;                                                               
   %let variable=%scan(&varlist,&i);                                          
%DissGraphMakerLogOdds(coil5,10,&variable,resp);
 %end;             
options nomprint; 
%mend GraphLoop;                                                              
%GraphLoop; 

%CatToBinWithDrop(coil5,seqnum,mostyp);
%CatToBinWithDrop(coil5,seqnum,MOSHOO);
*/
/*Replicate the programming below to test other variables for quadratic form*/;
data coil5;
set  coil5;
mgemomsq=mgemom**2;
run;

proc logistic data=coil5 descending;
model resp=mgemom;
title "Mgemom LINEAR Model";
run;
title;
proc logistic data=coil5 descending;
model resp=mgemom mgemomsq;
title "Mgemom QUADRATIC Model";
run;
title;

/*Checking if transformation neccessary for MGEMLE*/
data coil5;
set  coil5;
MGEMLESQ = MGEMLE**2;
run;

proc logistic data=coil5 descending;
model resp=MGEMLE;
title "MGEMLE LINEAR Model";
run;
title;
proc logistic data=coil5 descending;
model resp=MGEMLE MGEMLEsq;
title "MGEMLE QUADRATIC Model";
run;
title;

/*Checking if transformation neccessary for MAANTH*/
data coil5;
set  coil5;
MAANTHSQ = MAANTH**2;
run;

proc logistic data=coil5 descending;
model resp=MAANTH;
title "MAANTH LINEAR Model";
run;
title;
proc logistic data=coil5 descending;
model resp=MAANTH MAANTHsq;
title "MAANTH QUADRATIC Model";
run;
title;

/*Checking if transformation neccessary for MGODPR2*/
data coil5;
set  coil5;
MGODPR2SQ = MGODPR2**2;
run;

proc logistic data=coil5 descending;
model resp=MGODPR2;
title "MGODPR2 LINEAR Model";
run;
title;
proc logistic data=coil5 descending;
model resp=MGODPR2 MGODPR2sq;
title "MGODPR2 QUADRATIC Model";
run;
title;



/*Checking if transformation neccessary for MRELGE2*/
data coil5;
set  coil5;
MRELGE2SQ = MRELGE2**2;
run;

proc logistic data=coil5 descending;
model resp=MRELGE2;
title "MRELGE2 LINEAR Model";
run;
title;
proc logistic data=coil5 descending;
model resp=MRELGE2 MRELGE2sq;
title "MRELGE2 QUADRATIC Model";
run;
title;

/*Checking if transformation neccessary for MSKB12*/
data coil5;
set  coil5;
MSKB12SQ = MSKB12**2;
run;

proc logistic data=coil5 descending;
model resp=MSKB12;
title "MSKB12 LINEAR Model";
run;
title;
proc logistic data=coil5 descending;
model resp=MSKB12 MSKB12sq;
title "MSKB12 QUADRATIC Model";
run;
title;


data coil6;
set  coil5;
*Any final changes here;
run;

*This is MY MODEL.  Adjust the variables to make YOUR model;
proc logistic data=coil6 descending;
model resp=
mostyp_2
mostyp_3
mostyp_4
mostyp_5
mostyp_6
mostyp_7
mostyp_8
mostyp_9
mostyp_10
mostyp_11
mostyp_12
mostyp_13
mostyp_15
mostyp_16
mostyp_17
mostyp_18
mostyp_19
mostyp_20
mostyp_21
mostyp_22
mostyp_23
mostyp_24
mostyp_25
mostyp_26
mostyp_27
mostyp_28
mostyp_29
mostyp_30
mostyp_31
mostyp_32
mostyp_33
mostyp_34
mostyp_35
mostyp_36
mostyp_37
mostyp_38
mostyp_39
mostyp_40
mostyp_41
MINKGE2
MFWEKI2
MOSHOO_1
MOSHOO_2
MOSHOO_3
MOSHOO_4
MOSHOO_5
MOSHOO_6
MOSHOO_7
MOSHOO_8
MOSHOO_9
MOSHOO_10
MGEMLE
MRELGE2
MSKB22
MSKA2
MAUT02
MGEMOM
MAUT12
MGODRK2
MGODPR2
MAUT22
MSKC2
MFALLE2
MSKB12
MOPLHO2
MAANTH
MHHUUR2
PAANHA2
PWAPAR2
PPERSA2
AWAPAR
AMOTSC
APERSA
/selection=stepwise;
output out=scored p=pred;
run;

data holdout;
set  scored;
if   respholdout>.;
run;

proc sort data=holdout;
by descending pred;
run;

data RespAnal (keep=reccount respholdout cumcount cumresp);
set  holdout;
reccount=1;
cumcount+reccount;
cumresp+respholdout;
run;

proc freq data=RespAnal;
tables respholdout;
run;

*Change the denominators to match your total number of records and responses;
data RespAnal;
set  RespAnal;
RespPct=cumresp/105;
CountPct=cumcount/1678;
run;


data cutpoint;
set  RespAnal;
lagCountPct=lag(CountPct);
if   CountPct ge .75 and lagCountPct lt .75 then output;
run;

proc print data=cutpoint;
run;

/* Now right click on dataset RespAnal and select Copy to Clipboard*/
/* Create gains chart in Excel.  MAKE SURE YOU UNDERSTAND!! */


proc reg data=coil6;
model resp=mostyp_29 mostyp_31 mostyp_40 MOSHOO_2 MSKB22 PAANHA2 pwapar2;
run;
quit;
