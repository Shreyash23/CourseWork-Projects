


/**Adjust Library Name  **/
*libname Lec7 "C:\Users\shrey\Desktop\Spring 18\SAS\week 9";
*libname Lec7 "D:\School\IDS462\Fall2007\Lectures\Lecture7";
libname Lec7 "C:\Users\shrey\Desktop\Spring 18\SAS\week 9";
data holdout;
set  Lec7.ChaidOutput;
if   respholdout>.;
run;

proc sort data=holdout;
by descending VR_RESP;
run;

data RespAnalCHAID (keep=reccount respholdout cumcount cumresp);
set  holdout;
reccount=1;
cumcount+reccount;
cumresp+respholdout;
run;

data RespAnalCHAID;
set  RespAnalCHAID;
RespPct=cumresp/106;
CountPct=cumcount/1806;
run;


data cutpoint;
set  RespAnalCHAID;
lagCountPct=lag(CountPct);
if   CountPct ge .75 and lagCountPct lt .75 then output;
run;

proc print data=cutpoint;
run;

data holdout;
set  Lec7.NNOutput;
if   respholdout>.;
run;

proc sort data=holdout;
by descending VN_RESP;
run;

data RespAnalNN (keep=reccount respholdout cumcount cumresp);
set  holdout;
reccount=1;
cumcount+reccount;
cumresp+respholdout;
run;

data RespAnalNN;
set  RespAnalNN;
RespPct=cumresp/106;
CountPct=cumcount/1806;
run;


data cutpoint;
set  RespAnalNN;
lagCountPct=lag(CountPct);
if   CountPct ge .75 and lagCountPct lt .75 then output;
run;

proc print data=cutpoint;
run;

/*Assumes that RespAnalLR has been produced from a Logistic Regression model  */

data  compare;
merge Lec7.RespAnalLR    (keep=cumresp rename=(cumresp=LRCumResp))
           RespAnalNN    (keep=cumresp rename=(cumresp=NNCumResp))
	   RespAnalChaid (keep=cumresp rename=(cumresp=CHCumResp))
	  ;
mailed=12.25*_n_;
Vcost=mailed;
Fcost=8000;
Tcost=Vcost+Fcost;
RevLR=LRCumResp*475;
RevCH=CHCumResp*475;
RevNN=NNCumResp*475;
PrLR=RevLR-Tcost;
PrCH=RevCH-Tcost;
PrNN=RevNN-Tcost;
RoILR=PrLR/Tcost;
RoICH=PrCH/Tcost;
RoINN=PrNN/Tcost;
run;

**Stop Here!!;

