libname stocks "C:\Users\shrey\Desktop\Spring 18\SAS\Week 14 - Final Exam";

proc contents data=stocks.Annualreports varnum;
run;

proc freq data=stocks.Annualreports;
table IndFinancialYearEnd;
run;

data work.Annualreports;
set stocks.Annualreports;
FiscalyearDate=datepart(IndFinancialYearEnd);
FiscalYear=Year(FiscalYearDate);
run;

proc freq data=work.Annualreports;
table FiscalYear;
run;

*Part 1;
data work.no2014;
set work.Annualreports;
if Fiscalyear<2014;
run;

proc freq data=work.no2014;
table FiscalYear;
run;

*Part 3;
proc freq data=work.no2014;
tables sector*industry/list missing missprint;
run;

data work.MyCompanies;
set work.no2014;
if Sector="Health Care" and Industry="Biotechnology: Electr";
run;

proc freq data=MyCompanies order=freq;
title "Number of Annual Report Records by name";
tables name;
run;
title;

proc freq data=MyCompanies;
tables Symbol*Name/List missing missprint;
title "Counts of Symbol by name--detect duplicates";
run;
title;
*Part 4;
proc sort nodupkey data=MyCompanies;
by name FiscalYear;
run;

*part 5;
proc freq data=MyCompanies;
tables Symbol*Name/List missing missprint;
title "Counts of Symbol by name--detect duplicates";
run;
title;

*Part 6;
data MyCompanies;
set MyCompanies;
NameCompressed=compress(Name," .(),");
run;

proc freq data=myCompanies order=freq;
tables Symbol*NameCompressed/list out=CompanyCounts;
run;

proc sort data=CompanyCounts;
by descending Count Symbol;
run;

data FourCompanies;
set CompanyCounts(obs=4);
run;

data WithBinaries;
set MyCompanies;
if NameCompressed="CONMEDCorporation" then CONMEDCorporation=1;
									  else CONMEDCorporation=0;	
if NameCompressed="EscalonMedicalCorp" then EscalonMedicalCorp=1;
									 else EscalonMedicalCorp=0;	
if NameCompressed="FonarCorporation" then FonarCorporation=1;
									 else FonarCorporation=0;	
if NameCompressed="ArrhythmiaResearchTechnolo" then ArrhythmiaResearchTechnolo=1;
									  		   else ArrhythmiaResearchTechnolo=0;	
run;

proc freq data=WithBinaries order=freq;
tables Name*CONMEDCorporation*EscalonMedicalCorp*FonarCorporation*ArrhythmiaResearchTechnolo/list nopercent nocum missing missprint;
run;

*Part 7;
data ForAnova;
set WithBinaries;
if CONMEDCorporation=1 or EscalonMedicalCorp=1 or FonarCorporation=1 or ArrhythmiaResearchTechnolo=1;
run;

data ConvertMetric;
set ForAnova;
PEToIndustryNew=input(PEToIndustry,8.);
run;

proc means data=ConvertMetric;
class symbol;
var PEToIndustryNew;
run;

proc anova data=ConvertMetric;
class symbol;
model PEToIndustryNew=symbol;
means symbol/snk;
run;
quit;

*Part 8;
*Matching the companies to the options file using ticker symbol;
proc sort nodupkey data=MyCompanies;
by symbol;
run;

data work.optionsFile;
set stocks.optionsFile (rename=(underlying=symbol));
if "01Apr2014"d <=expdate<="31Jan2015"d;
run;

proc sort data=optionsFile;
by symbol expdate strike;
run;

data MyOptions;
merge MyCompanies(in=OnCompanies keep=symbol)
	  work.optionsFile(in=OnOptions)
	  ;
by Symbol;
if OnCompanies and OnOptions;
run;

proc freq data=MyOptions;
table symbol;
run;

proc means data=MyOptions;
class symbol type;
var strike;
run;

proc summary data=MyOptions nway;
class symbol type;
var strike;
output out=optionsStrikes mean=;
run;

*Part 9;
data work.prices;
set stocks.prices;
year=year(date);
run;

proc means data=work.prices n nmiss min;
class year;
var date;
run;

proc summary data=work.prices nway;
class year;
var date;
output out=FirstTradingDayPerYear min=;
run;

proc print data=FirstTradingDayPerYear;
run;

data MyFirstTradingDay;
set stocks.prices;
if date="02Jan2013"d;
run;

proc sort data=MyFirstTradingDay;
by tic;
run;

data MyPriceFirstTradingDay;
merge MyCompanies(in=OnCompanies keep=symbol)
	  MyFirstTradingDay(in=OnPrices rename=(tic=symbol))
	  ;
by symbol;
if OnCompanies and OnPrices;
run;

data work.DivFile;
set stocks.DivFile;
where date ge "02Jan2013"d;
rename tic=symbol;
run;

data MyDividends;
merge MyPriceFirstTradingDay (in=OnPrice)
	  DivFile (in=OnDiv)
	  ;
by symbol;
if OnPrice and OnDiv;
run;

proc summary data=MyDividends nway;
class symbol adjclose;
var DivAmount;
output out=DivSum Sum=;
run;

data DivCalc;
format DivYield percent8.1;
set DivSum;
DivYield=DivAmount/AdjClose;
run;

*Part 10;
*Determining the minimum and maximum split amounts for my companies;
data work.splits (drop=date rename=(splitdate=date));
set stocks.splits;
Splitdate=input(date,YYMMDD10.);
format splitdate YYMMDD10.;
rename tic=symbol;
run;

data Mysplits;
merge Mycompanies(in=OnCompanies keep=symbol)
	  Splits(in=OnSplits)
	  ;
by symbol;
if OnCompanies and OnSplits
   and date ge "01Jan1990"d
   ;
run;

proc means data=MySplits max min;
Class symbol;
var split;
run; 

proc summary data=MySplits nway;
Class symbol;
var split;
output out=SplitminMax(drop=_type_) min=SplitMin max=Splitmax;
run;

*Part 11;
data OnePerSymbolStart;
merge MyCompanies (in=OnBase keep=Symbol)
	  SplitMinMax (in=OnSplits)
	  DivCalc	  (in=OnDiv)
	  ;
by Symbol;
if OnBase;
run;

proc freq data=MyOptions;
table symbol /out=OptionsCounts (drop=percent rename=(count=OptionsCount));
run;

proc transpose data=OptionsStrikes (drop=_type_ _freq_)
			   out=OptionsTransposed prefix=Strikeprice_;
by symbol; id type; var Strike;
run;

Option nolabel;
data OneperSymbolRound2;
merge MyCompanies (in=OnBase keep=Symbol)
	  SplitMinMax (in=OnSplits  rename=(_freq_=Splitcount))
	  DivCalc	  (in=OnDiv drop=_type_ _freq_ adjclose)
	  optionsCounts(in=OnOptions)
	  optionsTransposed (in=optionsPrices drop=_NAME_)
	  ;
by symbol;
if OnBAse;
run;
Options nolabel;

data onePerSymbolNoBlanks;
set OneperSymbolRound2;
format StrikePrice_C StrikePrice_P 8.2;
array numbervars _numeric_;
do over numbervars;
	if numbervars=. then numbervars=0;
end;
run;

data onePerSymbolNoBlanks;
set OneperSymbolRound2;
format StrikePrice_C StrikePrice_P 8.2;
array BlankToZero SplitCount DivYield  DivAmount OptionsCount;
do Over BlankToZero;
	if BlankToZero=. then BlankToZero=0;
end;
run;

*Part II;
*Piotrski method for stock pricing;
data MyCompany;
set stocks.AnnualReports;
format infoAvailDate FiscalYearDate YYMMDD10.;
where Sector="Health Care" and Industry="Biotechnology: Electr";
FiscalYearDate=datepart(IndFinancialYearEnd);
FiscalYear=Year(FiscalYearDate);
InfoAvailDate=input(IndDatePrelimLoaded,YYMMDD10.);
run;

proc sort data=MyCompany nodupkey;
by Symbol FiscalYear;
run; 

Data buildLags;
set MyCompany (keep= symbol FiscalYearDate FiscalYear InfoAvailDate roa CFNetCashFromTotalOperatingActi ROA ISTotalNetIncome LTDebtOfTotDebt TotDebtToTotAss CurrentRatio TotalCommonSharesOut GrossMargin AssetTurnOver BSLTDebt BSTotalAssets);
By symbol FiscalYearDate;
	PrevROA						=lag(ROA);
	PrevLTDebtofTotDebt			=lag(LTDebtofTotDebt);
	PrevTotDebtToTotAss			=lag(TotDebtToTotAss);
	PrevCurrentRatio			=lag(CurrentRatio);
	PrevTotalCommonSharesOut	=lag(TotalCommonSharesOut);
	PrevGrossMargin				=lag(GrossMargin);
	PrevAssetTurnOver			=lag(AssetTurnOver);
	PrevBSTotalAssets			=lag(BSTotalAssets);
run;

Data ScreenFirsts;
Set BuildLags;
By SYMBOL FiscalYearDate;
If first.Symbol then do;
	PrevROA						=.;
	PrevLTDebtofTotDebt			=.;
	PrevTotDebtToTotAss			=.;
	PrevCurrentRatio			=.;
	PrevTotalCommonSharesOut	=.;
	PrevGrossMargin				=.;
	PrevAssetTurnOver			=.;
	PrevBSTotalAssets			=.;
end;
run;

data BuildMetrics;
set ScreenFirsts;
By Symbol FiscalYearDate;
IF ROA>0 then S1=1;
		 else S1=0;
IF CFNetCashFromTotalOperatingActi >0 	then S2=1;
										else S2=0;
IF CFNetCashFromTotalOperatingActi > ISTotalNetIncome then S4=1;
													  else S4=0;
IF NOT First.Symbol then do;
	IF ROA > PrevROA then  S3=1;
					 else  S3=0;
	IF BSLTDebt>0 then do;
		If LTDebtofTotDebt*TotDebtToTotAss < (PrevLTDebtofTotDebt*PrevTotDebtToTotAss) then S5=1;
																					   else S5=0;
	end;
	else do;
		IF BSTotalAssets>PrevBSTotalAssets then S5=1;
										   else S5=0;
	end;
	If CurrentRatio > PrevCurrentRatio then S6=1;
								   	   else S6=0;
	If TotalCommonSharesOut <= PrevTotalCommonSharesOut then S7=1;
		   											    else S7=0;
	If GrossMargin>PrevGrossMargin then S8=1;
							   	   else S8=0;
	If AssetTurnOver > PrevAssetTurnOver then S9=1;
										 else S9=0;
end;
	Score=S1+S2+S3+S4+S5+S6+S7+S8+S9;
Run;

Data CutOffYear;
Set BuildMetrics;
If FiscalYear=2010;
run;

data WithScores;
Set CutOffYear;
If Score>.;
run;

Data GetPrices;
Merge WithScores	(in=OnBase)
	  Stocks.prices (in=Onprices rename=(tic=symbol) keep=tic date close adjClose)
	  ;
by Symbol;
if OnBase and date=InfoAvailDate;
run;

proc freq data=GetPrices;
Tables symbol;
Title "GetPrices";
run;
title;

data GetPrices2;
merge WithScores		(in=OnBase)
	  Stocks.prices	(in=OnPrices rename=(tic=symbol) keep=tic date close adjclose)
	  ;
by symbol;
if OnBase and InfoAvailDate<=Date<=InfoAvailDate + 5;
run;

proc freq data=GetPrices2;
tables symbol;
title "GetPrices 2";
run;
title;

data GetPricesFirst;
set GetPrices2;
by symbol date;
if first.symbol;
run;

data MyCompaniesOneYearLater (keep=symbol FiscalYear InfoAvailDate);
set stocks.AnnualReports;
format InfoAvailDate YYMMDD10.;
where Sector="Health Care" and Industry="Biotechnology: Electr";
FiscalYearDate=datepart(IndFinancialYearEnd);
FiscalYear=Year(FiscalYearDate);
InfoAvailDate=input(IndDatePrelimLoaded,YYMMDD10.);
if FiscalYear=2011;
run;

Data OneYearlaterWithPrice;
merge MyCompaniesOneYearLater 	(in=OnCompanies)
	  Stocks.prices				(in=OnPrices rename=(tic=symbol adjClose=laterAdjClose) keep=tic date close adjClose)
	  ;
by symbol;
if InfoAvailDate-5<=date<=InfoAvailDate-1;
run;

data PriceBeforeNextReport;
set OneYearlaterWithPrice;
by symbol date;
if last.symbol;
run;

data EvalBeforeNextReport;
merge GetPricesFirst (in=OnBase)
	  PriceBeforeNextReport (in=OnNext)
	  ;
by Symbol;
if OnBase;
return=(laterAdjClose-AdjClose)/AdjClose;
run;

proc plot data=EvalBeforeNextReport;
Plot return*score=' ' $symbol;
run;
quit;

data MuchLaterPrice (keep=tic adjClose rename=(tic=Symbol adjClose=AdjClose2014));
set stocks.prices;
if date="02Jan2014"d;
run;

data LaterReturn;
merge EvalBeforeNextReport (in=OnBase)
	  MuchLaterPrice	   (in=OnLater)
	  ;
by symbol;
if OnBase;
return2014=(adjClose2014-AdjClose)/AdjClose;
run;

proc plot data=LaterReturn;
plot return2014*score=' ' $symbol;
run;
quit;

proc reg data=LaterReturn;
model return=Score;
run;
proc reg data=LaterReturn;
model return2014=Score;
run;
quit;
