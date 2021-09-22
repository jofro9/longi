/* Question 2 */
/* Import data */ 
proc import datafile = 'C:\Users\jofro\dev\longi\data\cholesterol.csv'
 out = data1
 dbms = CSV;
run;

/* add subject variable to data */
data wide;
 set data1;
 subject+1;
run;

/* 
	transpose data by subject: 
	'let' transposes only the last observation for each 'by' group
	rename _name_ and col1
*/
proc transpose data=wide out=long (rename=(_name_=time col1=cholesterol)) let;
 by subject;
run;

/* linear mixed model with unstructured covariance */
proc mixed data = long method = ml;
 class time;
 model cholesterol = time / solution;
 repeated / type=UN;
run;

/* Question 4 */
/* import data */
proc import datafile = 'C:\Users\jofro\dev\longi\data\global_temp_anomalies.csv'
 out = data2
 dbms = CSV;
run;

/* mixed model with AR(1) structure */
proc mixed data = data2 method=ml plots=all;
 model temp=year / solution outp=tempout;
 repeated / type=AR(1) subject=intercept;
run;

/* plot the residulas vs time */
symbol1 v=dot i=none;
proc gplot data=tempout;
 plot resid*year;
 title 'Residuals vs Year, Global Temperature Anomalies';
run;

/* transform the residuals */
data lag;
 set tempout;
 resid_lag = Resid + lag1(Resid) * 0.7395;
run;

/* plot the transformed residuals */
symbol1 v=dot i=none;
proc gplot data=lag;
 plot resid_lag*year;
 title 'Lagged Residuals vs Year, Global Temperature Anomalies';
run;

/* transform the year variable to quadratic and cubic terms */
data poly;
 set data2;
 year_sq = year*year;
 year_cu = year*year*year;
run;

/* cubic mixed model */
proc mixed data=poly method=ml plots=all;
 model temp=year year_sq year_cu / solution outp=tempout;
 repeated / type=AR(1) subject=intercept;
run;

/* non-parametric model */
proc loess data=data2;
 ods output scoreresults=scoreout
 	 outputstatistics=statout;
 model temp = year / smooth= 0.3 residual clm degree=1;
 score data=tempout / clm;
run;

/* plot residuals from non-parametric model */
symbol1 v=dot i=none;
proc gplot data=scoreout;
 plot resid*year;
 title 'Loess Residuals vs Year, Global Temperature Anomalies';
run;

/* Residual Histogram */
proc univariate data=scoreout;
 var resid;
 histogram;
run;
