proc import datafile = 'C:\Users\jofro\dev\longi\project\data_raw\month_clean.csv'
 out = data
 dbms = csv;
run;

/*proc print data=data; run;*/

/* UN no positive definite matrix */
/*proc mixed data = data;*/
/*  class neighborhood_id;*/
/*  model log_crime_rate = month / solution;*/
/*  random int / subject=neighborhood_id type=un;*/
/*  repeated / subject=neighborhood_id type=un;*/
/*run;*/

/* AR(1) AIC = -70.5 */
proc mixed data = data method=ml;
  class neighborhood_id;
  model log_crime_rate = month / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=ar(1);
run;

/* SP(POW) = infinite likelihood */
/*proc mixed data = data;*/
/*  class neighborhood_id;*/
/*  model log_crime_rate = month / solution;*/
/*  random int / subject=neighborhood_id type=un;*/
/*  repeated / subject=neighborhood_id type=sp(pow)(month);*/
/*run;*/

/* CS AIC = -20.4 */
/*proc mixed data = data;*/
/*  class neighborhood_id;*/
/*  model log_crime_rate = month / solution;*/
/*  random int / subject=neighborhood_id type=un;*/
/*  repeated / subject=neighborhood_id type=cs;*/
/*run;*/

/* CSH AIC = -60.2 */
proc mixed data = data method=ml;
  class neighborhood_id;
  model log_crime_rate = month / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=csh;
run;

/* TOEPH infinite likelihood */
/*proc mixed data = data;*/
/*  class neighborhood_id;*/
/*  model log_crime_rate = month / solution;*/
/*  random int / subject=neighborhood_id type=un;*/
/*  repeated / subject=neighborhood_id type=toeph;*/
/*run;*/

/* SP(EXP) infinite likelihood */
/*proc mixed data = data;*/
/*  class neighborhood_id;*/
/*  model log_crime_rate = month / solution;*/
/*  random int / subject=neighborhood_id type=un;*/
/*  repeated / subject=neighborhood_id type=sp(exp)(month);*/
/*run;*/

data poly;
 set data;
 month_sq = month*month;
 month_cu = month*month*month;
 month_qu = month*month*month*month;
run;

/* Poly terms */

/*proc print data=poly; run;*/

/* Square AR(1) AIC = -114.2 */
proc mixed data = poly method=ml;
  class neighborhood_id;
  model log_crime_rate = month month_sq / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=ar(1);
run;

/* Square CSH AIC = -113.0 */
proc mixed data = poly method=ml;
  class neighborhood_id;
  model log_crime_rate = month month_sq / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=csh;
run;

/* Cubic AR(1) AIC = -136.3 */
proc mixed data = poly method=ml;
  class neighborhood_id;
  model log_crime_rate = month month_sq month_cu / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=ar(1);
run;

/* Cubic CSH AIC = -143.2 */
proc mixed data = poly method=ml;
  class neighborhood_id;
  model log_crime_rate = month month_sq month_cu / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=csh;
run;

/* Quartic AR(1) AIC = -128.2 */
proc mixed data = poly method=ml;
  class neighborhood_id;
  model log_crime_rate = month month_sq month_cu month_qu / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=ar(1);
run;

/* Quartic CSH AIC = -136.2 */
proc mixed data = poly method=ml;
  class neighborhood_id;
  model log_crime_rate = month month_sq month_cu month_qu / solution;
  random int / subject=neighborhood_id type=un;
  repeated / subject=neighborhood_id type=csh;
run;
