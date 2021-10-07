/* Question 3 */
/* Import data */ 
proc import datafile = 'C:\Users\jofro\dev\longi\data\beta_carotene_univar.csv'
 out = data
 dbms = CSV;
run;

/* Part A & B */
proc mixed data = data;
  class prepar time;
  model y = prepar time prepar*time / solution;
  repeated / subject=ID(prepar) type=UN;
  contrast "30 mg vs. 60 mg"
	prepar*time 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 -1 1 0 0 0,
	prepar*time 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 -1 0 1 0 0,
	prepar*time 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 -1 0 0 1 0,
	prepar*time 0 0 0 0 0 0 0 0 0 0 1 0 0 0 -1 -1 0 0 0 1;
  contrast "12-week vs. baseline"
    prepar*time 1 0 0 0 -1 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0,
	prepar*time 1 0 0 0 -1 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0,
	prepar*time 1 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 1;
run;

/* Part C */
data poly;
 set data;
 time_sq = time*time;
 time_cu = time*time*time;
run;

proc mixed data=poly;
  class prepar;
  model y = prepar time time_sq time_cu prepar*time prepar*time_sq prepar*time_cu / solution;
  repeated / subject=ID(prepar) type=UN;
run;

/* Part D */
proc import datafile = 'C:\Users\jofro\dev\longi\data\beta_carotene_univar_CLEAN.csv'
 out = clean
 dbms = CSV;
run;

proc mixed data=clean;
  class prepar time;
  model y = prepar baseline time / solution;
  repeated / subject=ID(prepar) type=UN;
  estimate 'linear' time -3 -1 1 3;
  estimate 'quadratic' time 1 -1 -1 1;
  estimate 'cubic' time -1 3 -3 1;
run;
