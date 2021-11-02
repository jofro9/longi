/* Question 2 */
/* Import data */ 
proc import datafile = 'C:\Users\jofro\dev\longi\data\p1np.csv'
 out = data
 dbms = CSV;
run;

/* Question A */
/* UN@UN structure */
/* Doesn't converge, infinite likelihood */
proc mixed data=data method=ml;
  class day time;
  model p1np=day time day*time / solution;
  repeated day time / type=un@un subject=id r rcorr;
run;

/* UN@AR(1) */
proc mixed data=data method=ml;
  class day time;
  model p1np=day time day*time / solution;
  repeated day time / type=un@ar(1) subject=id r rcorr;
run;

/* UN@CS */
/* better AIC */
proc mixed data=data method=ml;
  class day time;
  model p1np=day time day*time / noint solution;
  repeated day time / type=un@cs subject=id r rcorr;
run;

/* Question C */
proc mixed data=data method=ml;
  class day time;
  model p1np=day time day*time / noint solution;
/*  repeated day time / type=un@cs subject=id;*/
  random intercept / subject=id v vcorr;
run;

/* Question D */
data data2;
  set data;
  timec = time;
run;

proc mixed data=data2 method=ml;
  class day time;
  model p1np=day timec day*timec / noint solution;
  repeated day time / type=un@cs subject=id r rcorr;
run;

proc mixed data=data2 method=ml;
  class day time;
  model p1np=day timec day*timec day*timec*timec / noint solution;
  repeated day time / type=un@cs subject=id r rcorr;
run;

/* Question E */
proc mixed data=data2 method=reml;
  class day time;
  model p1np=day day*timec day*timec*timec / noint solution;
  repeated day time / subject=id type=un@cs r rcorr;
  contrast "linear trend" day*timec 0 1;
  contrast "linear trend 2" day*timec -1 1;
  contrast "quadratic trend" day*timec*timec 0 1;
  contrast "trend" day*timec 0 1,
  				   day*timec*timec 0 1;
  contrast "treatment vs. reference" day*timec -1 1,
  									 day*timec*timec -1 1;
run;
