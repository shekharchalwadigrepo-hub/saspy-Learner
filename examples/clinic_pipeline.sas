proc import datafile="patients.csv" out=work.patients dbms=csv replace;
  getnames=yes;
run;

data adults;
  set patients;
  where age ge 18;
  bmi = weight / (height ** 2);
  if bmi ge 30 then obese = 1;
  else obese = 0;
  keep subject age bmi obese;
run;

proc sort data=patients out=patients_s nodupkey;
  by site descending enrolled;
run;

proc means data=patients n mean std min max;
  class site sex;
  var age bmi;
  output out=stats mean=;
run;

proc freq data=patients;
  tables site * sex / missing;
run;

data both;
  merge demo labs;
  by subject;
run;

proc sql;
  create table site_mean as
  select site, mean(age) as mean_age
  from patients
  where age ge 18
  group by site
  order by site;
quit;

proc export data=adults outfile="adults.csv" dbms=csv replace;
run;
