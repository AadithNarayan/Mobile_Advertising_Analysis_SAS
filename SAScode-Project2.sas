ODS HTML;
ODS LISTING CLOSE;
ODS GRAPHICS ON;


LIBNAME Project2 "E:\Users\axr180085\Desktop\Project";
data ad;
set Project2.DATA;
run;

proc surveyselect data=ad out=ad_smp outall samprate=0.7 seed=10;
run;

data ad_train ad_test;
set ad_smp;
if selected then output ad_train; 
else output ad_test;
run;


/* 1. Linear probability model*/

proc glmmod data=ad_smp outdesign=LPM_ind noprint; 
class device_platform_class;
model install = device_volume wifi resolution device_height device_width publisher_id_class device_os_class device_make_class device_platform_class/noint;
weight selected;
run;

proc glmmod data=ad_train outdesign=ad_train_ind noprint; 
class device_platform_class;
model install = device_volume wifi resolution device_height device_width publisher_id_class device_os_class device_make_class device_platform_class/noint;
run;

 
proc glmmod data=ad_test outdesign=ad_test_ind noprint; 
class device_platform_class;
model install = device_volume wifi resolution device_height device_width publisher_id_class device_os_class device_make_class device_platform_class/noint;
run;

proc contents data= ad_train_ind; 
run;

/* Initial Model: Linear probability model using PROC reg */
proc reg data=ad_train_ind  PLOTS(MAXPOINTS=100000);
 model install = col1 - col10 ;
/*  OUTPUT OUT=LPM pred=p ;*/
quit; 


data ad_logged;
set Project2.DATA;
log_dw = log(device_width);
log_res = log(resolution);
log_dv = log(device_volume);
log_dh = log(device_height);

run;
proc surveyselect data=ad_logged out=ad_smp_logged outall samprate=0.6 seed=76;
run;

data ad_train_logged ad_test_logged;
set ad_smp_logged;
if selected then output ad_train_logged; 
else output ad_test_logged;
run;

proc glmmod data=ad_smp_logged outdesign=ad_outputlog noprint;
class device_platform_class;
model install = publisher_id_class device_make_class device_platform_class device_os_class device_height device_width resolution device_volume wifi
log_dw log_dv log_res log_dh;
weight selected;
run;

proc contents data=ad_outputlog;
run;

proc reg data=ad_outputlog;
Withoutlog : model install = col2 - col11;
Logmodel : model install = col2- col6 col11-col15;
weight selected;
output out=ad_problog pred=p;
quit;

/* Stepwise Selection*/
proc glmselect data=ad_train_ind plots=all;
model install = col1- col10/selection=stepwise(select=sl) stats=all showpvalues;
run;

proc contents data=ad_train_ind ;
run;





/* Final Model: Linear probability model using PROC reg */
proc reg data=ad_train_ind plots(maxpoints=100000);
model install = col2- col3 col6-col8  ;
quit;

/* Logistic Regression */
/* Initial model - Logistic Regression */
/*output 21,22,23 */
proc logistic data=ad_train_ind;
 logit: model install (event='1') = col1 - col10; 
run;


/* Stepwise Selection*/
proc logistic data=ad_train_ind outest=ans covout;
logit: model install (event='1') = col1 - col10
/ selection=stepwise
                  slentry=0.25
                  slstay=0.35
                  details
                  lackfit;
run;



/* Final model - Logistic Regression */
/* (i) Estimation of the model without considering rare events*/
proc logistic data=ad_smp;
class device_platform_class;
 logit: model install (event='1') = device_volume wifi resolution device_height device_width publisher_id_class device_os_class device_make_class device_platform_class;
run;

/*(ii)b. Estimate the model using oversampling approach for handling rare events and then applying the correction to obtain the corrected intercept */
proc freq data=ad_smp;
table install / out=fullpct(where=(install=1) rename=(percent=fullpct));
title "response counts in full data set";
run;

data sub;
set ad_smp;
if install =1 or (install =0 and ranuni(75302)<1/119) then output;
run;
proc freq data=sub;
table install / out=subpct(where=(install =1) rename=(percent=subpct));
title "Response counts in oversmp, subset data set";
run;

data sub;
set sub;
if _n_=1 then set fullpct(keep=fullpct);
if _n_=1 then set subpct(keep=subpct);
p1=fullpct/100; r1=subpct/100;
w=p1/r1; if install =0 then w=(1-p1)/(1-r1);
off=log( (r1*(1-p1)) / ((1-r1)*p1) );
run;
proc logistic data=sub;
class device_platform_class;
model install (event="1")=device_volume wifi resolution device_height device_width publisher_id_class device_os_class device_make_class device_platform_class;
output out=out p=pnowt;
title "True Parameters: -8.1248 (intercept)";
title2 "Unadjusted Model";
run;
proc logistic data=out;
class device_platform_class;
model install (event="1")=device_volume wifi resolution device_height device_width publisher_id_class device_os_class device_make_class device_platform_class;
weight w;
output out=out p=pwt;
title2 "Weight-adjusted Model";
run;
proc logistic data=out;
class device_platform_class;
model install (event="1")=device_volume wifi resolution device_height device_width publisher_id_class device_os_class device_make_class device_platform_class / offset=off;
output out=out xbeta=xboff;
title2 "Offset-adjusted Model";
run;





/* Classification and ROC */

/* Initial Model - Linear probability model */
proc reg data=LPM_ind  PLOTS(MAXPOINTS=100000);
linear: model install = col1 - col10 ;
output out=ad_linear_initial p=linear_predictions;
weight selected; 
quit;

/* To plot ROC curve based on predictions from linear model */
proc logistic data=ad_linear_initial plots=roc(id=prob);
model install (event='1') = col1 - col10/ nofit;
roc pred=linear_predictions;
where selected=0;
run;


/* Final Model: Linear probability model using PROC reg */

proc reg data=LPM_ind  PLOTS(MAXPOINTS=100000);
linear: model install = col1 - col8 ;
output out=ad_linear_final p=linear_predictions;
weight selected; 
quit;

/* To plot ROC curve based on predictions from linear model */
proc logistic data=ad_linear_final  plots=roc(id=prob);
model install (event='1') = col1 - col8/ nofit;
roc pred=linear_predictions;
where selected=0;
run;



/*Initial Model- Logistic regression */
proc logistic data=ad_train_ind;
logit: model install (event='1') = col1 - col10; 
score data=ad_test_ind out=ad_logit_predict_initial; 
run;

/*ROC curve on test data */
proc logistic data=ad_logit_predict_initial plots=roc(id=prob);
model install (event='1') = col1 - col10/ nofit;
roc pred=p_1;
run;



/* Final model - Logistic Regression */
/* Estimation of the model without considering rare events*/
proc logistic data=ad_train_ind;
logit: model install (event='1') = col1 - col8; 
score data=ad_test_ind out=ad_logit_predfin; 
run;

/*ROC curve on test data */
proc logistic data=ad_logit_predfin plots=roc(id=prob);
model install (event='1') = col1 - col8/ nofit;
roc pred=p_1;
run;


/* PART 2 */
/* (i) ROC Table for Initial Logistic Regression Model */
proc logistic data=ad_train_ind outmodel=train_roc_1;
logit: model install (event='1') = col1 - col10; 
run;

proc logistic inmodel=train_roc_1;
score data=ad_test_ind outroc=logged_roc_2;
run;

data ad_threshold1;
set logged_roc_2;
total_cost1 = _FALPOS_*0.01 + _FALNEG_*1;
run;
proc sql; 
create table total_cost1 as(select*,min(total_cost1) as min_cost from ad_threshold1); 
run;

/* (ii) ROC Table for Final Logistic Regression Model */
proc logistic data=ad_train_ind outmodel=train_roc_2;
logit: model install (event='1') = col1 - col8; 
run;

proc logistic inmodel=train_roc_2;
score data=ad_test_ind outroc=logged_roc_2;
run;
data ad_threshold2;
set logged_roc_2;
total_cost2 = _FALPOS_*0.01 + _FALNEG_*1;
run;
proc sql; 
create table total_cost2 as(select*,min(total_cost2) as min_cost from ad_threshold2); 
run;


/* (ii) ROC Table for Initial Linear Probability Model */
data tab_a (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.001 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_a as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from tab_a;
quit;


data table_b (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.005 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_b as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_b;
quit;

data table_c (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.010 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_c as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_c;
quit;


data table_d (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.015 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_d as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_d;
quit;


data table_e (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.020 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_e as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_e;
quit;


data table_f (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.025 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_f as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_f;
quit;


data table_g (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.030 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_g as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_g;
quit;


data table_h (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.035 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_h as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_h;
quit;


data table_i (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.040 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_i as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_i;
quit;


data table_j (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.045 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_j as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_j;
quit;


data table_k (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.050 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_k as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_k;
quit;


data initial_linear_roc_temp;
input probability false_positive false_negative;
datalines;
0.001 36061 0
0.005 32438 0
0.010 7611 0
0.015 125 0
0.020 0 0
0.025 0 0
0.030 0 0
0.035 0 0
0.040 0 0
0.045 0 0
0.050 0 0 
;
run;

data initial_linear_roc;
set initial_linear_roc_temp;
total_cost = false_positive*0.01 + false_negative*1;
run;


/* (ii) ROC Table for Final Linear Probability Model */

data tab_a1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_final;
where selected=0;
if linear_predictions > 0.001 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_a1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from tab_a1;
quit;


data table_b1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.005 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_b1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_b1;
quit;

data table_c1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.010 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_c1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_c1;
quit;


data table_d1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.015 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_d1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_d1;
quit;


data table_e1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.020 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_e1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_e1;
quit;


data table_f1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.025 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table cnt_f1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_f1;
quit;


data table_g1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.030 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_g1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_g1;
quit;


data table_h1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.035 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_h1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_h1;
quit;


data table_i1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.040 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_i1 as select*,count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_i1;
quit;


data table_j1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.045 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_j1 as select count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_j1;
quit;


data table_k1 (keep=install selected linear_predictions predicted false_pos false_neg);
set ad_linear_initial;
where selected=0;
if linear_predictions > 0.050 then predicted=1;
if install=0 and predicted=1 then false_pos=1;
if install=1 and predicted=0 then false_neg=1;
run;

proc sql;
create table count_k1 as select count(false_pos) as cnt_fp, count(false_neg) as cnt_fn from table_k1;
quit;


data final_temp;
input probability false_positive false_negative;
datalines;
0.001 36067 0
0.005 32438 0
0.010 7611 0
0.015 125 0
0.020 0 0
0.025 0 0
0.030 0 0
0.035 0 0
0.040 0 0
0.045 0 0
0.050 0 0 
;
run;

data final_linear_roc;
set final_temp;
total_cost = false_positive*0.01 + false_negative*1;
run;




