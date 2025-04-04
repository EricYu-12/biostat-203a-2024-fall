libname Final "/home/u64043546/Biostat203A/Final";
run;

PROC IMPORT OUT= Final.heart
            DATAFILE= "/home/u64043546/Biostat203A/Final/heart.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2;
RUN;

DATA Final.heart (label="Heart Data for Subjects");
 	infile "/home/u64043546/Biostat203A/Final/heart.csv"  DSD firstobs=2;
 	informat Age best32. ;
                informat Sex $1. ;
                informat ChestPainType $3. ;
                informat RestingBP best32. ;
                informat Cholesterol best32. ;
                informat FastingBS best32. ;
                informat RestingECG $6. ;
                informat MaxHR best32. ;
                informat ExerciseAngina $1. ;
                informat Oldpeak best32. ;
                informat ST_Slope $4. ;
                informat HeartDisease best32. ;
                format Age best12. ;
                format Sex $1. ;
                format ChestPainType $3. ;
                format RestingBP best12. ;
                format Cholesterol best12. ;
                format FastingBS best12. ;
                format RestingECG $6. ;
                format MaxHR best12. ;
                format ExerciseAngina $1. ;
                format Oldpeak best12. ;
                format ST_Slope $4. ;
                format HeartDisease best12. ;
             input
                         Age
                         Sex  $
                         ChestPainType  $
                         RestingBP 
                         Cholesterol
                         FastingBS  
                         RestingECG   $
                         MaxHR 
                         ExerciseAngina  $
                         Oldpeak 
                         ST_Slope  $
                         HeartDisease ;
	         label Age = "Age"
	               Sex = "Gender"
	               ChestPainType = "Chest Pain Type"
	               RestingBP = "Resting Blood Pressure"
	               Cholesterol = "Cholesterol Level"
	               FastingBS= "Fasting Blood Sugar"
	               RestingECG = "Resting Electrocardiogram Results"
	               MaxHR = "Maximum Heart Rate Achieved"
	               ExerciseAngina = "Exercise-Induced Angina"
	               Oldpeak = "ST"
	               ST_Slope ="The Slope of The Peak Exercise ST Segment"
	               HeartDisease = "Has Heart Disease";

RUN;
proc format;
    value $Chest_Pain_Type_Group
        "ATA" = 'Atypical Angina'
        "TA" = 'Typical Angina'
        "NAP" = 'Non-Anginal Pain'
        "ASY" = 'Asymptomatic';
run;

PROC FORMAT;
value heartdisease_no_yes 0 = 'No'
             1  = 'Yes';
RUN;

proc format;
value $ExerciseAngina_no_yes
        "N" = 'No'
        "Y" = 'Yes';
run;

DATA Final_heart_formatted;
set Final.heart;
format ChestPainType Chest_Pain_Type_Group.
HeartDisease heartdisease_no_yes.
ExerciseAngina ExerciseAngina_no_yes.;
RUN;


PROC MEANS data = Final_heart_formatted n mean std median min max nonobs maxdec = 3;
var  Age RestingBP Cholesterol MaxHR;
class ChestPainType;
title height=14pt "Baseline Characteristics of Study Population";
run;

proc tabulate data=Final_heart_formatted;
   class ChestPainType ;
   class RestingECG;
   class ExerciseAngina;
  tables RestingECG ExerciseAngina,
         (all='Chest Pain Types' ChestPainType)* (n='Count' colpctn='Col %')
         ;
run;

PROC FREQ data = Final_heart_formatted;
tables ChestPainType * HeartDisease /nocol norow;
RUN;
ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=Final_heart_formatted;
	title height=14pt "Heart Diseases Frequencies Amongst Each Chest Pain Type";
	vbar HeartDisease / group=ChestPainType groupdisplay=cluster datalabel;
	yaxis grid;
run;
ods graphics / reset;
title;

DATA Final.ATA;
set Final_heart_formatted;
where ChestPainType = "ATA";
RUN;

DATA Final.TA;
set Final_heart_formatted;
where ChestPainType = "TA";
RUN;

DATA Final.NAP;
set Final_heart_formatted;
where ChestPainType = "NAP";
RUN;

DATA Final.ASY;
set Final_heart_formatted;
where ChestPainType = "ASY";
RUN;

/* ATA */
ods output ClassificationTable=ctable_data ROCCurve=roc_data;

proc logistic data=Final.ATA plots=(roc);
	class RestingECG ExerciseAngina (param=ref ref="No") / param=glm;
	model HeartDisease(event='Yes')= Age RestingBP Cholesterol MaxHR RestingECG ExerciseAngina 
		/ link=logit ctable pprob=0.01 to 0.99 by 0.01 technique=fisher;
	output out=Final.ATA_heartdisease_prob predicted =ATA_heartdisease_predicted;
run;

ods output close;

data optimal_cutoff;
   set roc_data;
   sensitivity = _sensit_;       /* Extract sensitivity */
   specificity = 1 - _1mspec_;   /* Calculate specificity */
   sum_sens_spec = sensitivity + specificity; /* Compute the sum */
run;

proc sort data=optimal_cutoff;
   by descending sum_sens_spec;
run;

proc print data=optimal_cutoff(obs=1); /* Display the optimal cutoff */
run;

/*Formating 0 and 1 Predictions as No or Yes*/
PROC FORMAT;
value prediction_no_yes 0 = 'No'
             1  = 'Yes';
RUN;
/*Use Probability that Maximizes the sum of specificiy and sensitivity from CTABLE*/
data Final.heart_ATA_LogRes;
set Final.ATA_heartdisease_prob;
if ATA_heartdisease_predicted >= 0.199 then predicted = 1;
else predicted = 0;
format predicted prediction_no_yes.;
run;

/*Generating Confusion Matrix for ATA Group. */
proc freq data=Final.heart_LogRes;
    tables HeartDisease*predicted / out = Final.ATA_Heart_CM;
    title "Confusion Matrix of Predicted Heart Disease Occurences Amongst ATA Group using a Logistic Regression Model";
run;




/* TA */
ods output ClassificationTable=ctable_data ROCCurve=roc_data;

proc logistic data=Final.TA plots=(roc);
	class RestingECG ExerciseAngina (param=ref ref="No") / param=glm;
	model HeartDisease(event='Yes')= Age RestingBP Cholesterol MaxHR RestingECG ExerciseAngina 
		/ link=logit ctable pprob=0.01 to 0.99 by 0.01 technique=fisher;
	output out=Final.TA_heartdisease_prob predicted =TA_heartdisease_predicted;
run;

ods output close;

data optimal_cutoff;
   set roc_data;
   sensitivity = _sensit_;       /* Extract sensitivity */
   specificity = 1 - _1mspec_;   /* Calculate specificity */
   sum_sens_spec = sensitivity + specificity; /* Compute the sum */
run;

proc sort data=optimal_cutoff;
   by descending sum_sens_spec;
run;

proc print data=optimal_cutoff(obs=1); /* Display the optimal cutoff */
run;

/*Formating 0 and 1 Predictions as No or Yes*/
PROC FORMAT;
value prediction_no_yes 0 = 'No'
             1  = 'Yes';
RUN;
/*Use Probability that Maximizes the sum of specificiy and sensitivity from CTABLE*/
data Final.heart_TA_LogRes;
set Final.TA_heartdisease_prob;
if TA_heartdisease_predicted >= 0.475 then predicted = 1;
else predicted = 0;
format predicted prediction_no_yes.;
run;

/*Generating Confusion Matrix for ATA Group. */
proc freq data=Final.heart_TA_LogRes;
    tables HeartDisease*predicted / out = Final.TA_Heart_CM;
    title "Confusion Matrix of Predicted Heart Disease Occurences Amongst TA Group using a Logistic Regression Model";
run;




/* NAP */
ods output ClassificationTable=ctable_data ROCCurve=roc_data;

proc logistic data=Final.NAP plots=(roc);
	class RestingECG ExerciseAngina (param=ref ref="No") / param=glm;
	model HeartDisease(event='Yes')= Age RestingBP Cholesterol MaxHR RestingECG ExerciseAngina  
		/ link=logit ctable pprob=0.01 to 0.99 by 0.01 technique=fisher;
	output out=Final.NAP_heartdisease_prob predicted =NAP_heartdisease_predicted;
run;

ods output close;

data optimal_cutoff;
   set roc_data;
   sensitivity = _sensit_;       /* Extract sensitivity */
   specificity = 1 - _1mspec_;   /* Calculate specificity */
   sum_sens_spec = sensitivity + specificity; /* Compute the sum */
run;

proc sort data=optimal_cutoff;
   by descending sum_sens_spec;
run;

proc print data=optimal_cutoff(obs=1); /* Display the optimal cutoff */
run;

/*Formating 0 and 1 Predictions as No or Yes*/
PROC FORMAT;
value prediction_no_yes 0 = 'No'
             1  = 'Yes';
RUN;
/*Use Probability that Maximizes the sum of specificiy and sensitivity from CTABLE*/
data Final.heart_NAP_LogRes;
set Final.NAP_heartdisease_prob;
if NAP_heartdisease_predicted >= 0.441 then predicted = 1;
else predicted = 0;
format predicted prediction_no_yes.;
run;

/*Generating Confusion Matrix for ATA Group. */
proc freq data=Final.heart_NAP_LogRes;
    tables HeartDisease*predicted / out = Final.NAP_Heart_CM;
    title "Confusion Matrix of Predicted Heart Disease Occurences Amongst NAP Group using a Logistic Regression Model";
run;



/* ASY */
ods output ClassificationTable=ctable_data ROCCurve=roc_data;

proc logistic data=Final.ASY plots=(roc);
	class RestingECG ExerciseAngina (param=ref ref="No") / param=glm;
	model HeartDisease(event='Yes')= Age RestingBP Cholesterol MaxHR RestingECG ExerciseAngina RestingECG 
		/ link=logit ctable pprob=0.01 to 0.99 by 0.01 technique=fisher;
	output out=Final.ASY_heartdisease_prob predicted =ASY_heartdisease_predicted;
run;

ods output close;

data optimal_cutoff;
   set roc_data;
   sensitivity = _sensit_;       /* Extract sensitivity */
   specificity = 1 - _1mspec_;   /* Calculate specificity */
   sum_sens_spec = sensitivity + specificity; /* Compute the sum */
run;

proc sort data=optimal_cutoff;
   by descending sum_sens_spec;
run;

proc print data=optimal_cutoff(obs=1); /* Display the optimal cutoff */
run;

/*Formating 0 and 1 Predictions as No or Yes*/
PROC FORMAT;
value prediction_no_yes 0 = 'No'
             1  = 'Yes';
RUN;
/*Use Probability that Maximizes the sum of specificiy and sensitivity from CTABLE*/
data Final.heart_ASY_LogRes;
set Final.ASY_heartdisease_prob;
if ASY_heartdisease_predicted >= 0.750 then predicted = 1;
else predicted = 0;
format predicted prediction_no_yes.;
run;

/*Generating Confusion Matrix for ATA Group. */
proc freq data=Final.heart_ASY_LogRes;
    tables HeartDisease*predicted / out = Final.ASY_Heart_CM;
    title "Confusion Matrix of Predicted Heart Disease Occurences Amongst ASY Group using a Logistic Regression Model";
run;





