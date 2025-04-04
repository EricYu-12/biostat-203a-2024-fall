libname MIDTERM "/home/u64043140/BIOSTAT 203A/Midterm Project/";
run;

/*Reading in the data*/
PROC IMPORT OUT= MIDTERM.demographic
            DATAFILE= "/home/u64043140/BIOSTAT 203A/Midterm Project/healthcare-dataset-stroke-demographic-data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= MIDTERM.medical
            DATAFILE= "/home/u64043140/BIOSTAT 203A/Midterm Project/healthcare-dataset-stroke-medical-data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

DATA Demographic (label="Demographic Data for Subjects");
 	infile "/home/u64043140/BIOSTAT 203A/Midterm Project/healthcare-dataset-stroke-demographic-data.csv"  DSD firstobs=2;
 	informat id 5.                 
	gender $6. 
	age 3.
	ever_married $3.
	work_type $15.
	Residence_type $10.
	smoking_status $15.;
 	length smoking_status $ 15
 	work_type $ 10
 	Residence_type $ 10;
	format id 5.                 
	gender $6. 
	age 3.
	ever_married $3.
	work_type $15.
	Residence_type $10.
	smoking_status $15.;
	input id gender $ age ever_married $ work_type $ Residence_type $ smoking_status $;
	label id = "ID"
	gender = "Gender"
	age = "Age"
	ever_married = "Marriage Status"
	work_type = "Occupation"
	Residence_type = "Residence Type"
	Smoking_status = "Smoking Status";
RUN;

DATA Medical (label="Medical Data for Subjects");
 	infile "/home/u64043140/BIOSTAT 203A/Midterm Project/healthcare-dataset-stroke-medical-data.csv"  DSD firstobs=2;
	informat id 5.
	hypertension 1.                
	heart_disease 1. 
	avg_glucose_level 3.3
	bmi 4.2
	stroke 1.;
	format id 5.
	hypertension 1.                
	heart_disease 1. 
	avg_glucose_level 6.3
	bmi 6.3
	stroke 1.;
	input id hypertension heart_disease avg_glucose_level bmi stroke;
	label id = "ID"
	hypertension = "Has Hypertension"
	heart_disease = "Has Heart Disease"
	avg_glucose_level = "Average Glucose Level"
	bmi = "Body Max Index (BMI)"
	stroke = "Had Stroke";
RUN;

/*Merging the data*/
PROC SORT data = Demographic;
	by id;
RUN;

PROC SORT data = Medical;
	by id;
RUN;

/*Quality Control of the data where BMI != N/A*/

DATA MIDTERM.Demographic_Medical_QC;
set MIDTERM.Demographic_Medical;
where NOT MISSING(bmi);
RUN;

/*Formatting Age Groups*/
PROC FORMAT;
value age_groups 0-18 = 'Adolescent (0-18)'
             19-35  = 'Young Adult (19-35)'
             36-50 = 'Middle Adult (36-50)'
             51-65 = 'Older Adult (51-65)'
             65-high = 'Senior (> 65)';
RUN;

/*Formating Stroke Occurences*/
PROC FORMAT;
value stroke_no_yes 0 = 'No'
             1  = 'Yes';
RUN;

DATA MIDTERM.Demographic_Medical_QC_Formatted;
set MIDTERM.Demographic_Medical_QC;
format age age_groups.
stroke stroke_no_yes.;
RUN;


/*Tabulate to Visualize the Data Characteristics*/

/*Generating Table 1a and 1b to Describe Demographic Baseline Characteristics */
PROC MEANS data = MIDTERM.Demographic_Medical_QC_Formatted n mean std median min max nonobs maxdec = 3;
var bmi avg_glucose_level;
class age;
title height=14pt "Baseline Characteristics of Study Population";
run;

proc tabulate data=MIDTERM.Demographic_Medical_QC_Formatted;
   class age ;
   class heart_disease;
   class hypertension;
   class smoking_status;
  tables heart_disease hypertension smoking_status,
         (all='Age groups' age)
         ;
run;

/*Generating Figure 1 to Visualize Occurence of Stroke Amongst the Age Groups*/
PROC FREQ data = MIDTERM.Demographic_Medical_QC_Formatted;
tables age * stroke /nocol norow;
RUN;

ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=MIDTERM.DEMOGRAPHIC_MEDICAL_QC_FORMATTED;
	title height=14pt "Stroke Frequencies Amongst Age Groups";
	vbar stroke / group=age groupdisplay=cluster datalabel;
	yaxis grid;
run;

ods graphics / reset;
title;

/*Subsetting the Data by Age Group*/

DATA MIDTERM.Adolescent;
set MIDTERM.Demographic_Medical_QC_Formatted;
where age>=0 & age<=18;
RUN;

DATA MIDTERM.Young_Adult;
set MIDTERM.Demographic_Medical_QC_Formatted;
where age>=19 & age<=35;
RUN;

DATA MIDTERM.Middle_Adult;
set MIDTERM.Demographic_Medical_QC_Formatted;
where age>=36 & age<=50;
RUN;

DATA MIDTERM.Older_Adult;
set MIDTERM.Demographic_Medical_QC_Formatted;
where age>=50 & age<=65;
RUN;

DATA MIDTERM.Senior;
set MIDTERM.Demographic_Medical_QC_Formatted;
where age>65;
RUN;

/*Logistic Regression Models*/

/*Using Glucose levels and BMI to predict stroke in Older Adult Participants*/
/*ROC Curve will be Figure 2*/
proc logistic data=MIDTERM.Older_Adult plots=(roc);
	class hypertension heart_disease smoking_status (param=ref ref="never smoked") / param=glm;
	model stroke(event='Yes')=bmi avg_glucose_level hypertension heart_disease smoking_status/ link=logit CTABLE
		technique=fisher;
		output out =MIDTERM.Older_Adult_Stroke_Prob predicted =Older_Adult_Stroke_Predict;
run;

/*Formating 0 and 1 Predictions as No or Yes*/ 

PROC FORMAT;
value prediction_no_yes 0 = 'No'
             1  = 'Yes';
RUN;

/*Use Probability that Maximizes the sum of specificiy and sensitivity from CTABLE*/
data MIDTERM.Older_Adult_Stroke_LogRes;
set MIDTERM.Older_Adult_Stroke_Prob;
if Older_Adult_Stroke_Predict >= 0.060 then predicted = 1;
else predicted = 0;
format predicted prediction_no_yes.;
run;

/*Generating Confusion Matrix for Older Adults. This will be table 2*/
proc freq data=MIDTERM.Older_Adult_Stroke_LogRes;
    tables stroke*predicted / out = MIDTERM.Older_Adult_Stroke_CM;
    title "Confusion Matrix of Predicted Stroke Occurences Amongst Older Adults using a Logistic Regression Model";
run;

/*Printing Confusion Matrix*/
proc print data= MIDTERM.Older_Adult_Stroke_CM;
run;





/*Using Glucose levels and BMI to predict stroke in Senior Participants*/
/*ROC Curve will be Figure 3*/
proc logistic data=MIDTERM.Senior plots=(roc);
	class hypertension heart_disease smoking_status (param=ref ref="never smoked") / param=glm;
	model stroke(event='Yes')=bmi avg_glucose_level hypertension heart_disease smoking_status/ link=logit CTABLE
		technique=fisher;
		output out =MIDTERM.Senior_Stroke_Prob predicted =Senior_Stroke_Predict;
run;

/*Formating 0 and 1 Predictions as No or Yes*/ 

PROC FORMAT;
value prediction_no_yes 0 = 'No'
             1  = 'Yes';
RUN;

/*Use Probability that Maximizes the sum of specificiy and sensitivity from CTABLE*/
data MIDTERM.Senior_Stroke_LogRes;
set MIDTERM.Senior_Stroke_Prob;
if Senior_Stroke_Predict >= 0.160 then predicted = 1;
else predicted = 0;
format predicted prediction_no_yes.;
run;

/*Generating Confusion Matrix for Seniors. This will be table 3*/
proc freq data=MIDTERM.Senior_Stroke_LogRes;
    tables stroke*predicted / out = MIDTERM.Senior_Stroke_CM;
    title "Confusion Matrix of Predicted Stroke Occurences Amongst Seniors using a Logistic Regression Model";
run;

/*Printing Confusion Matrix*/
proc print data= MIDTERM.Senior_Stroke_CM;
run;
