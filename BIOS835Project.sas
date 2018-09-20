






 /*------------------------------------------------------------------------------------------------*/
		dm'output;clear;log;clear;';

		options  pageno=1  nodate  dsoptions=nomissopt;
 /*------------------------------------------------------------------------------------------------*/

		*** Concussions in Male and Female College Athletes ***;

		Title1 "Project Title: Concussions in Male and Female College Athletes";

		*** Define Variable Formats for Inputting the Data ***;
		proc format;
			  value setting  0 = "Practice"							1 = "Game";

			  value gender 	 0 = "Female"							1 = "Male";

			  value sport	 0 = "Soccer"							1 = "Lacrosse"		
							 2 = "Basketball" 						3 = "Softball/ Baseball"
							 4 = "Gymnastics";

			  value acyear 	 0 = "1997"								1 = "1998"
			  				 2 = "1999";

  			  value status 	 0 = "No"  								1 = "Yes"; 
		run;

 /*------------------------------------------------------------------------------------------------*/

		*** Inputing Data in the Original Form  ***;

		data concussion;
		 	infile 'C:\Users\muham\Google Drive\KUMC\Fall Semester 2017\BIOS 835 - Categorical Data Analysis\Project\SAS Code\concussion.txt'  dlm=' '  ;
  			input Setting Gender Sport Year Status n @@;
			format Setting setting. Gender gender. Sport sport. Year acyear. Status status.;
  			label   Setting = "Setting"
					 Gender = "Gender of Atheletes"
				      Sport = "Type of Sport"
				       Year = "Academic Year"
				     Status = "Concussion Status";							
		;

		Title2 "Data in the Original Form";
		proc print data = concussion label; run;						*Print Labelled Data*;

 /*------------------------------------------------------------------------------------------------*/

		*** Creating Data as Binomial Observations  ***;
		*** In the .txt file, each row (record) contains counts for yes and no concussion status for
			each combination of gender, sport, year. To create SAS data with binomial rows, we read 
			each row as a single records and then compute total number of atheletes for that 
			particular combination. Finally, we drop redundant variables ***;
 
		data concussion_binom;
			infile 'C:\Users\muham\Google Drive\KUMC\Fall Semester 2017\BIOS 835 - Categorical Data Analysis\Project\SAS Code\concussion.txt' dlm=' ';
  			input Setting Gender Sport Year x1 n x2-x6 x7 @@;
			format Setting setting. Gender gender. Sport sport. Year acyear.;
  			label Gender = "Gender of Atheletes"
				  Sport = "Type of Sport"
				  Year = "Academic Year"
				  n = "Number of Concussion Cases"
				  Total = "Total Number of Athletes";

			Total = n + x7;												*Compute total number of
																		 atheletes*;

			Rate = ( n / Total ) * 1000; 								*Compute Incidence / 1000
																		 game or practice cases*;	

			drop  x1 x2-x7;
		;

		Title2 "Data as binomial observations"; 
		proc print data = concussion_binom label; run;					*Print Labelled Data*;

 /*------------------------------------------------------------------------------------------------*/
 
		*** Creating our own K-1 dummy variables for each categorical predictor with
			K total categories***;
		*** In our case, 1 dummy variable for gender, 4 for Sports, 2 for Year***;

		data concussion_binom;
 			set concussion_binom;

			Soccer 	   = (Sport = 0);  			Lacrosse = (Sport = 1);  
			Basketball = (Sport = 2); 	      Gymnastics = (Sport = 4); 

			Year97 	   = (Year = 0);			  Year99 = (Year = 2); 
 		run;

		Title3 "(with dummy variables for multi-category predictors)";
		proc print data = concussion_binom label; run;					*Print Labelled Data again*;

 /*------------------------------------------------------------------------------------------------*/

		*** For our data, let us tabulate distribution of events/ non-events ***;
		*** In our project, we are going to use rule of thumb 20:1 ratio of min(events, non-events) 
			to number of candidate parameters ***;
		*** Sources: 
			http://www.citeulike.org/user/harrelfe/article/13467382
			https://stats.stackexchange.com/questions/26016/sample-size-for-logistic-regression***;
		
		Title2 "Distribution of events/ non-events";

		proc freq data = concussion order = data;
			table  Status;
			weight n;
		run;

 /*------------------------------------------------------------------------------------------------*/

		*** Let us make I-by-2 contingency tables with row variables being our potential predictor
			and column being the response variable Concussion Status***;

		Title2 "Contingency Table: Setting vs. Concussion Cases";
		proc freq data = concussion order = data;
			table  Setting*Status / nocol nopercent;
			weight n;
		run;

		Title2 "Contingency Table: Gender vs. Concussion Cases";
		proc freq data = concussion order = data;
			table  Gender*Status / nocol nopercent;
			weight n;
		run;


		Title2 "Contingency Table: Type of Sport vs. Concussion Cases";
		proc freq data = concussion order = data;
			table  Sport*Status / nocol nopercent;
			weight n;
		run;


		Title2 "Contingency Table: Academic Year vs. Concussion Cases";
		proc freq data = concussion order = data;
			table  Year*Status / nocol nopercent;
			weight n;
		run;
 /*------------------------------------------------------------------------------------------------*/
		
		*** Since vwe rarely observe any concussion cases for atheletes who do gymnastics, so for 
			our project we going to focus on the other four sports (i.e. Soccer, Lacrosse, 
			Basketball and Softball/ Baseball. Let us now modify our SAS data sets accordingly.***;

		data concussion;
			set concussion;
			if Sport = 4 then delete;
		run;
	
		proc print data = concussion label; run;

		data concussion_binom;
			set concussion_binom;
			if Sport = 4 then delete;
			drop gymnastics;
		run; 

		proc print data = concussion_binom label; run;

 /*------------------------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------------------------*/

		*** To get an idea about marginal associations between (potential) predictor variables in 
			our logistic model, let us perform chi-sqaure and LR tests for each variable ***;
		
		Title2 "Chi-square test of association between Gender and Concussion Status";
		proc freq data = concussion order = data;
			table  Gender * Status/ nocol norow nopercent expected relrisk chisq crosslist stdres;
			weight n;
		run;

		Title2 "Chi-square test of association between Game Setting and Concussion Status";
		proc freq data = concussion order = data;
			table  Setting * Status/ nocol norow nopercent expected relrisk chisq crosslist stdres;
			weight n;
		run;

		Title2 "Chi-square test of association between Sport Types and Concussion Status";
		proc freq data = concussion order = data;
			table  Sport * Status/ nocol norow nopercent expected chisq  crosslist stdres;
			weight n;
		run;

		Title2 "Chi-square test of association between Academic Year and Concussion Status";
		proc freq data = concussion order = data;
			table  Year * Status/ nocol norow nopercent expected chisq  crosslist stdres;
			weight n;
		run;

 /*------------------------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------------------------*/

		************************** NON-REGRESSION MODEL BASED ANALYSIS ****************************;

		*** Since our main aim is to asses association between gender and concussion status, we
			can perform CMH test that allows us to control for other variables ***;


		*** We will apply two CMH tests for each sport (controlling for academic year)
			to assess the following associations:
			 (i) Gender vs. Incidence of Concussion  
			(ii) Setting vs. Incidence of Concussion ***;
		
		*** Create new data which contains data for any given sport ***;
		*** To generate NewData for ***: 
			***   (i) Soccer:   			Use 	if Sport ~= 0 then delete; ***;
			***  (ii) Lacrosse: 			Use 	if Sport ~= 1 then delete; ***;
			*** (iii) Basketball: 			Use 	if Sport ~= 2 then delete; ***;
			***  (iv) Softball/ Baseball: 	Use 	if Sport ~= 3 then delete; ***;
	
 /*------------------------------------------------------------------------------------------------*/

		*** SOCCER ***;
 		data soccer;
			set concussion;
			if Sport ~= 0 then delete; 
		run;

		Title2 "Data for a soccer";
		proc print data  = soccer label; run;

		Title3 "CMH Test to test conditional association between Gender and Concussion Status for
				athletes who play soccer";
		/*check CMH test condition: (63 - 44.487)+ (56 -46.264) + (73 - 57.632) = 43.617 > 5*/
		/*also breslow p value 0.65 > 0.05 fail to reject homogeneous association*/
		proc freq data = soccer order = data;
		    table    Year *  Gender * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;
		
		/*check CMH test condition: (85 - 22.139) + (74-19.423) + (100 - 23.771) = 193.667 > 5*/
		/*also breslow p value 0.4240 > 0.05 fail to reject homogeneous association*/
		Title3 "CMH Test to test conditional association between Setting and Concussion Status for
				athletes who play soccer";
		proc freq data = soccer order = data;
		    table    Year *  Setting * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;

		*** Assess Marginal Associations across academic years, conditioned on soccer ***;

		Title3 "Marginal Association (across academic years) between Gender and Concussion 
				Status, conditioned on soccer";	
		proc freq data = soccer order = data;
			table   Gender * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;

		Title3 "Marginal Association (across academic years) between Setting and Concussion 
				Status, conditioned on soccer";	
		proc freq data = soccer order = data;
			table   Setting * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;
		
 /*------------------------------------------------------------------------------------------------*/

		*** LACROSSE ***;
		data Lacrosse;
			set concussion;
			if Sport ~= 1 then delete; 
		run;

		title2 "Data for Lacrosse";
		proc print data= Lacrosse; run;

		title3 "CMH Test to test conditional association between Gender and Concussion Status for
				athletes who play lacrosse";
		/*(26 - 17.885) + (10-10.655) + (12-15.014) = 4.446 < 5, but the potential 
			values are smaller than -5 if we condition on margins*/
		/*Breslow p value is 0.0653, fail to reject homogeneous association*/
		proc freq data = Lacrosse order = data;
		    table    Year *  Gender * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;

		title3 "CMH Test to test conditional association between Setting and Concussion Status for
				athletes who play lacrosse"
		/*(31 - 9.4503) + (22-4.644) + (24-6.7159) = 56.1898 > 5*/
		/*Breslow p value is 0.1268, fail to reject homogeneous association*/
		proc freq data = Lacrosse order = data;
		    table    Year *  Setting * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;

		Title3 "Marginal Association (across academic years) between Gender and Concussion 
				Status, conditioned on lacrosse";	
		proc freq data = Lacrosse order = data;
			table   Gender * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;

		Title3 "Marginal Association (across academic years) between Setting and Concussion 
				Status, conditioned on lacrosse";	
		proc freq data = Lacross order = data;
			table   Setting * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;	

 /*------------------------------------------------------------------------------------------------*/

		*** BASKETBALL ***;
		data basketball;
			set concussion;
			if Sport ~= 2 then delete;
		run;

		title2 "data for Basketball";
		proc print data= basketball; run;

		title3 "CMH Test to test conditional association between Gender and Concussion Status for
				athletes who play basketball";
		/*(40- 30.596) + (56-50.477) + (51-41.116) = 24.811*/
		/*Breslow p value is 0.4496, fail to reject homogeneous association*/
		proc freq data = basketball order = data;
		    table    Year *  Gender * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;

		title3 "CMH Test to test conditional association between Setting and Concussion Status for
				athletes who play basketball";
		/*(24-13.014) + (51-21.867) + (46-18.492) = 67.627 > 5*/
		/*Breslow p value is 0.2735, fail to reject homogeneous association*/
		proc freq data = basketball order = data;
		    table    Year *  Setting * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;

		Title3 "Marginal Association (across academic years) between Gender and Concussion 
				Status, conditioned on basketball";	
		proc freq data = basketball order = data;
			table   Gender * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;

		Title3 "Marginal Association (across academic years) between Setting and Concussion 
				Status, conditioned on basketball";	
		proc freq data = basketball order = data;
			table   Setting * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;

 /*------------------------------------------------------------------------------------------------*/

		*** SOFTBALL/ BASEBALL ***;
		data soft_base_ball;
			set concussion;
			if Sport ~= 3 then delete;
		run;

		title2 "data for softball/baseball";
		proc print data= soft_base_ball; run;

		title3 "CMH Test to test conditional association between Gender and Concussion Status for
				athletes who play softball/ basebbal";
		/*(16 - 13.678) + (17 - 12.814) + (45-32.416) = 19.092 > 5*/
		/*Breslow p value is 0.547, fail to reject homogeneous association*/
		proc freq data = soft_base_ball order = data;
		    table    Year *  Gender * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;

		title3 "CMH Test to test conditional association between Setting and Concussion Status for
				athletes who play softball/ basebbal";
		/*(31 - 14.828) + (16 - 9.9537) + (53-24.659) = 50.5593> 5*/
		/*Breslow p value is 0.2322, fail to reject homogeneous association*/
		proc freq data = soft_base_ball order = data;
		    table    Year *  Setting * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
		run;

		Title3 "Marginal Association (across academic years) between Gender and Concussion 
				Status, conditioned on softball/ basebbal";	
		proc freq data = soft_base_ball order = data;
			table   Gender * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;

		Title3 "Marginal Association (across academic years) between Setting and Concussion 
				Status, conditioned on softball/ basebbal";	
		proc freq data = soft_base_ball order = data;
			table   Setting * Status / nopercent norow nocol expected chisq relrisk;
			weight n;
		run;

 /*------------------------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------------------------*/




 /*------------------------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------------------------*/

		********************************* MODEL BASED ANALYSIS *********************************;

		*** Now we fit logistic regression model with concussion status (1=yes, 0=no) being the 
			response variable. ***;
		
		Title2 "Logistic Regression Model";	

		** Lets try differnt selection procedures **;

		Title3 "Forward Selection Procedure";
		proc logistic  data = concussion_binom; 		
		class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
		model n/Total = Gender Setting Sport Year / selection = forward sle = 0.05;
		*** Model: Concussion  = Gender + Sport + Setting ***;
		run; quit;

		Title3 "Backward Elimination Procedure";
		proc logistic  data = concussion_binom; 		
		class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
		model n/Total = Gender|Setting|Sport Year / selection = backward sls = 0.05;
		***Model: Concussion = Gender + Sport * Setting ***;
		run; quit;

		Title3 "Stepwise Elimination Procedure";
		proc logistic  data = concussion_binom; 		***Model: Gender + Sport * Setting ***;
		class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
		model n/Total = Gender|Setting|Sport Year / selection = stepwise sle = 0.05 sls = 0.05;
		***Model: Concussion = Gender + Sport * Setting ***;
		run; quit;

		*** Remark: Based on results of selection procedures, the hierarchical logistic model 
									Concussion = Gender + Setting * Sport
					looks the most appropriate model ***;
/*------------------------------------------------------------------------------------------------*/


		*** Let us manually fit various models to see deviance and AIC***;

		Title3 "Main Effects Model: Gender Setting Sport Year";
		proc genmod data = concussion_binom; 
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender Setting Sport Year;
			 *** Result: Dev = 152.6875, df = 40, Deviance P-value < 0.0001, AIC = 385.4810 ***; 
		run;

		Title3 "Two-way interaction Model: Gender*Setting Sport Year";
		proc genmod data = concussion_binom; 
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender|Setting Sport Year;
			 *** Result: Dev = 151.8411, df = 39, Deviance P-value < 0.0001, AIC = 386.6345 ***; 
		run;

		Title3 "Two-way interaction Model: Gender*Sport Setting Year";
		proc genmod data = concussion_binom;
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender|Sport Setting Year;
			 *** Result: Dev = 146.7646, df = 36, Deviance P-value < 0.0001, AIC = 385.5580 ***;
		run;

		Title3 "Two-way interaction Model: Gender*Year Sport Setting";
		proc genmod data = concussion_binom; 
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender|Year Sport Setting ;
			 *** Result: Dev = 151.3548, df = 38, Deviance P-value < 0.0001, AIC = 388.1482 ***;
		run;

		Title3 "Two-way interaction Model: Gender Sport*Setting Year";
		proc genmod data = concussion_binom;
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender  Setting|Sport Year;
			 *** Result: Dev = 54.7229, df = 37, Deviance P-value = 0.0303, AIC = 293.5164 ***; 
		run;

		Title3 "Two-way interaction Model: Gender Setting*Year Sport";
		proc genmod data = concussion_binom;
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender  Setting|Year Sport ;
			 *** Result: Dev = 151.0311, df = 38, Deviance P-value < 0.0001, AIC = 387.8245 ***;
		run;

		Title3 "Two-way interaction Model: Gender Setting Sport*Year";
		proc genmod data = concussion_binom;
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender  Setting Year|Sport ;
			 *** Result: Dev = 138.3009, df = 34, Deviance P-value < 0.0001, AIC = 383.0944 ***;
		run;


		Title3 "Three-way interaction Model: Gender*Setting*Sport Year";
		proc genmod data = concussion_binom; 
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender|Setting|Sport Year;
			 *** Result: Dev = 40.9519, df = 30, Deviance P-value = 0.0877, AIC = 293.7453 ***;
		run;

		Title3 "Three-way interaction Model: Gender*Setting*Year Sport";
		proc genmod data = concussion_binom; 
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender|Setting|Year Sport;
			 *** Result: Dev = 145.7673, df = 33, Deviance P-value < 0.0001, AIC = 392.5607 ***;
		run;

		Title3 "Three-way interaction Model: Gender*Sport*Year Setting";
		proc genmod data = concussion_binom;		
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender|Sport|Year Setting;
			 *** Result: Dev = 124.5111, df = 23, Deviance P-value < 0.0001, AIC = 391.3046 ***;
		run;

		Title3 "Three-way interaction Model: Gender Setting*Sport*Year";
		proc genmod data = concussion_binom;		 
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
			 model n/Total = Gender Setting|Sport|Year ;
			 *** Result: Dev = 28.9251, df = 23, Deviance P-value = 0.1828, AIC = 295.7186 ***;
		run;


		Title3 "Two-way interaction Model: Gender Sport*Setting";
		proc genmod data = concussion_binom;		 
			 class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				     / param = ref;
			 model n/Total = Gender  Setting|Sport;
			 *** Result: Dev = 56.3077, df = 39, Deviance P-value = 0.0359, AIC = 291.1011 ***;
		run;

		*** As per the AIC criterion, 
									Concussion = Gender + Sport*Setting
			seems the most reasonable model. By deviance test statistic, however, we have a marginal
			evidence to reject the null hypothesis of adequately model fit. Among all the above 
			fitted models, only Concussion =  Gender Setting|Sport|Year adequately fits the data.
			For sake of model parsimony, we will stick to our fitted model. ***;

/*------------------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------------------*/


		*** Since our model indicatess that there is an interaction between setting and sport and
			we didn't focus on controlling sport type in our non-regression modol based approach. 
			Let us also establish interaction by showing non-homogenous association between setting
		    and concussion across the levels of type of sport. This will be done using the
			Breslow-Day Test Statistic. For demonstration, we will do this for 1997 data only. ***;
		
		data NewData;
			set concussion;
			if year ~= 1 then delete;  
		run;

		Title2 "Data for academic year 1998 - 1999";
		proc print data  = NewData label; run;
 /*------------------------------------------------------------------------------------------------*/

		*** Assess Conditional Associations ***;

		proc freq data = NewData order = data;
		    table    Sport *  Setting * Status / nopercent norow nocol expected relrisk chisq cmh;
			weight n;
			*** Consistent with our logistic regression model results, P-value for Breslow-Day
				statistic was found to be less than 0.001. This shows an interaction between 
				game setting and type of sport ***;
		run;

 /*------------------------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------------------------*/
		
		*** For our logistic model, create effect plot and roc curve ***;

		ods latex path="C:\Users\muham\Google Drive\KUMC\Fall Semester 2017\BIOS 835 - Categorical Data Analysis\Project\Report";
		ods graphics on / imagefmt=png imagename='Fig';
		proc logistic data = concussion_binom descending;					*** ROC = 0.767 ***;
			 class Gender(ref = "Male") Setting(ref = "Practice")  Sport(ref = "Softball/ Baseball")  
					/ param = ref;
			 model n / Total = Setting Sport Setting*Sport Gender / CLPARM = wald outroc = concussion_roc ;
			effectplot interaction(X=Sport)/ yrange=(0, 0.002) clm connect;
		run;
		ods graphics off;
		ods latex close;

		ods latex path="C:\Users\muham\Google Drive\KUMC\Fall Semester 2017\BIOS 835 - Categorical Data Analysis\Project\Report";
		ods graphics on / imagefmt=png imagename='Fig';
		proc logistic data = concussion_binom descending;					*** ROC = 0.767 ***;
			 class Gender(ref = "Male") Setting(ref = "Practice")  Sport(ref = "Softball/ Baseball")  
					/ param = ref;
			 model n / Total = Sport Setting  Setting*Sport Gender  ;
			 exact Sport;
			 effectplot interaction(X=Setting)/ yrange=(0, 0.002) clm connect;
		run;
		ods graphics off;
		ods latex close;

 /*------------------------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------------------------*/


		*** In our chosen model, two interaction terms are significant. Let us perform selection
			procedures on our chosen model but by including dummy variables as possible candidates 
			in our model. This will allow us to see if these excluding insignificant terms still 
			yields a model with an adequate fit ***;
		
		Title3 "Backward Elimination Procedure";
		proc logistic  data = concussion_binom; 	 
		class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
		model n/Total = Gender Setting|Soccer|Lacrosse|Basketball   / selection = backward sls = 0.05;
		run;
		quit;

		Title3 "Stepwise Selection Procedure";
		proc logistic  data = concussion_binom; 		 
		class Gender(ref = "Male") Setting(ref = "Practice") Sport(ref = "Softball/ Baseball") 
				   Year (ref = "1998") / param = ref;
		model n/Total = Gender Setting|Soccer|Lacrosse|Basketball   /
						selection = stepwise sle = 0.05 sls = 0.05;
		run;
		quit;
	

		*** These two selection procedures indicate that 
					Concussion = Gender + Setting + Soccer + Lacrosse + Basketball +
								 Setting*Soccer + Setting*Lacrosse 
			is appropriate***;

		*** To compare deviances and AIC of both models ***;

		Title2 "Simplified version of our model";
		proc genmod data = concussion_binom;		
			 class Gender(ref = "Male") Setting(ref = "Practice")      / param = ref;
			 model n/Total = Gender   Setting   Soccer  Lacrosse   Basketball  
								 Setting*Soccer   Setting*Lacrosse / dist = binomial link = logit;
			*** Result: Dev = 56.5327, df = 40, Deviance P-value = 0.0432, AIC = 289.3261 ***; 
		run;

		proc logistic data = concussion_binom;
			 class Gender(ref = "Male") Setting(ref = "Practice")    / param = ref;
			 model n/Total = Gender   Setting   Soccer  Lacrosse   Basketball  
								 Setting*Soccer   Setting*Lacrosse / outroc = simple; * C = 0.579 ;
		run;

		*** This shows that simplified version is not that different from the model with all
			categories in it, but it has low prediction power as compared to our model.
			Depends on investigator which model he/ she prefers then. ***;

 /*------------------------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------------------------*/
