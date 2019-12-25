#########################################################################################################
#Goal:	Final submission for The DREAM-Phil Bowen ALS Prediction Prize4Life Challenge
#
#Authors:	Jules van Ligtenberg BSc
#		Rubio Vaughan BSc
#
#Date:	October 16th, 2012
#
#Copyright:	This source code is protected by copyright. Nothing from this source code may be used or
#		reproduced in any form without the written permission of its authors.
#		(authors can be contacted via JulesvanLigtenberg at sign OrcaXL dot nl)
#
#########################################################################################################

options(stringsAsFactors = FALSE) # This must be the first statement!!!
options(digits=12)

DEFAULT_AGE					=   54.88235294
DEFAULT_ALSFRS_R_TOTAL_SLOPE		=   -0.0214912985737     # from  206 instances
DEFAULT_ALSFRS_SLOPE_2			=   -0.0177077896516
DEFAULT_ALSFRS_TOTAL_SLOPE		=   -0.0168363589413     # from  709 instances
DEFAULT_ALSFRS_SECOND_DERIVATIVE	=   -8.60731450365e-05   # from  900 instances
DEFAULT_CLIMBING_STAIRS_SLOPE		=   -0.00359396958658    # from  915 instances
DEFAULT_CUTTING_WITHOUT_SLOPE		=   -0.00197819156783    # from  912 instances
DEFAULT_CUTTING_WITH_SLOPE		=    0.0078125           # from    4 instances
DEFAULT_DELTA_FIRST_ALSFRS_SCORE	=    3.174749
DEFAULT_DRESSING_SLOPE			=   -0.00302021281031    # from  915 instances
DEFAULT_DYSPNEA_SLOPE			=   -0.000580458293431   # from  206 instances
DEFAULT_FIRST_ALSFRS_SCORE		=   31.21572
DEFAULT_LAST_ALSFRS_SCORE		=   29.9770992366        # from  917 instances
DEFAULT_HANDWRITING_SLOPE		=   -0.00178707681463    # from  915 instances
DEFAULT_ONSET_DELTA			= -675.0927
DEFAULT_ORTHOPNEA_SLOPE			=   -0.000388064250533   # from  206 instances
DEFAULT_OTD					=  295.6359223           # Onset to Diagnosis
DEFAULT_RESP_INSUFF_SLOPE		=   -0.000396200139384   # from  206 instances
DEFAULT_RESPIRATORY_SLOPE		=   -0.000851025319924   # from  709 instances
DEFAULT_SALIVATION_SLOPE		=   -0.00111766503241    # from  915 instances!
DEFAULT_SLOPE				=   -0.675323514
DEFAULT_SOO					=    3.109022556         # Site of Onset
DEFAULT_SPEECH_SLOPE			=   -0.00156692130789    # from  915 instances
DEFAULT_SWALLOWING_SLOPE		=   -0.000832087688278   # from  915 instances
DEFAULT_TURNING_IN_BED_SLOPE		=   -0.00135763018862    # from  915 instances
DEFAULT_WALKING_SLOPE			=   -0.00169589770601    # from  915 instances
DEFAULT_WEIGHT				=   74.428057799
DEFAULT_WEIGHT_SLOPE			=   0.0105029045545	 # from 796 instances

# http://www.globalrph.com/conv_si.htm
# http://www.unc.edu/~rowlett/units/scales/clinical_data.html
LABTEST_CONVERSIONS <- matrix(c( 
	"Urine Ph",				"<nounit>",		0,
	"Glucose", 				"mmol/l", 		1,
	"Glucose", 				"g/l", 		5.55,
	"Glucose", 				"mg/dl", 		0.0555,
	"Total Cholesterol",		"mmol/l",		1,
	"Total Cholesterol",		"g/l",		2.59,
	"Total Cholesterol",		"mg/dl",		0.0259,
	"Bicarbonate",			"mmol/l",		1,
	"Bicarbonate",			"meq/l",		1,
	"Albumin",				"g/l",		1, 		# few cases of Unit being '%' are ignored
	"Sodium",				"mmol/l",		1,
	"Sodium",				"meq/l",		1,
	"Hemoglobin",			"mmol/l",		1,
	"Hemoglobin",			"g/l",		0.06206, 	# from discussionboard 1 g/l =  0.06206 mmol/l
	"Hemoglobin",			"mg/l",		0.00006206,
	"Red Blood Cells (RBC)",	"10e12/l",		1, 		# one instance of "per field" is ignored
	"Red Blood Cells (RBC)",	"10e9/l",		0.001, 
	"Red Blood Cells (RBC)",	"per mm3",		0.000001,
	"Red Blood Cells (RBC)",	"10e6/mm3",		1,
	"Calcium",				"mmol/l",		1,
	"Calcium",				"meq/l",		0.5,
	"Calcium",				"mg/dl",		0.25,
	"Calcium",				"mg/l",		0.025,
	"HbA1c (Glycated Hemoglobin)","%",			1,
	"HbA1c (Glycated Hemoglobin)","v/v",		100,
	"Monocytes",			"%",			1, 		# Only '%' is used: it is the most common, proper conversion to other units is unknown
	"Lymphocytes",			"%",			1, 		# Only '%' is used: it is the most common, proper conversion to other units is unknown
	"Hematocrit",			"%",			1,
	"Hematocrit",			"v/v",		100,
	"Hematocrit",			"1",			100,
	"Creatine Kinase",		"iu/l",		1,
	"Creatine Kinase",		"u/l",		1, 		# u/l seems to be the same as iu/l, mIU/L was ignored: 2 cases
	"Neutrophils",			"%",			1, 		# Only "%" is used: it is the most common, proper conversion to other units is unknown
	"Alkaline Phosphatase",		"iu/l",		1,
	"Alkaline Phosphatase",		"u/l",		1, 		# u/l seems to be the same as iu/l
	"Gamma-glutamyltransferase",	"iu/l",		1,
	"Gamma-glutamyltransferase",	"u/l",		1, 		# u/l seems to be the same as iu/l
	"ALT(SGPT)",			"iu/l",		1,
	"ALT(SGPT)",			"u/l",		1, 		# u/l seems to be the same as iu/l
	"Protein",				"g/l",		1,
	"Protein",				"g/dl",		10, 		# one instance of mEq/L ignored
	"White Blood Cells (WBC)",	"10e9/l",		1, 		# "g/L" ignored: no suitable conversion factor found
	"White Blood Cells (WBC)",	"per mm3",		0.001,
	"White Blood Cells (WBC)",	"10e3/mm3",		1,
	"Eosinophils",			"%",			1, 		# Only "%" is used: it is the most common, proper conversion to other units is unknown
	"AST(SGOT)",			"iu/l",		1,
	"AST(SGOT)",			"u/l",		1, 		# u/l seems to be the same as iu/l
	"Chloride", 			"mmol/l",		1,
	"Basophils", 			"%",			1, 		# Only "%" is used: it is the most common, proper conversion to other units is unknown
	"Phosphorus", 			"mmol/l",		1,
	"Phosphorus", 			"mg/dl",		0.323,
	"Phosphorus", 			"mg/l",		3.23,
	"Creatinine", 			"umol/l",		1,
	"Creatinine", 			"mg/dl",		88.4,
	"Creatinine", 			"mg/l",		884,
	"Triglycerides", 			"mmol/l",		1,
	"Triglycerides", 			"mg/dl",		0.0113,
	"Triglycerides", 			"g/l",		1.13,
	"Potassium", 			"mmol/l",		1,
	"Potassium", 			"meq/l",		1,
	"Bilirubin (total)",		"umol/l",		1,
	"Bilirubin (total)",		"mg/dl",		17.1,
	"Bilirubin (total)",		"mg/l",		171,
	# RBC Morphology skipped, seems useless, only value is "NORMAL"
	"Platelets",			"10e9/l",		1, 		# "g/L" ignored: no suitable conversion factor found
	"Platelets",			"per mm3",		0.001,
	"Platelets",			"10e3/mm3",		1,
	"Urea",				"mmol/l",		1, 		# one instance of umol/L ignored
	"Urea",				"mg/dl",		0.357, 
	"Urea",				"g/l",		35.7, 
	"Urine Protein", 			"<nounit>",		0,
	# Urine blood skipped, unknown values
	"Urine Glucose", 			"<nounit>",		0,
	"Blood Urea Nitrogen (BUN)",	"mmol/l",		1,
	# Beta HCG skipped too few numeric values
	"Prothrombin Time (clotting)","%",			1, 		#unable to find conversion from seconds to %	
	#"Urine WBCs" skipped
	#"Urine Casts" skipped see result check_test(test)
	"Free T3",				"nmol/l",		1,
	"Free T3",				"pmol/l",		0.001,
	"Amylase",				"u/l",		1,
	"Amylase",				"iu/l",		1,
	"Absolute Band Neutrophils",	"10e9/l",		1,
	"Urine Specific Gravity",	"<nounit>",		1,
	"Uric Acid", 			"umol/l", 		1,
	"Uric Acid", 			"mg/dl", 		59.48,
	"Lactate Dehydrogenase",	"u/l", 		1,
	"Lactate Dehydrogenase",	"iu/l", 		1,
	# Urine RBCs skipped too few numeric values
	"Thyroid Stimulating Hormone","miu/l",		1, 		# IU/L skipped, only one case found
	"Absolute Basophil Count",	"10e12/l",		1, 		# 10E9/L skipped, values are not 1000 x 10e12/l values!
	"Free T4",				"pmol/l",		1,
	"International Normalized Ratio (clotting)", "1", 1, 
	# Pancreatic Amylase skipped, no data before cutoff
	# Salivary Amylase skipped, no data before cutoff
	"Absolute Eosinophil Count",	"10e9/l",		1,
	"Absolute Monocyte Count",	"10e9/l",		1,
	"Absolute Segmented Neutrophils", "10e9/l",	1,
	"Absolute Total Neutrophils",	"10e9/l",		1,
	"Band Neutrophils",		"%",			1, 		# Only "%" is used: it is the most common, proper conversion to other units is unknown
	# Lithium skipped, only value: "<0.10"    ( 785  times)
	"Segmented Neutrophils",	"%",			1,
	# Urine Ketones skipped, too few numeric values
	# Urine Leukesterase skipped, too few numeric values
	"Absolute Lymphocyte Count",	"10e9/l",		1
	# C-Reactive Protein skipped, only 12 cases and non-numeric values
	# RBC Morphology: Macrocytosis, only 35 cases and non-numeric values
	# RBC Morphology: Anisocytosis, only 36 cases and non-numeric values
	# Hep A Antibody (IgG), only 6 cases, all with same value
	# Hep B Surface Antibody, only 6 cases, all with same value
	# Basophillic Stippling, only 1 case
	# Polychromasia, only 4 cases
	# Mean Corpuscular Volume, only 1 case before cutoff, 8 cases after
	# Activated Partial Thromboplastin Time, only 2 cases all after cutoff
	# Mean Corpuscular Hemoglobin, only 8 cases all after cutoff
	# Mean Corpuscular Hemoglobin Concentration, only 7 cases
	# Anion Gap, only 6 cases all after cutoff
	# RBC Distribution Width, only 5 cases, all after cutoff
	# Bilirubin (Direct), only 6 cases, all after cutoff
	# Bilirubin (Indirect), only 5 cases, all after cutoff
	# Creatine Kinase MM, only 2 cases, all after cutoff
	# Creatine Kinase MB, only 4 cases
	# Urine Bacteria, only 1 case
	# Immuno Electrophoresis, only 2 cases
	# Mean Platelet Volume, only 2 cases, all after cutoff
	# RBC Morphology: Spherocytes, only 1 case
	# Total T3, only 2 cases
	# RBC Morphology: Microcytosis, only 2 cases
	# GAMMA-GLOBULIN, only 2 cases
	# BETA-GLOBULIN, only 2 cases
	# CA 19-9, only 1 case
	# CO2, only 1 case
	# Erythrocyte Sediment skipped, not enough useful data
	# Globulin skipped, not enough useful data
	# Urine HCG skipped, not enough useful data
	# Antinuclear Antibody skipped, not enough useful data
	# Folate skipped, not enough useful data
	# Ferritin skipped, not enough useful data
	# Vitamin B12 skipped, not enough useful data
	# Arterial O2 Fraction skipped, not enough useful data
	# Saturated O2 Fraction skipped, not enough useful data
	# Arterial Ph skipped, not enough useful data
	# Free Thyroxine Index skipped, not enough useful data
	# LDL skipped, not enough useful data
	# T3 Uptake skipped, not enough useful data
	# HDL skipped, not enough useful data
	# Total T4 skipped, not enough useful data
	# Myoglobin skipped, not enough useful data
	# Urine Epithelial Cells skipped, not enough useful data
	# Urine Mucus skipped, not enough useful data
	# Urine Urobilinogen skipped, not enough useful data
	# Urine Hemoglobin skipped, not enough useful data
	# Urine Bilirubins skipped, not enough useful data
	# Urine Appearance skipped, not enough useful data
	# Urine Nitrite skipped, not enough useful data
	# Hep B Core Antibody skipped, not enough useful data
	# Hep B Surface Antigen skipped, not enough useful data
	# Urine Culture skipped, not enough useful data
	# Magnesium skipped, not enough useful data
	# Base Excess skipped, not enough useful data
	# Plasma Ph skipped, not enough useful data
	# Osmolality skipped, not enough useful data
	# Lipase skipped, not enough useful data
	# CO2 Fraction skipped, not enough useful data
	# Partial Thromboplastin Time skipped, not enough useful data
	# ALPHA1-GLOBULIN skipped, not enough useful data
	# ALPHA2-GLOBULIN skipped, not enough useful data
	# Urine Density skipped, not enough useful data
	# Iron skipped, not enough useful data
	# IMMUNOGLOBULIN M skipped, not enough useful data
	# Hep C Antibody skipped, not enough useful data
	# Urine Sodium skipped, not enough useful data
	# Total Cortisol skipped, not enough useful data
	# Urine Osmolality skipped, not enough useful data
	), ncol=3, byrow=TRUE )
LABTEST_CONVERSIONS <- data.frame(LABTEST_CONVERSIONS)
names(LABTEST_CONVERSIONS) <- c("Name", "Unit", "Factor")
LABTEST_CONVERSIONS$Unit <- tolower(LABTEST_CONVERSIONS$Unit) # make sure units are in lowercase

LABTEST_DEFAULTS <- matrix(c( 
    "Urine Protein",        "<nounit>",        0.0869304556355,    -0.000766024405615,    0,    2,    0.0971223021583,    0.0827338129496    # from 278, 118, 278, 278, 278 and 278 instances
 ), ncol=8, byrow=TRUE )
LABTEST_DEFAULTS <- data.frame(LABTEST_DEFAULTS)
names(LABTEST_DEFAULTS) <- c("Name", "Unit", "Mean", "Slope", "Min", "Max", "First", "Last")
LABTEST_DEFAULTS$Unit <- tolower(LABTEST_DEFAULTS$Unit) # make sure units are in lowercase

VITALSIGN_DEFAULTS <- matrix(c(
    "Blood Pressure (Systolic)",        130.022633123,    -0.00625610814798,    80,    220,    132.131226054,    128.20210728,    # from 1044, 1044, 1044, 1044, 1044 and 1044 instances
    "Pulse",        76.8812875888,    0.0115786722986,    18,    144,    74.5670498084,    80.2710727969,    # from 1044, 1044, 1044, 1044, 1044 and 1044 instances
    "Respiratory Rate",        17.5444649071,    0.00283061390403,    6,    40,    16.9324034335,    18.3787553648    # from 932, 932, 932, 932, 932 and 932 instances
	), ncol=7, byrow=TRUE )
VITALSIGN_DEFAULTS <- data.frame(VITALSIGN_DEFAULTS)
names(VITALSIGN_DEFAULTS) <- c("Name", "Mean", "Slope", "Min", "Max", "First", "Last")

FAMILY_HISTORY <- matrix(c(
	"Aunt",				1288,
	"Aunt (Maternal)",		1289,
	"Aunt (Paternal)",		1290,
	"Cousin",				1291,
	"Cousin (Paternal)",		1292,
	"Cousin (Maternal)",		1293,
	"Father",				1294,
	"Grandfather",			1295,
	"Grandfather (Maternal)",	1296,
	"Grandfather (Paternal)",	1297,
	"Grandmother",			1298,
	"Grandmother (Maternal)",	1299,
	"Grandmother (Paternal)",	1300,
	"Mother",				1301,
	"Nephew",				1302,
	"Niece",				1305,
	"Sibling",				1309,
	"Uncle",				1311,
	"Uncle (Maternal)",		1312,
	"Uncle (Paternal)",		1313,
	"Son",				1424,
	"Daughter",				1425,
	"Sister",				1426,
	"Brother",				1427
	), ncol=2, byrow=TRUE )
FAMILY_HISTORY <- data.frame(FAMILY_HISTORY)
names(FAMILY_HISTORY) <- c("Name", "Field_ID")

### Read training data from current directory
testset <- read.table('test.txt', header = TRUE, stringsAsFactors=FALSE ,sep='|', nrows=-1,  quote="")
names(testset) <- c('Subject_ID', 'Form_ID', 'Form_Name', 'Key' , 'Field_ID', 'Field_Name', 'Value')
dataset <- testset
SubjectIDs <- unique(dataset[,"Subject_ID"])


################################
#Functions to submit
################################

################################
# convert_units
################################
convert_units <- function(pDF, pTestName, pDebug){
	if(missing(pDebug)){pDebug <- FALSE}

	if(nrow(pDF) == 0){
		return(pDF)
	}
	
	#make sure the return dataframe has always the correct number of columns
	df <- data.frame(pDF[1,],stringsAsFactors=FALSE)
	df <- df[-1,]

	if(pTestName == "Weight"){
		for(i in 1:nrow(pDF)) {
			value <- pDF[i, "Result"]
			unit <- pDF[i, "Unit"]
			if(tolower(unit) == "pounds") {
				# convert to kilograms
				pDF[i,"Result"] <- as.numeric(as.double(value) * 0.45359233) # http://www.metric-conversions.org
			} else if(tolower(unit) != "kilograms" & unit != 2) {	# assuming 2 to mean Kilograms 
				# Unknown unit: ignore this row
				next
			}
			df <- rbind(df, pDF[i,])
		}
		names(df) <- names(pDF)
		df <- df[,names(df) != "Unit"] # Strip Unit column from the df
		return(df)
	}
	if(pTestName == "Height"){
		for(i in 1:nrow(pDF)) {
			value <- pDF[i, "Result"]
			unit <- pDF[i, "Unit"]
			if(tolower(unit) == "inches") {
				# convert to centimeters
				pDF[i,"Result"] <- as.numeric(as.double(value) * 2.54) # http://www.metric-conversions.org
			} else if(tolower(unit) != "centimeters" & tolower(unit) != "cm") {
				# Unknown unit: ignore this row
				next
			}
			df <- rbind(df, pDF[i,])
		}
		names(df) <- names(pDF)
		df <- df[,names(df) != "Unit"] # Strip Unit column from the df
		return(df)
	}

	if(!(pTestName %in% LABTEST_CONVERSIONS$Name)) {
		# Unknown test
		return(data.frame())
	}

	for(i in 1:nrow(pDF)) {
		if(pDebug) {
			print(pDF[i,])
			flush.console()
		}
		value <- as.numeric(pDF[i, "Result"])
		if(is.na(value)) {
			# Faulty data: ignore this row
			next
		}
		unit <- tolower(pDF[i, "Unit"])
		df_conv <- subset(LABTEST_CONVERSIONS, Name == pTestName & Unit == unit)
		if(pDebug) {
			cat("Unit: ", unit, ", Value: ", value)
			flush.console()
		}
		if(nrow(df_conv) == 0) {
			# Unknown unit: ignore this row
			if(pDebug) {
				cat(" - IGNORED\n")
				flush.console()
			}
			next
		} else {
			if(pDebug) {
				cat(" - CONVERTED\n")
				flush.console()
			}
			conv_factor <- as.numeric(df_conv[1,"Factor"])
			pDF[i,"Result"] <- as.numeric(conv_factor * value)
		}
		df <- rbind(df, pDF[i,])
	}

	names(df) <- names(pDF)
	df <- df[,names(df) != "Unit"] # Strip Unit column from the df
	df
}



################################
# generate_nn_table
################################
#pDefaults BOOLEAN when true a default is supplied for every missing value when false NA is enetered for a missing value (only for a selected number of variables)
generate_nn_table <- function(pDefaults, pDebug){
	if(missing(pDefaults)){pDefaults <- TRUE}
	if(missing(pDebug)){pDebug <- FALSE}
	Train_Table <- data.frame(stringsAsFactors=FALSE)
	i=0
	for(Subject in SubjectIDs){
		colnames <- vector()
		row <- vector()

		df_subject_data <- subset(dataset, Subject_ID == Subject)
		i = i + 1
		if(pDebug){
			cat( "\n", i, " - ", Subject)
			flush.console()
		}

		row <- c(row, Subject)
		colnames <- c(colnames, "Subject_ID")

		#####################################
		##### Demographics #####
		#####################################

		##### ALSFRS Slope (linear regression) #####
		#lineaire regressie: predict_slopes_lr met linear_regression(Subject, 43.0)
		row <- c(row, ((as.double(subset(ALSFRS_slopes, Subject_ID == Subject)[,"Slope"]) * 365.24 / 12) + DEFAULT_SLOPE) / 1.95)
		colnames <- c(colnames, "ALSFRS_Slope")

		##### First ALSFRS Score, Last ALSFRS Score, mean ALSFRS Score #####
		scores <- get_ALSFRS_scores(Subject)
		cutoff_point <- 91
		scores <- subset(scores, as.integer(ALSFRS_Delta) <= cutoff_point)
		if(nrow(scores) > 0) {
			First_ALSFRS_Score <- scores[1, "ALSFRS_Score"]
			Last_ALSFRS_Score <- scores[nrow(scores), "ALSFRS_Score"]
			Mean_ALSFRS_Score <- mean(scores[, "ALSFRS_Score"])
		} else {
			if(pDefaults){
				First_ALSFRS_Score <- DEFAULT_FIRST_ALSFRS_SCORE
				Last_ALSFRS_Score  <- DEFAULT_LAST_ALSFRS_SCORE
				Mean_ALSFRS_Score  <- DEFAULT_FIRST_ALSFRS_SCORE
			}else{
				First_ALSFRS_Score <- NA
				Last_ALSFRS_Score  <- NA
				Mean_ALSFRS_Score  <- NA
			}
		}
		row <- c(row, First_ALSFRS_Score, Last_ALSFRS_Score, Mean_ALSFRS_Score)
		colnames <- c(colnames, "First_ALSFRS_Score", "Last_ALSFRS_Score", "Mean_ALSFRS_Score")

		##### First ALSFRS Score sq, Last ALSFRS Score sq, avg ALSFRS Score sq #####
		First_ALSFRS_Score_sq <- First_ALSFRS_Score^2
		Last_ALSFRS_Score_sq <- Last_ALSFRS_Score^2
		Mean_ALSFRS_Score_sq <- Mean_ALSFRS_Score^2
		row <- c(row, First_ALSFRS_Score_sq, Last_ALSFRS_Score_sq, Mean_ALSFRS_Score_sq)
		colnames <- c(colnames, "First_ALSFRS_Score_sq", "Last_ALSFRS_Score_sq", "Mean_ALSFRS_Score_sq")

		#####################################
		##### Laboratory Data #####
		#####################################
		labtests <- unique(LABTEST_DEFAULTS$Name)
		for(testname in labtests) {
			if(pDefaults){
				test_mean  <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Mean"][1]
				test_slope <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Slope"][1]
				test_first <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "First"][1]
				test_last  <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Last"][1]
				test_min   <- -999999
				test_max   <- -999999
			}else{
				test_mean  <- NA
				test_slope <- NA
				test_first <- NA
				test_last  <- NA
				test_min   <- NA
				test_max   <- NA
			}
			df <- subset(Labtest_data, Subject_ID == Subject & Testname == testname)
			if(nrow(df) > 0) {
				test_mean <- df[1,"Mean"]
				test_slope <- df[1,"Slope"]
				test_first <- df[1,"First"]
				test_last <- df[1,"Last"]
			}
			testname <- gsub("[ -]", "_", testname)
			testname <- gsub("[\\(\\)]", "", testname)
			row <- c(row, test_mean, test_slope, test_first, test_last)
			colnames <- c(colnames, paste(testname, "Mean", sep="_"), paste(testname, "Slope", sep="_"), paste(testname, "First", sep="_"), paste(testname, "Last", sep="_"))
		}

		#####################################
		##### Vital Signs #####
		#####################################
		for(vitalname in VITALSIGN_DEFAULTS$Name) {
			if(pDefaults){
				mean	<- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == vitalname, "Mean"][1]
				slope <- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == vitalname, "Slope"][1]
				first <- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == vitalname, "First"][1]
				last	<- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == vitalname, "Last"][1]
				min   <- -999999
				max   <- -999999
			}else{
				mean	<- NA
				slope	<- NA
				first	<- NA
				last	<- NA
				min	<- NA
				max	<- NA
			}
			df <- subset(Vitalsign_data, Subject_ID == Subject & Name == vitalname)
			if(nrow(df) > 0) {
				mean <- df[1,"Mean"]
				slope <- df[1,"Slope"]
				first <- df[1,"First"]
				last <- df[1,"Last"]
			}
			vitalname <- gsub("[ -]", "_", vitalname)
			vitalname <- gsub("[\\(\\)]", "", vitalname)
			row <- c(row, mean, slope, first, last)
			colnames <- c(colnames, paste(vitalname, "Mean", sep="_"), paste(vitalname, "Slope", sep="_"), paste(vitalname, "First", sep="_"), paste(vitalname, "Last", sep="_"))
		}

		#####################################
		##### Special Attributes #####
		#####################################

		##### Onset Delta #####
		Onset_Delta <- as.numeric(DEFAULT_ONSET_DELTA)
		df <- subset(df_subject_data, Field_ID==1417)
		if(nrow(df)>0){
			Onset_Delta <- mean(as.numeric(df[,"Value"]))
		}
		row <- c(row, Onset_Delta)
		colnames <- c(colnames, "Onset_Delta")

		##### Onset Delta sq #####
		row <- c(row, Onset_Delta^2)
		colnames <- c(colnames, "Onset_Delta_sq")

			
		##### Add row to table #####
		Train_Table <- rbind(Train_Table, row)
	}

	names(Train_Table) <- colnames
	Train_Table
}

get_ALSFRS_scores <- function(pSubject_ID){
	if(missing(pSubject_ID)){
		if(exists("Table_ALSFRS_Score")){
			remove(Table_ALSFRS_Score, envir=globalenv())
		}
		Subset <- unique(subset(dataset, Form_ID==145 & Value!="")) #subset to be used in the rest of the function
	} else {
		if(exists("Table_ALSFRS_Score")){
			cache <- subset(Table_ALSFRS_Score, Subject_ID == pSubject_ID)
			if(nrow(cache) > 0) {
				cache$Subject_ID <- NULL
				return(cache)
			}
		}
		Subset <- unique(subset(dataset, Subject_ID == pSubject_ID & Form_ID==145 & Value!="")) #subset to be used in the rest of the function
	}
	Keys <- unique(subset(Subset, Field_ID==1225)[,4])
	if(length(Keys)!=0){
		scores_df <- data.frame(stringsAsFactors=FALSE)
		for(TheKey in Keys){
			Scores <- subset(Subset, Key==TheKey)
			Subject <- as.integer(Scores[1,1])
			Delta <- as.integer(subset(Scores, Field_ID==1225)[,7])
			# Only use 5a if both 5a and 5b are present
			if(length(subset(Scores, Field_ID==1218 & Value!="")[,1]) > 0 & length(subset(Scores, Field_ID==1219 & Value!="")[,1]) > 0){
				Scores <- subset(Scores, Field_ID!=1219)
			}
			Sum_subset <- subset(Scores, Field_ID!=1225 & Field_ID!=1228 & Field_ID!=1229 & Field_ID!=1231 & Field_ID!=1232)
			if(nrow(Sum_subset) == 10) {
				Score <- sum(as.integer(Sum_subset[,7]))
				scores_df <- rbind(scores_df, c(Subject, Delta, Score))
			}
		}
		scores_df <- data.frame(scores_df[order(scores_df[,1],scores_df[,2]),], row.names=seq_along(scores_df[,1]), stringsAsFactors=FALSE) #row 'numbers' aanharken
		names(scores_df) <- c("Subject_ID", "ALSFRS_Delta", "ALSFRS_Score")
		scores_df <- transform(scores_df, Subject_ID = as.integer(Subject_ID), ALSFRS_Delta = as.integer(ALSFRS_Delta), ALSFRS_Score = as.integer(ALSFRS_Score))
		if(!exists("Table_ALSFRS_Score")){
			Table_ALSFRS_Score <<- data.frame(stringsAsFactors=FALSE)
		}
		Table_ALSFRS_Score <<- rbind(Table_ALSFRS_Score, scores_df) # save result
		if(!missing(pSubject_ID)){
			scores_df$Subject_ID <- NULL # remove Subject_ID column from result if called with a specific Subject_ID
		}
	}
}

################################
#get_values
################################
#preconditions: see get_values + pFilter must make sense (is applied on both fields)
get_values <- function(pSubject_ID, pField_Name1, pField_Name2, pFilter, pSourceTable) {
	if(missing(pSourceTable)) {
		pSourceTable <- dataset
	}
	if(missing(pFilter)) {
		subset_tmp <- subset(pSourceTable, Subject_ID == pSubject_ID & (Field_Name == pField_Name1 | Field_Name == pField_Name2 & Value != ""))
	} else {
		subset_tmp <- subset(pSourceTable, Subject_ID == pSubject_ID & (Field_Name == pField_Name1 | Field_Name == pField_Name2) & Value != pFilter & Value != "")
	}
	Field1_Keys <- subset(subset_tmp, Field_Name == pField_Name1)[,"Key"]
	Field2_Keys <- subset(subset_tmp, Field_Name == pField_Name2)[,"Key"]
	subset_tmp <- subset(subset_tmp, Key %in% Field1_Keys & Key %in% Field2_Keys)
	subset_tmp <- subset_tmp[4:7]
	subset_tmp <- subset_tmp[order(subset_tmp[,1]),]
	unorderd_pairs_tmp <- data.frame(c(subset(subset_tmp, Field_Name==pField_Name1)[,4]), c(subset(subset_tmp, Field_Name==pField_Name2)[,4]), stringsAsFactors=FALSE)
	names(unorderd_pairs_tmp) <- c(pField_Name1, pField_Name2)
	unorderd_pairs_tmp[order(unorderd_pairs_tmp[,1]),]
}

get_values_2 <- function(pSubject_ID, pGroup_Field, pSelect_Field, pResult_Fields, pSourceTable, pSelect_Criteria) {
	if(missing(pSourceTable)) {
		pSourceTable <- dataset
	}
	if(missing(pSelect_Criteria)) {
		subset_tmp <- subset(pSourceTable, Subject_ID == pSubject_ID & 
			(Field_Name == pGroup_Field | Field_Name == pSelect_Field | Field_Name %in% pResult_Fields) & 
			!is.na(Value) & Value != "")
	} else {
		subset_tmp <- subset(pSourceTable, Subject_ID == pSubject_ID & 
			(Field_Name == pGroup_Field | (Field_Name == pSelect_Field & Value == pSelect_Criteria) | Field_Name %in% pResult_Fields) & 
			!is.na(Value) & Value != "")
	}
	Group_Keys <- subset(subset_tmp, Field_Name == pGroup_Field)[,"Key"]
	Select_Keys <- subset(subset_tmp, Field_Name == pSelect_Field)[,"Key"]
	subset_tmp <- subset(subset_tmp, Key %in% Group_Keys & Key %in% Select_Keys)
	for(result_field in pResult_Fields){
		result_field_keys <- subset(subset_tmp, Field_Name == result_field)[,"Key"]
		subset_tmp <- subset(subset_tmp, Key %in% result_field_keys)
	}
	subset_tmp <- subset_tmp[4:7]
	subset_tmp <- subset_tmp[order(subset_tmp[,1]),]
	unorderd_pairs_tmp <- data.frame(c(subset(subset_tmp, Field_Name==pGroup_Field)[,4]), c(subset(subset_tmp, Field_Name==pSelect_Field)[,4]), stringsAsFactors=FALSE)
	names(unorderd_pairs_tmp) <- c(pGroup_Field, pSelect_Field) #early naming for debugging purposes
	for(result_field in pResult_Fields){
		unorderd_pairs_tmp <- cbind(unorderd_pairs_tmp, subset(subset_tmp, Field_Name==result_field)[,4])
	}
	names(unorderd_pairs_tmp) <- c(pGroup_Field, pSelect_Field, pResult_Fields)
	subset <- unorderd_pairs_tmp[order(unorderd_pairs_tmp[,1]),]
	subset
}

#pDefaults BOOLEAN when true a default is supplied for every missing value when false NA is supplies for a missing value
linear_regression <- function(pSubject, pOnset_Score, pDefaults){
	if(missing(pDefaults)){pDefaults <- TRUE}
	scores <- get_ALSFRS_scores(pSubject)
	cutoff_point <- 91
	scores <- subset(scores, as.integer(ALSFRS_Delta) <= cutoff_point)
	if(nrow(scores) < 1) {
		if(pDefaults){
			scores <- rbind(c(DEFAULT_DELTA_FIRST_ALSFRS_SCORE, DEFAULT_FIRST_ALSFRS_SCORE), scores)
		}else{
			scores <- rbind(c(NA, NA), scores)
		}
		names(scores) <- c("ALSFRS_Delta", "ALSFRS_Score")	#namen gaan verloren als aantal rijen nul is geweest
	}
	Onset_Delta_Subset <- subset(dataset, Subject_ID==pSubject & Field_ID==1417) # Field_ID==1417 is onset delta
	Onset_Delta <- min(as.integer(Onset_Delta_Subset[,7]))
	scores <- rbind(c(Onset_Delta, pOnset_Score), scores)
	scores <- data.frame(scores, stringsAsFactors=FALSE)
	res <- lm(scores[,"ALSFRS_Score"] ~ scores[,"ALSFRS_Delta"])
	as.double(coefficients(res)[2])
}

linear_regression_2 <- function(pSubject, pDelta, pValue, pDefault){ 
	cutoff_point <- 91
	df <- get_values(pSubject, pDelta, pValue)
	df <- data.frame(as.numeric(df[,pDelta]),as.numeric(df[,pValue]))
	names(df) <- c("Delta", "Value")
	df <- subset(df, as.integer(Delta) <= cutoff_point)
	df <- df[order(as.numeric(df[,"Delta"])),]
	if(nrow(df) < 2) {
		return(pDefault)
	}
	res <- lm(df[,"Value"] ~ df[,"Delta"])
	as.double(coefficients(res)[2])
}

#special lr for ALSFRS_score
linear_regression_3 <- function(pSubject, pDefault, pDefaults){ 
	cutoff_point <- 91
	scores <- get_ALSFRS_scores(pSubject)
	cutoff_point <- 91
	scores <- subset(scores, as.integer(ALSFRS_Delta) <= cutoff_point)
	if(nrow(scores) < 2) {
		if(pDefaults){
			return(pDefault)
		}else{
			return(NA)
		}
	}
	res <- lm(scores[,"ALSFRS_Score"] ~ scores[,"ALSFRS_Delta"])
	as.double(coefficients(res)[2])
}

predict_earth <- function (pModel, pTable) {
	for(i in 1:ncol(pTable)) {
		pTable[,i] <- as.numeric(pTable[,i])
	}

	if(is.character(pModel)) {
		slopes <- eval(parse(text=paste(
			"with(as.data.frame(pTable),\n",
			pModel,
			")\n", sep = "")))
	} else {
		slopes <- predict(object = pModel, newdata = pTable)
	}

	slopes <- data.frame(pTable$Subject_ID, slopes)
	names(slopes) <- c("Subject_ID", "Slope")
	slopes
}

#pDefaults BOOLEAN when true a default is supplied for every missing value when false NA is enetered for a missing value (for only a selected number of variables)
predict_slopes_lr <- function(pDefaults){
	if(missing(pDefaults)){pDefaults <- TRUE}
	slopes <- data.frame()
	for(Subject in SubjectIDs){
		slope <- linear_regression(Subject, 43.0, pDefaults) 
		df <- subset(get_ALSFRS_scores(Subject), as.numeric(ALSFRS_Delta) <= 91)
		slopes <- rbind(slopes, c(Subject, slope))
	}
	names(slopes) <- c("Subject_ID", "Slope")
	slopes
}

#pDefaults BOOLEAN when true a default is supplied for every missing value when false NA is enetered for a missing value (for only a selected number of variables)
process_labtests <- function(pDefaults, pDebug){
	if(missing(pDefaults)){pDefaults <- TRUE}
	if(missing(pDebug)) { pDebug <- FALSE }
	cutoff_point <- 91
	labtests <- data.frame(stringsAsFactors=FALSE)
	testnames <- LABTEST_DEFAULTS$Name # unique(subset(dataset, Field_ID == 1250)[,"Value"])
	i=0
	for(Subject in SubjectIDs){
		i=i+1
		if(pDebug) { 
			#if(i>20) {break}
			cat("\n\n", i, " - Processing labtests for subject ", Subject, ":\n") 
			cat("\nLabtests\n") 
			flush.console()
		}
		j = 0
		df_subject_data <- subset(dataset, Subject_ID == Subject) 
		for(testname in testnames) {
			j = j + 1
			if(!(testname %in% LABTEST_CONVERSIONS$Name)) {
				next
			}
			if(pDebug) { 
				if(j%%5==0) {
					cat("\n")
				}
				cat("\t", testname) 
				flush.console()
			}
			if(testname %in% LABTEST_CONVERSIONS[LABTEST_CONVERSIONS$Unit == "<nounit>","Name"]) {
				df <- get_values_2(Subject, "Laboratory Delta", "Test Name", "Test Result", df_subject_data, testname)
				names(df) <- c("Delta", "Name", "Result")
				df <- subset(df, as.integer(Delta) <= cutoff_point)
				options(warn=-1) # temporarily ignore warnings from as.numeric
				df$Result <- as.numeric(df$Result)
				options(warn=0) # restore warnings
				df <- subset(df, ! is.na(Result)) # remove all results which couldn't be converted to numeric
			} else {
				df <- get_values_2(Subject, "Laboratory Delta", "Test Name", c("Test Result", "Test Unit"), df_subject_data, testname)
				names(df) <- c("Delta", "Name", "Result", "Unit")
				df <- subset(df, as.integer(Delta) <= cutoff_point)
				options(warn=-1) # temporarily ignore warnings from as.numeric
				df$Result <- as.numeric(df$Result)
				options(warn=0) # restore warnings
				df <- subset(df, ! is.na(Result)) # remove all results which couldn't be converted to numeric
				df <- convert_units(df, testname)
			}
				
			df <- subset(df, !duplicated(Delta)) # remove duplicate results (with identical delta)
			df$Delta <- as.numeric(df$Delta)
			df$Result <- as.numeric(df$Result) ##toch ook maar result weer eens numeriek gemaakt
			df <- df[order(df[,"Delta"]),]
			if(pDefaults){
				mean  <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Mean"][1]
				slope <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Slope"][1]
				first <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "First"][1]
				last  <- LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Last"][1]
			}else{
				mean  <- NA
				slope <- NA
				first <- NA
				last  <- NA
			}
			range_min <- as.numeric(LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Min"][1]) #don't try NULL !
			range_max <- as.numeric(LABTEST_DEFAULTS[LABTEST_DEFAULTS$Name == testname, "Max"][1]) #je kan simpelweg niet genoeg getallen weer numeriek maken
			range <- range_max - range_min
			range_min <- range_min - (1.2 * range) # 20% deviation allowed
			range_max <- range_max + (1.2 * range) # 20% deviation allowed
			if (pDebug){
				df_not_realistic <- subset(df, as.numeric(Result) < range_min | as.numeric(Result) > range_max)
				if(nrow(df_not_realistic)>0){
					cat("The following values where considered not to be realistic and are left out:\n")
					print(df_not_realistic)
				}
			}
			df <- subset(df, as.numeric(Result) >= range_min & as.numeric(Result) <= range_max)
			min <- -999999
			max <- -999999
			if(nrow(df) >= 2) {
				res <- lm(df[,"Result"] ~ df[,"Delta"])
				slope <- as.double(coefficients(res)[2])
			}
			if (nrow(df) > 0) {
				mean <- mean(df[,"Result"])
				min  <- min(df[,"Result"])
				max  <- max(df[,"Result"])
				first <- df[1, "Result"]
				last <- df[nrow(df), "Result"]
			}
			labtests <- rbind(labtests, c(Subject, testname, as.numeric(mean), as.numeric(slope), as.numeric(min), as.numeric(max), as.numeric(first), as.numeric(last)))
		}
	}
	names(labtests) <- c("Subject_ID", "Testname", "Mean", "Slope", "Min", "Max", "First", "Last")
	labtests
}

#pDefaults BOOLEAN when true a default is supplied for every missing value when false NA is enetered for a missing value (for only a selected number of variables)
process_vitalsigns <- function(pDefaults, pDebug){
	if(missing(pDefaults)){pDefaults <- TRUE}
	if(missing(pDebug)) { pDebug <- FALSE }
	cutoff_point <- 91
	vitalsigns <- data.frame(stringsAsFactors=FALSE)
	i=0
	for(Subject in SubjectIDs){
		i=i+1
		if(pDebug) { 
			#if(i>20) {break}
			cat("\n\n", i, " - Processing vital signs for subject ", Subject, ":\n") 
			cat("\nVital Signs\n") 
			flush.console()
		}
		j = 0
		df_subject_data <- subset(dataset, Subject_ID == Subject) 
		for(name in VITALSIGN_DEFAULTS$Name) {
			j = j + 1
			if(pDebug) { 
				if(j%%5==0) {
					cat("\n")
				}
				cat("\t", name) 
				flush.console()
			}
			if(name == "Weight" | name == "Height") {
				if(name == "Weight") {
					df <- get_values_2(Subject, "Vital Signs Delta", name, "Weight Units", df_subject_data)
				} else if(name == "Height") {
					df <- get_values_2(Subject, "Vital Signs Delta", name, "Height Units", df_subject_data)
				}
				names(df) <- c("Delta", "Result", "Unit")
				df <- subset(df, as.integer(Delta) <= cutoff_point)
				options(warn=-1) # temporarily ignore warnings from as.numeric
				df$Result <- as.numeric(df$Result)
				options(warn=0) # restore warnings
				df <- subset(df, ! is.na(Result)) # remove all results which couldn't be converted to numeric
				df <- convert_units(df, name)
			} else {
				df <- get_values_2(Subject, "Vital Signs Delta", name, c(), df_subject_data)
				names(df) <- c("Delta", "Result")
			}
			df <- subset(df, !duplicated(Delta)) # remove duplicate results (with identical delta)
			df$Delta <- as.numeric(df$Delta)
			df$Result <- as.numeric(df$Result) ##toch ook maar result weer eens numeriek gemaakt
			df <- df[order(df[,"Delta"]),]
			if(pDefaults){
				mean  <- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == name, "Mean"][1]
				slope <- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == name, "Slope"][1]
				first <- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == name, "First"][1]
				last  <- VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == name, "Last"][1]
			}else{
				mean  <- NA
				slope <- NA
				first <- NA
				last  <- NA
			}
			range_min <- as.numeric(VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == name, "Min"][1]) #don't try NULL !
			range_max <- as.numeric(VITALSIGN_DEFAULTS[VITALSIGN_DEFAULTS$Name == name, "Max"][1])
			range <- range_max - range_min
			range_min <- range_min - (1.2 * range) # 20% deviation allowed
			range_max <- range_max + (1.2 * range) # 20% deviation allowed
			if (pDebug){
				df_not_realistic <- subset(df, as.numeric(Result) < range_min | as.numeric(Result) > range_max)
				if(nrow(df_not_realistic)>0){
					cat("The following values where considered not to be realistic and are left out (", range_min, ") (", range_max, "):\n")
					print(df_not_realistic)
				}
			}
			df <- subset(df, as.numeric(Result) >= range_min & as.numeric(Result) <= range_max)
			min <- -999999
			max <- -999999
			if(nrow(df) >= 2) {
				res <- lm(df[,"Result"] ~ df[,"Delta"])
				slope <- as.double(coefficients(res)[2])
			}
			if (nrow(df) > 0) {
				mean <- mean(df[,"Result"])
				min  <- min(df[,"Result"])
				max  <- max(df[,"Result"])
				first <- df[1, "Result"]
				last <- df[nrow(df), "Result"]
			}
			vitalsigns <- rbind(vitalsigns, c(Subject, name, as.numeric(mean), as.numeric(slope), as.numeric(min), as.numeric(max), as.numeric(first), as.numeric(last)))
		}
	}
	names(vitalsigns) <- c("Subject_ID", "Name", "Mean", "Slope", "Min", "Max", "First", "Last")
	vitalsigns
}

ps_lr <- function(pDelta, pValue, pDefault){
	slopes <- data.frame()
	for(Subject in SubjectIDs){
		slope <- linear_regression_2(Subject, pDelta, pValue, pDefault) 
		slopes <- rbind(slopes, c(Subject, slope))
	}
	names(slopes) <- c("Subject_ID", "slope")
	slopes
}

n_order <- function(pVar, pFit){
	result <- 0
	pVar <- as.numeric(pVar)
	pFit <- as.numeric(pFit)
	for(i in 1:length(pFit)){
		result <- as.double(result + pFit[i] * (pVar^(i-1)))
	}
	result
}	

### Main ###
options(stringsAsFactors = FALSE) # no factors please

get_ALSFRS_scores()

ALSFRS_slopes <- predict_slopes_lr()

Labtest_data <- process_labtests(TRUE) ##TRUE -> with defaults
Vitalsign_data <- process_vitalsigns(TRUE) ###no DEFAULTS while predicting

NN_Table <- generate_nn_table()          ###learning with defaults

for(i in 1:ncol(NN_Table)) {
	NN_Table[,i] <- as.numeric(NN_Table[,i])
}

#clean run for development

#vars fits and strings
earth_vars <- c(	"ALSFRS_Slope",						"Mean_ALSFRS_Score",						"Pulse_Slope",						"Respiratory_Rate_Slope",
			"Blood_Pressure_Systolic_Slope",			"Onset_Delta_sq",							"Urine_Protein_Mean")

earth_string <- "  -0.55913399\n
			+     1.2053995 * pmax(0,                  ALSFRS_Slope -                   -0.83437939) \n  
			-     1.2289048 * pmax(0,                  ALSFRS_Slope -                   -0.41156389) \n  
			-    0.40843679 * pmax(0,                   -0.41156389 -                  ALSFRS_Slope) \n  
			-   0.059661852 * pmax(0,             Mean_ALSFRS_Score -                         16.25) \n  
			+   0.051514355 * pmax(0,             Mean_ALSFRS_Score -                            24) \n  
			-     6.7378442 * pmax(0,                   Pulse_Slope -                  0.0092453818) \n  
			+ 2.7790788e-07 * pmax(0,                  0.0092453818 -                   Pulse_Slope) \n  
			+     10.379449 * pmax(0,        Respiratory_Rate_Slope -                  -0.015843751) \n  
			-     37.930218 * pmax(0,        Respiratory_Rate_Slope -                  0.0011397663) \n  
			+     26.739187 * pmax(0,        Respiratory_Rate_Slope -                   0.012719355) \n  
			+       2.71924 * pmax(0, Blood_Pressure_Systolic_Slope -                  -0.099598535) \n"
 


predicted_slopes <- predict_earth(earth_string, NN_Table)

### Build an R 'list' (i.e., key:value pairs) containing subjectIDs and slopes
predicted <- list(as.numeric(predicted_slopes[,1]), as.numeric(predicted_slopes[,2]))

### Write out the prediction 
write.table(predicted, file="predicted.out", append=FALSE, quote=TRUE, sep=",", col.names=FALSE, row.names=FALSE, qmethod="escape")



################################
#end main
################################
