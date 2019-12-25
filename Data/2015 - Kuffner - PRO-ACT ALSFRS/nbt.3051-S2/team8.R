library(MASS)

# =========================
# GENERAL PURPOSE FUNCTIONS
# =========================


# Transforms a single category of the dataset to sensible columns, instead of related rows
# In other words, this joins all data with the same patient or test_id together into a single row
# @param category the category number
# @param group_by how a single row in the dataset can be identified (patient or patient+test_id)
# @param keys columns will be used, with names(keys) as names of the columns, and the values matching the "key" column
rows.to.form <- function(data,category,group_by,keys)
{
        # Only the data of the current category
        print.expr(paste("finding category", category),cat_data <- data$category==category)
        
        # separate in columns and keys, discarding original
        print.expr("assigning category to values", col_data <- data[cat_data,c(group_by,"value")])
        print.expr("assigning category to keys", kdata <- data[cat_data,"key"])
        
        # Make for more efficient merging by using only existing patients/test ids
        col_data <- factorize.cols(col_data,group_by)
        # One value per column, use unique
        print.expr("indexing",index <- data.frame(unique(col_data[,group_by])))
        # for merging, the names should match
        names(index)=group_by
        
        # Merge each key into a new column
        rdata <- index
        for (k in names(keys)) {
                # all.x=TRUE: Left outer join: the index should always be the same, but the values may be NA
                # 
                print.expr(paste("merging key", k), rdata <- merge(rdata,col_data[kdata==keys[k],],by=group_by,all.x=TRUE,stringsAsFactors=FALSE,sort=FALSE))
        }
        names(rdata)=c(group_by,names(keys))
        return(rdata)
}

# Use factors for all given columns, so that they are "indexed"
factorize.cols <- function(data,cols)
{
        for (col in cols) {
                data[,col] <- factor(data[,col])
        }
        return(data)
}

# Use numeric values for all given columns
# Note that any empty or textual values will result in NA
numerize.cols <- function(data,cols)
{
        for (col in cols) {
                data[,col] <- as.numeric(data[,col])
        }
        return(data)
}

# Print a string, execute an expression and measure the time taken
print.expr <- function(text, expr)
{
        cat(text, ": ", sep="")
        cat(system.time(expr)[[3]], "s\n")
}

# Order data by given factor columns
order.numeric <- function(data,col)
{
        return(data[order(as.numeric(levels(data[,col]))[data[,col]]),])
}

#Write all the different values that occur for a given test to file
write.test.results.to.file <- function(lab,test,outfile)
{
        cat(unique(lab[lab$test==test,"result"]),sep="\n",file=outfile)
}

# all.deltas <- function(data)
# {
#         deltas <- data[data$key %in% c(1203,1225,1234,1236,1200,1417,1418,1190,1174,1267,1456),]
#         deltas[deltas$value=="","value"]="0"
#         deltas <- as.numeric(levels(deltas$value))[deltas$value]
#         month3deltas<-deltas[deltas>=0&deltas<=90]
#         return(deltas)
# }

# Make an interpolation between the tests that we have to a given number of bins
interpolate.tests <- function(name, tests, bins=7)
{
        xs <- as.numeric(names(tests))
        ys <- as.numeric(tests)
        
        # With 7 bins, you have equal bins with 0-12, 13-25, 26-38, 39-51, 52-64, 65-77, 78-90
        # To actually get the middle value (-0.5), realize that the combined size is 91,
        # and because we then have range 1-91, subtract 1.
        target_xs <- round((1:bins - 0.5)*91/bins)-1
        # Linearly interpolate the values between the bins, at the edges use
        # the nearest available value (rule=2)
        target_ys <- (approx(xs, ys, target_xs, rule=2))$y
        # Sensible name: a name plus the delta used for the bin
        names(target_ys) = paste(name, target_xs, sep=".")
        return(target_ys)
}

convert.unit <- function(data,test,oldunit, unit, factor)
{
	old <- data$unit==oldunit & data$test==test
	old[is.na(old)] <- FALSE
	data$result[old] <- data$result[old]*factor
	data$unit[old] <- unit
	return(data)
}

convert.units <- function(data, oldunit, unit, factor)
{
	old <- data$unit==oldunit
	old[is.na(old)] <- FALSE
	data$result[old] <- data$result[old]*factor
	data$unit[old] <- unit
	return(data)
}

# Do a linear fit of the results to the delta's. It will return name.x0 as
# intercept (bias), name.x1 as a slope and name.var as variance compared to
# that slope. As with lm, it will return NA for name.x1 if there is only one
# delta and result given
fit.linear <- function(name,delta,result)
{
	delta <- as.array(delta)
	result <- as.array(result)
	if (length(delta) == 0) {
		ret <- c(NA, NA, NA)
	} else {
		fit <- lm(result~delta)
		# var = sqrt(sum (y_i - x0 - x1 x_i)^2 / (n - 1))
		ret <- c(fit$coefficients[[1]], fit$coefficients[[2]], sqrt(sum((fit$residuals)^2)/(length(delta)-1)))
	}
	names(ret)<-c(paste(name,".x0",sep=""), paste(name,".x1",sep=""),paste(name,".var",sep=""))
	return(ret)
}

add.col.fit <- function(name, comb, ldata)
{
	addcol <- function(patient) {
		data <- ldata[[patient]]
		cols <- unique(data[!is.na(data[,name]),c("delta",name)])
		return(fit.linear(name,cols[,"delta"],cols[,name]))
	}
	return(data.frame(comb,t(sapply(comb$patient,addcol))))
}

add.test.fit <- function(name, comb, ldata)
{
	addcol <- function(patient) {
		data <- ldata[[patient]]
		cols <- unique(data[data$test==name&!is.na(data$result),c("delta","result")])
		return(fit.linear(name,cols$delta,cols$result))
	}
	return(data.frame(comb,t(sapply(comb$patient,addcol))))
}
numeric.cols  <- function(data)
{
	# All columns except the patient column
	cols <- setdiff(which(sapply(data,is.numeric)&!all(is.na(data))),c(1))
	names(cols) <- names(data)[cols]
	return(cols)
}

na.to.mean <- function(data)
{
	# Remove columns with only NA
	data <- data[,!all(is.na(data))]
	# only apply to numeric columns
	vars <- numeric.cols(data)
	
	for (name in vars) {
		# For NA values, take the mean of the other values
		data[is.na(data[,name]),name] <- mean(data[,name],na.rm=TRUE)
	}
	return(data)
}

generic.fit <- function(data, signif)
{
	vars <- numeric.cols(data)
	inset <- signif %in% names(vars)
	if (!all(inset)) {
		print(cat("warning: fit excludes ", signif[!inset]))
	}
	signif <- signif[inset]
	formula <- as.formula(paste("data$slope ~", paste(paste("data$", signif, sep=""), collapse=" + ")))
	return (rlm(formula=formula, data=data))
}

top.cor <- function(data, ratio=0.96)
{
	vars <- setdiff(numeric.cols(data),c("slope"))
	corr <- cor(data[,vars],data$slope)
	rowcorr <- rownames(corr)
	corrm <- rowcorr!="slope"&!is.na(corr)
	corr <- abs(corr[corrm])
	rowcorr <- rowcorr[corrm]
	return(rowcorr[corr >= quantile(corr, probs=ratio)])
}

top.cor.sampled <- function(data, samples=10, ratio=0.96)
{
	allrows <- rownames(data)
	rows <- rownames(data)
	res <- vector("list",10)
	len <- length(rows)/samples
	for (i in 1:samples) {
		s <- sample(rows, len)
		si <- setdiff(allrows, s)
		res[[i]] <- top.cor(data[si,], ratio)

		rows <- setdiff(rows, s)
	}
	return(res)
}

rm.cols <- function(data, cols)
{
	return(data[,setdiff(names(data),cols)])
}

score <- function(slope,pred)
{
	rmsd <- sqrt(sum((slope-pred)^2/length(pred)))
	cat(paste("RMSD:", rmsd, "\n"))
	sc <- -8*log10(rmsd)
	cat(paste("Score:", sc, "\n"))
	return(sc)
}
# =======================
# FORM CREATION FUNCTIONS
# =======================

# Create the Demographics data table
demographics.form <- function(data)
{
        keys <- c(
                "age"=1257,
                "sex"=1205,
                "ethnicity"=1204,
                "race.asian"=1207,
                "race.black"=1208,
                "race.caucasian"=1211,
                "race.unknown"=1210,
                "race.other"=1393,
                "race.other.specify"=1394)
        # Category: 144, Demographics. Group by patient.
        ddata <- rows.to.form(data,144,"patient",keys)
        
        # Convert known values to 1 and 2.
        ddata$sex[ddata$sex=="Male"] <- 1
        ddata$sex[ddata$sex=="Female"] <- 2
        
        # All other data is categorical
        fcols <- c("patient","race.other.specify")
        ddata <- factorize.cols(ddata, fcols)

        # Treat numerical data as such
        ncols <- setdiff(names(keys),fcols)
        ddata <- numerize.cols(ddata, ncols)
        # coerce to 0 for all but sex and age
        ncols <- setdiff(ncols,c("sex","age"))
        ddata[,ncols][is.na(ddata[,ncols])]=0

        return(ddata)
}

# Create the Laboratory Data table
laboratory.form <- function(data)
{
        keys <- c(
                "delta"=1234,
                "test"=1250,
                "result"=1251,
                "unit"=1252)
        # Category: 146, Laboratory Data. Group the results by patient and test_id.
        lab_data <- rows.to.form(data,146,c("patient","test_id"),keys)
        # Treat categorical data as such
        fcols <- c("patient","test","unit")
        lab_data <- factorize.cols(lab_data, fcols)

        # Numerize delta
        lab_data$delta[lab_data$delta==""] = 0
        lab_data$delta <- as.numeric(lab_data$delta)
        
        return(lab_data)
}

# Returns all lab test results that are done on almost all patients
remove.small.lab.samples <- function(lab_data)
{
        np <- nlevels(lab_data$patient)
        tests <- levels(lab_data$test)
        # Find the total frequency of the test, and assume that the number of patients
        # should be at most 110% the number of tests.
        print.expr("calculating frequent lab tests", frequent_tests<-tests[table(lab_data$test)*1.1 >= np])
        freq_ldata <- lab_data[lab_data$test %in% frequent_tests,]
        # make sure to re-index the test names
        print.expr("reindexing", freq_ldata <- factorize.cols(freq_ldata, c("test_id","test","unit")))
        return(freq_ldata)
}

clean.laboratory.form <- function(lab_data)
{
        # Unfortunately not all the results are numeric, otherwise we could take as.numeric(lab_data$result)
        # Removing worst offenders
        lab_data <- lab_data[lab_data$test!="Urine Blood"&lab_data$test!="Urine Casts"&lab_data$test!="RBC Morphology"&lab_data$test!="Urine RBC"&lab_data$test!="Urine WBC"&lab_data$test!="International Normalized Ratio (clotting)"&lab_data$test!="Urine Specific Gravity",]
        lab_data$test <- factor(lab_data$test)

        # First round removing NA
        print.expr("first removal of null values",lab_data <- lab_data[!is.na(lab_data$result),])
        
        # If there is a ">" or "<" sign (why?!) take some higher or lower value
        print.expr("find < or > char", first_char <- sapply(lab_data$result, function(s) substring(s, 1, 1)))
        has_lt <- first_char == "<"
        has_gt <- first_char == ">"
        # Add/subtract 0.1, seems a reasonable value from some of the tests
        lab_data$result[has_gt] <- 0.1 + as.numeric(sapply(lab_data$result[has_gt], function(s) substring(s,2)))
        lab_data$result[has_lt] <- -0.1 + as.numeric(sapply(lab_data$result[has_lt], function(s) substring(s,2)))
        
        # Remove % char, divide by 100 if the unit is not percentage, is this correct?
        print.expr("finding percentages", has_pct <- sapply(lab_data$result, function(s) substring(s, nchar(s)) == "%"))
        print.expr("removing percentages", lab_data$result[has_pct] <- as.numeric(sapply(lab_data$result[has_pct], function(s) substring(s,1,nchar(s)-1))))
        
        # Cleaned the data enough, convert to numeric
        # NN, ND, +, 37.0 U/L, A,  will be coerced to NA (which I guess is fine?)
        print.expr("convert results to numeric",lab_data$result <- as.numeric(lab_data$result))
        lab_data$result[has_pct&(lab_data$unit!="%")] <- lab_data$result[has_pct&(lab_data$unit!="%")]/100.0
        
        # Remove all unparsable data
        print.expr("removing unparsable data",lab_data <- lab_data[!is.na(lab_data$result),])

        return(lab_data)
}

lab.convert.units <- function(data)
{
	# Make sure the levels we assign are possible
	data$unit <- factor(data$unit, levels=unique(c(levels(data$unit),"10E9/l","u/l","g/l","mmol/l","per mm3","%","umol/l","nmol/l","pmol/l","")))
	data$unit[is.na(data$unit)] <- ""

	# Unrealistic red blood cell values are cast to another unit (per mm3)
	data$unit[data$test=="Red Blood Cells (RBC)"&data$result>=100&data$unit=="10E12/L"]<-"per mm3"

	# Following units appear to be erroneous
	data <- data[data$test!="Albumin"   |data$unit!="%",]
	data <- data[data$test!="Hemoglobin"|data$unit!="mg/L",]
	data <- data[data$test!="Protein"   |data$unit!="mEq/L",]
	data <- data[data$test!="Prothrombin Time (clotting)",]
	data <- data[data$test!="Urine Glucose",]

	# This is the same, but it is better to use the same upper/lower case
	data$unit[data$unit %in% c("10E3/mm3","10E9/L")]              <- "10E9/l"
	data$unit[data$unit %in% c("IU/L","iu/l","iu/L","U/L","u/L")] <- "u/l"
	data$unit[data$unit %in% c("G/L","g/L","G/l")]                <- "g/l"
	data$unit[data$unit %in% c("mmol/L","MMOL/L")]                <- "mmol/l"
	data$unit[data$unit=="umol/L"]                                <- "umol/l"
	data$unit[data$unit=="nmol/L"]                                <- "nmol/l"
	data$unit[data$unit=="pmol/L"]                                <- "pmol/l"

	# Convert between metric units
	data <- convert.units(data, "g/dL",     "g/l",    10.0)
	data <- convert.units(data, "10E12/L",  "10E9/l", 1/1000.0)
	data <- convert.units(data, "per mm3",  "10E9/l", 1/1000.0)
	data <- convert.units(data, "10E6/mm3", "10E9/l", 1000.0)
	data <- convert.units(data, "V/V",      "%",      100.0)
	
	celltest <- c("Basophils","Eosinophils","Lymphocytes","Neutrophils","Monocytes")
	# Removing all g/L if the type is one of nogram
	nogram <- c(celltest, "White Blood Cells (WBC)", "Platelets")
	data <- data[!(data$unit=="g/l"&(data$test %in% nogram)),]

	# If listed as percentage and it is a cell test, compute as percentage of WBC
	cellP <- data$unit=="%"&(data$test %in% celltest)
	wbc <- data[data$test=="White Blood Cells (WBC)",c("patient","delta","result")]
	wbc <- wbc[!duplicated(wbc[,c("patient","delta")]),]
	cellMerge <- merge(data[cellP,c("patient","delta","result")],wbc,by=c("patient","delta"),all.x=TRUE)
	data[cellP,"result"]<-cellMerge$result.x*cellMerge$result.y/100.0
	data[cellP,"unit"  ]<-"10E9/l"
	
	data <- convert.unit(data, "Triglycerides",     "g/l",   "mmol/l", 1/1.13)
	data <- convert.unit(data, "Total Cholesterol", "g/l",   "mmol/l", 2.59)
	data <- convert.unit(data, "Urea",              "mg/dL", "mmol/l", 0.357)
	data <- convert.unit(data, "Urea",              "g/l",   "mmol/l", 35.7/2.14)
	data <- convert.unit(data, "Uric Acid",         "mg/dL", "umol/l", 59.48)
	data <- convert.unit(data, "Protein",           "g/dL",  "g/l",    10)
	data <- convert.unit(data, "Hemoglobin",        "g/l",   "mmol/l", 0.06206)
	data <- convert.unit(data, "Hematocrit",        "1",     "%",      100.0)
	data <- convert.unit(data, "Free T3",           "pmol/l","nmol/l", 1/1000.0)
	data <- convert.unit(data, "Bilirubin (total)", "mg/L",  "umol/l", 1.71)
	data <- convert.unit(data, "Calcium",           "mEq/L", "mmol/l", 0.5)
	data <- convert.unit(data, "Calcium",           "mg/L",  "mmol/l", 0.025)
	data <- convert.unit(data, "Creatine Kinase",   "mIU/L", "u/l",    1)
	data <- convert.unit(data, "Creatinine",        "mg/L",  "umol/l", 8.84)
	data <- convert.unit(data, "Glucose",           "g/l",   "mmol/l", 5.55)
	data <- convert.unit(data, "Phosphorus",        "mg/L",  "mmol/l", 0.0323)
	
	data <- convert.units(data, "mg/L",     "g/l",    1/1000.0)
	
	data$unit[data$unit=="mEq/L"] <- "mmol/l"
	
	data$unit <- factor(data$unit)

	# Diff: Free T3
	
	return(data)
}

alsfr.form <- function(data)
{
        keys <- c(
                "delta"=1225,
                "Total"=1228,
                "R.Total"=1229,
                "T01"=1213,
                "T02"=1215,
                "T03"=1216,
                "T04"=1217,
                "T05a"=1218,
                "T05b"=1219,
                "T06"=1220,
                "T07"=1221,
                "T08"=1222,
                "T09"=1223,
                "T10"=1214,
                "R1"=1230,
                "R2"=1231,
                "R3"=1232)
        use_data <- data[data$category==145,]
        use_data$value <- as.numeric(use_data$value)
        als_data <- rows.to.form(use_data,145,c("patient","test_id"),keys)
        als_data <- factorize.cols(als_data,c("patient","test_id"))
        
        # Set default delta to 0
        als_data$delta[als_data$delta==""] = 0
        
        # All keys are numeric
        als_data <- numerize.cols(als_data,names(keys))
        
        # Replace 05a and 05b with column 05a (<-05a|05b) and column with.gastronomy indicating whether 05b was used.
        fiveA_NA <- is.na(als_data$T05a)
        als_data$T05a[fiveA_NA]<-als_data$T05b[fiveA_NA]
        exceptFiveB <- setdiff(names(als_data),c("T05b"))
        als_data <- data.frame(als_data[,exceptFiveB],"with.gastronomy"=as.numeric(fiveA_NA))

        # Replace 10 with column R1 (Dyspnea) if it is applicable
        ten_NA <- is.na(als_data$T10)
        als_data$T10[ten_NA] <- als_data$R1[ten_NA]

		# Recalculate total
		als_data$Total <- rowSums(als_data[,c("T01","T02","T03","T04","T05a","T06","T07","T08","T09","T10")])

        return(als_data)
}

vital.form <- function(data)
{
        keys <- c(
                "delta"=1174, # total 13020 measurements; for 0 <= delta <= 90: 5857 measurements: 6.5/patient
                "blood.pressure.dia"=1169,
                "blood.pressure.dia.standing"=1446,
                #"blood.pressure.dia.standing.base"=1428,
                #"blood.pressure.dia.standing.end"=1440,
                "blood.pressure.dia.supine"=1444,
                #"blood.pressure.dia.supine.base"=1430,
                #"blood.pressure.dia.supine.end"=1442,
                #"blood.pressure.dia.unit"=1410, # always "MMHG"
                "blood.pressure.sys"=1170,
                "blood.pressure.sys.standing"=1447,
                #"blood.pressure.sys.standing.base"=1429,
                #"blood.pressure.sys.standing.end"=1441,
                "blood.pressure.sys.supine"=1445,
                #"blood.pressure.sys.supine.base"=1431,
                #"blood.pressure.sys.supine.end"=1443,
                #"blood.pressure.sys.unit"=1413, # always "MMHG"
                "height"=1171, # measured for 518 patients
                "height.unit"=1181, # values: {"": 691, "CM": 552, "Centimeters": 74, "Inches": 12} default is CM if not measured
                "pulse"=1175,
                "pulse.standing"=1434,
                #"pulse.standing.base"=1438,
                #"pulse.standing.end"=1436,
                "pulse.supine"=1433,
                #"pulse.supine.base"=1437,
                #"pulse.supine.end"=1435,
                #"pulse.unit"=1411, # always "BEATS/MIN"
                "respiratory"=1176,
                #"respiratory.unit"=1412, # always "BREATHS/MIN"
                "weight"=1178,
                #"weight.base"=1432,
                #"weight.end"=1439,
                "weight.unit"=1180 # values: {"": 462, "2": 621, "Kilograms": 10393, "Pounds": 120}
                )
        vital_data <- rows.to.form(data,183,c("patient","test_id"),keys)

        # Set default delta to 0
        vital_data$delta[vital_data$delta==""] = 0

        # factorize categorical data
        fcols <- c("patient","test_id","height.unit","weight.unit")
        vital_data <- factorize.cols(vital_data,fcols)
        
        # all other columns are numeric
        ncols <- setdiff(names(keys),fcols)
        vital_data <- numerize.cols(vital_data,ncols)
        
        # Convert inches to centimeters
        inches <- !is.na(vital_data$height.unit)&vital_data$height.unit=="Inches"
        vital_data$height[inches]<-vital_data$height[inches]*2.54

        # Convert pounds to kilograms
        pounds <- !is.na(vital_data$weight.unit)&vital_data$weight.unit=="Pounds"
        vital_data$weight[pounds]<-vital_data$weight[pounds]*0.45359237
        
        # remove the units
        vital_data <- vital_data[,setdiff(names(vital_data),c("height.unit","weight.unit"))]

        # Some tests are completely empty, except for the delta which we set forcibly
        lcols <- length(ncols)-1
        vital_data <- vital_data[rowSums(is.na(vital_data))<lcols,]
	
	# Convert all values to non-supine and non-standing, by taking the average between the two, and subtracting
	# the difference with the mean of the non-supine/non-standing.
	# Diastolic
	# standing = 84.19274, supine = 82.70044, non = 81.05476; (standing + supine) / 2 - non = 2.39183
	noblood <- is.na(vital_data$blood.pressure.dia)&!is.na(vital_data$blood.pressure.dia.supine)&!is.na(vital_data$blood.pressure.dia.standing)
	vital_data$blood.pressure.dia[noblood]<-(vital_data$blood.pressure.dia.standing[noblood]+vital_data$blood.pressure.dia.supine[noblood])/2-2.39183

	# Systolic
	# standing = 134.7334, supine = 133.344, non = 130.5034; (standing + supine) / 2 - non = 3.5353
	noblood <- is.na(vital_data$blood.pressure.sys)&!is.na(vital_data$blood.pressure.sys.supine)&!is.na(vital_data$blood.pressure.sys.standing)
	vital_data$blood.pressure.sys[noblood]<-(vital_data$blood.pressure.sys.standing[noblood]+vital_data$blood.pressure.sys.supine[noblood])/2-3.5353
	
	# Pulse
	# standing = 79.5434, supine = 74.6895, non = 76.85084; (standing + supine) / 2 - non = 0.26561
	nopulse <- is.na(vital_data$pulse)&!is.na(vital_data$pulse.supine)&!is.na(vital_data$pulse.standing)
	vital_data$pulse[nopulse]<-(vital_data$pulse.standing[nopulse]+vital_data$pulse.supine[nopulse])/2-3.5353
	
	# Remove the columns
	vital_data <- vital_data[,setdiff(names(vital_data),c("blood.pressure.dia.standing","blood.pressure.dia.supine","blood.pressure.sys.standing","blood.pressure.sys.supine","pulse.standing","pulse.supine"))]

	# Set height the same for the same patient
	vital_data <- merge(vital_data[,setdiff(names(vital_data),c("height"))],vital_data[!is.na(vital_data$height),c("patient","height")],all.x=TRUE,by=c("patient"))
	# bmi = kg/(m*m) = kg/(cm/100*cm/100) = 100*100*kg/(cm*cm)
	vital_data <- data.frame(vital_data, "bmi"=10000*vital_data$weight/(vital_data$height*vital_data$height))
	
	return(vital_data)
}

forms.combine <- function(vital,vital_forced,als_hist,als,demography,test)
{
	# Combine only patients with als data
	patients <- unique(als$patient)
	# Split the dataset into one dataset per patient
	als3m <- als[als$delta<=91,]
	ldata <- split(als3m, als3m$patient)

	# Start with only the patient ID
	comb <- data.frame(patient=patients)

	print.expr("adding ALSFR score", comb <- add.col.fit("Total", comb, ldata))
	print.expr("adding ALSFR1 score", comb <- add.col.fit("T01", comb, ldata))
	print.expr("adding ALSFR2 score", comb <- add.col.fit("T02", comb, ldata))
	print.expr("adding ALSFR3 score", comb <- add.col.fit("T03", comb, ldata))
	print.expr("adding ALSFR4 score", comb <- add.col.fit("T04", comb, ldata))
	print.expr("adding ALSFR5a score", comb <- add.col.fit("T05a", comb, ldata))
	print.expr("adding ALSFR6 score", comb <- add.col.fit("T06", comb, ldata))
	print.expr("adding ALSFR7 score", comb <- add.col.fit("T07", comb, ldata))
	print.expr("adding ALSFR8 score", comb <- add.col.fit("T08", comb, ldata))
	print.expr("adding ALSFR9 score", comb <- add.col.fit("T09", comb, ldata))
	print.expr("adding ALSFR10 score", comb <- add.col.fit("T10", comb, ldata))
	
	# Add age
	demography$patient <- factor(demography$patient, levels(als$patient))
	print.expr("adding age and sex", comb <- merge(comb,demography[,c("patient","age","sex")],by=c("patient"),all=TRUE))

	# Split the dataset into one dataset per patient
	vital$patient <- factor(vital$patient, levels(als$patient))
	vital3m <- vital[vital$delta <= 91,]
	print.expr("splitting vital data into patients", ldata <- split(vital3m,vital3m$patient))

	print.expr("adding blood pressure diastolic", comb <- add.col.fit("blood.pressure.dia", comb, ldata))
	print.expr("adding blood pressure systolic", comb <- add.col.fit("blood.pressure.sys", comb, ldata))
	print.expr("adding pulse", comb <- add.col.fit("pulse", comb, ldata))
	print.expr("adding respiratory", comb <- add.col.fit("respiratory", comb, ldata))
	print.expr("adding bmi", comb <- add.col.fit("bmi", comb, ldata))
	
	# Use the same levels as vital, so the split doesn't get permutated
	vital_forced$patient <- factor(vital_forced$patient, levels(als$patient))
	vital_forced3m <- vital_forced[vital_forced$delta <= 91,]
	print.expr("splitting vital_forced data into patients", ldata <- split(vital_forced3m,vital_forced3m$patient))
	
	print.expr("adding vital forced (subject.liters)", comb <- add.col.fit("subject.liters", comb, ldata))
	
	als_hist$patient <- factor(als_hist$patient, levels(als$patient))
	comb <- merge(comb,als_hist[,c("patient","onset.delta","onset.location","diagnosis.delta")],by=c("patient"),all=TRUE)
	
	test3m <- test[test$delta <= 91,]
	test3m$patient <- factor(test3m$patient, levels(als$patient))
	test3m$test <- factor(test3m$test)
	print.expr("splitting lab data into patients", ldata <- split(test3m,test3m$patient))
	
	for (name in levels(test3m$test)) {
		print.expr(paste("adding lab test",name), comb <- add.test.fit(name, comb, ldata))
	}
	
	#slopes$patient <- factor(slopes$patient, levels(als$patient))
	#comb <- merge(comb,by=c("patient"),all=TRUE)
	
	# fam$patient <- factor(fam$patient, levels(fam$patient))
	# comb <- merge(comb, fam[,c("patient","disease")],all.x=TRUE,by="patient")
	
	
	return (comb)
}

forms.exp.combine <- function(vital,vital_forced,als_hist,als,demography,test,fam)
{
	is.pos <- function (x) all(is.na(x)|x>=0)
	# Combine only patients with als data
	patients <- unique(als$patient)
	# Split the dataset into one dataset per patient
	usenames <- c("Total","T01","T02","T03","T04","T05a","T06","T07","T08","T09","T10")
	als3m <- als[als$delta<=91&apply(als[,usenames],1,is.pos),]
	als3m[,usenames] <- log(als3m[,usenames]+1)
	
	ldata <- split(als3m, als3m$patient)

	# Start with only the patient ID
	comb <- data.frame(patient=patients)

	print.expr("adding ALSFR score", comb <- add.col.fit("Total", comb, ldata))
	print.expr("adding ALSFR1 score", comb <- add.col.fit("T01", comb, ldata))
	print.expr("adding ALSFR2 score", comb <- add.col.fit("T02", comb, ldata))
	print.expr("adding ALSFR3 score", comb <- add.col.fit("T03", comb, ldata))
	print.expr("adding ALSFR4 score", comb <- add.col.fit("T04", comb, ldata))
	print.expr("adding ALSFR5a score", comb <- add.col.fit("T05a", comb, ldata))
	print.expr("adding ALSFR6 score", comb <- add.col.fit("T06", comb, ldata))
	print.expr("adding ALSFR7 score", comb <- add.col.fit("T07", comb, ldata))
	print.expr("adding ALSFR8 score", comb <- add.col.fit("T08", comb, ldata))
	print.expr("adding ALSFR9 score", comb <- add.col.fit("T09", comb, ldata))
	print.expr("adding ALSFR10 score", comb <- add.col.fit("T10", comb, ldata))
	
	# Add age
	demography$patient <- factor(demography$patient, levels(als$patient))
	print.expr("adding age and sex", comb <- merge(comb,demography[,c("patient","age","sex")],by=c("patient"),all=TRUE))

	# Split the dataset into one dataset per patient
	vital$patient <- factor(vital$patient, levels(als$patient))
	usenames <- c("blood.pressure.dia","blood.pressure.sys","pulse","respiratory","bmi")
	vital3m <- vital[vital$delta <= 91&apply(vital[,usenames], 1, is.pos),]
	vital3m[,usenames] <- log(vital3m[,usenames]+1)
	
	print.expr("splitting vital data into patients", ldata <- split(vital3m,vital3m$patient))

	print.expr("adding blood pressure diastolic", comb <- add.col.fit("blood.pressure.dia", comb, ldata))
	print.expr("adding blood pressure systolic", comb <- add.col.fit("blood.pressure.sys", comb, ldata))
	print.expr("adding pulse", comb <- add.col.fit("pulse", comb, ldata))
	print.expr("adding respiratory", comb <- add.col.fit("respiratory", comb, ldata))
	print.expr("adding bmi", comb <- add.col.fit("bmi", comb, ldata))
	
	# Use the same levels as vital, so the split doesn't get permutated
	vital_forced$patient <- factor(vital_forced$patient, levels(als$patient))
	vital_forced3m <- vital_forced[vital_forced$delta <= 91&!is.na(vital_forced$subject.liters)&vital_forced$subject.liters>=0,]
	vital_forced3m$subject.liters <- log(vital_forced3m$subject.liters+1)
	
	print.expr("splitting vital_forced data into patients", ldata <- split(vital_forced3m,vital_forced3m$patient))
	
	print.expr("adding vital forced (subject.liters)", comb <- add.col.fit("subject.liters", comb, ldata))
	
	als_hist$patient <- factor(als_hist$patient, levels(als$patient))
	comb <- merge(comb,als_hist[,c("patient","onset.delta","onset.location","diagnosis.delta")],by=c("patient"),all=TRUE)
	
	test3m <- test[test$delta <= 91&!is.na(test$result)&test$result >= 0,]
	test3m$patient <- factor(test3m$patient, levels(als$patient))
	test3m$test <- factor(test3m$test)
	test3m$result <- log(test3m$result+1)
	
	print.expr("splitting lab data into patients", ldata <- split(test3m,test3m$patient))
	
	for (name in levels(test3m$test)) {
		print.expr(paste("adding lab test",name), comb <- add.test.fit(name, comb, ldata))
	}
	
	# fam$patient <- factor(fam$patient, levels(fam$patient))
	# comb <- merge(comb, fam[,c("patient","disease")],all.x=TRUE,by="patient")
	
	return (comb)
}

vital.forced.form <- function(data)
{
        keys <- c(
                "delta"=1190,
                "subject.liters"=1185,
                "subject.normal"=1188,
                "unit"=1405)
        vital_data <- rows.to.form(data,149,c("patient","test_id"),keys)
        if (nrow(vital_data)==0) return(vital_data)

        # Set default delta to 0
        vital_data$delta[vital_data$delta==""] = 0

        # factorize categorical data
        fcols <- c("patient","test_id","unit")
        vital_data <- factorize.cols(vital_data,fcols)
        
        # all other columns are numeric
        ncols <- setdiff(names(keys),fcols)
        vital_data <- numerize.cols(vital_data,ncols)
        
        # Some tests are completely empty, except for the delta which we set forcibly
        lcols <- length(ncols)-1
        vital_data <- vital_data[rowSums(is.na(vital_data))<lcols,]
        
        return(vital_data)
}

vital.slow.form <- function(data)
{
        keys <- c(
                "delta"=1267,
                "subject.liters"=1262)
        vital_data <- rows.to.form(data,212,c("patient","test_id"),keys)
        if (nrow(vital_data)==0) return(vital_data)

        # Set default delta to 0
        vital_data$delta[vital_data$delta==""] = 0

        # factorize categorical data
        fcols <- c("patient","test_id")
        vital_data <- factorize.cols(vital_data,fcols)
        
        # all other columns are numeric
        ncols <- setdiff(names(keys),fcols)
        vital_data <- numerize.cols(vital_data,ncols)
        
        # Some tests are completely empty, except for the delta which we set forcibly
        lcols <- length(ncols)-1
        vital_data <- vital_data[rowSums(is.na(vital_data))<lcols,]
        
        return(vital_data)
}

treatment.form <- function(data)
{
        keys <- c(
                "delta"=1456,
                "study.arm"=1454) # in delta <= 90: {ACTIVE: 110, PLACEBO: 119}
        treatment_data <- rows.to.form(data,217,c("patient","test_id"),keys)

        # Set default delta to 0
        treatment_data$delta[treatment_data$delta==""] = 0

        # factorize categorical data
        fcols <- c("patient","test_id","study.arm")
        treatment_data <- factorize.cols(treatment_data,fcols)
                
        # all other columns are numeric
        ncols <- setdiff(names(keys),fcols)
        treatment_data <- numerize.cols(treatment_data,ncols)
        return(treatment_data)
}

family.form <- function(data)
{
        keys <- c(
                "delta"=1236,
                "disease"=1419,
                "disease.specify"=1420,
                "aunt"=1288,
                "aunt.m"=1289,
                "aunt.p"=1290,
                "cousin"=1291,
                "cousin.m"=1292,
                "cousin.p"=1293,
                "father"=1294,
                "grandfather"=1295,
                "grandfather.m"=1296,
                "grandfather.p"=1297,
                "grandmother"=1298,
                "grandmother.m"=1299,
                "grandmother.p"=1300,
                "mother"=1301,
                "nephew"=1302,
                "niece"=1305,
                "sibling"=1309,
                "uncle"=1311,
                "uncle.m"=1312,
                "uncle.p"=1313,
                "son"=1424,
                "daughter"=1425,
                "sister"=1426,
                "brother"=1427
                )
        family_data <- rows.to.form(data,147,c("patient","test_id"),keys)
        if (nrow(family_data)==0) return(family_data)
        
        # Set default values to 0 (find indices where =="", and replace by 0)
        family_data[family_data==""] = 0

        # Diseases can not be numbers
        family_data$disease[family_data$disease==0]=NA
        family_data$disease.specify[family_data$disease.specify==0]=NA

        # factorize categorical data
        fcols <- c("patient","test_id","disease","disease.specify")
        family_data <- factorize.cols(family_data,fcols)
        
        # all other columns are numeric
        ncols <- setdiff(names(keys),fcols)
        family_data <- numerize.cols(family_data,ncols)
        return(family_data)
}

als.hist.form <- function(data,demography)
{
        keys <- c(
                "delta"=1200,
                "symptom"=1247,
                "location"=1249,
                "diagnosis.delta"=1418,
                "onset.delta"=1417,
                "onset.location"=1416,
                "onset.age"=1245
                )
        als_data <- rows.to.form(data,148,c("patient","test_id"),keys)

        # delta's and age are numeric
        ncols <- c("delta", "onset.delta", "diagnosis.delta", "onset.age")
        als_data <- numerize.cols(als_data,ncols)

        # factorize categorical data
        fcols <- setdiff(c("patient","test_id",names(keys)),ncols)
        als_data <- factorize.cols(als_data,fcols)
        
        # onset age can be deduced from onset delta and current age
        deduce_onset <- !is.na(als_data$onset.delta)
        if (sum(deduce_onset) > 0) als_data$onset.age[deduce_onset] <- demography$age[match(als_data$patient[deduce_onset],demography$patient)]+als_data$onset.delta[deduce_onset]/365

        # Merge the data of the ALS symptom with the ALS onset
        onset <- which(!is.na(als_data$onset.location)&is.na(als_data$symptom))
        if (length(onset)>0) {
                diagnosis <- match(als_data$patient[onset],als_data$patient[!is.na(als_data$symptom)])
                onset<-onset[!is.na(diagnosis)]
                diagnosis<-diagnosis[!is.na(diagnosis)]
                if (length(onset)>0) {
                        print.expr(paste("merging symptom",length(diagnosis),"times"),als_data[onset,c("symptom","location","diagnosis.delta","onset.delta","onset.age")] <- als_data[diagnosis,c("symptom","location","diagnosis.delta","onset.delta","onset.age")])
                }
        }

        # Merge the data of the ALS onset delta with the ALS onset
        onset <- which(!is.na(als_data$onset.location)&is.na(als_data$onset.delta))
        if (length(onset)>0) {
                diagnosis <- match(als_data$patient[onset],als_data$patient[!is.na(als_data$onset.delta)])
                onset<-onset[!is.na(diagnosis)]
                diagnosis<-diagnosis[!is.na(diagnosis)]
                if (length(onset)>0) {
                        print.expr(paste("merging onset delta",length(diagnosis),"times"),als_data[onset,c("symptom","location","diagnosis.delta","onset.delta","onset.age")] <- als_data[diagnosis,c("symptom","location","diagnosis.delta","onset.delta","onset.age")])
                }
        }
        
        #ASSUME: site 1 is bulbar, site 3 is limb (seems possible given the ratio 1:5)
        als_data$onset.location[als_data$onset.location=="1"] = "Onset: Bulbar"
        als_data$onset.location[als_data$onset.location=="3"] = "Onset: Limb"
        als_data <- als_data[!is.na(als_data$onset.location),]
        als_data$onset.location <- factor(als_data$onset.location)
        
        return(als_data)
}

comb.select.disease <- function(comb, disease)
{
	comb <- comb[comb$disease %in% c(disease),]
	return(na.to.mean(comb))
}


comb.select.location <- function(comb, location)
{
	comb <- comb[comb$onset.location %in% c(location),]
	return(na.to.mean(comb))
}

combL.fit <- function(comb)
{
	return(generic.fit(comb,
		c(	"Total.x1",
			"subject.liters.x1",
			"bmi.x1",
			"sex")))
}

combL.fit2 <- function(comb)
{
	return(generic.fit(comb,
		c(	"diagnosis.delta",
			"T07.x1",
			"T01.x1",
			"T03.var",
			"T08.x1")))
}

combL.predict <- function(comb)
{
	return(predict.lin(comb,c(
			"intercept"=-0.50153,
			"Total.x1"=3.09764,
			"subject.liters.x1"=18.93059,
			"bmi.x1"=6.61562,
			"sex"=-0.04605)))
}
combL.predict2 <- function(comb)
{
	return(predict.lin(comb,
		c(	"intercept"=-0.6674658,
			"diagnosis.delta"=-0.0002468,
			"T07.x1"=5.1697039,
			"T01.x1"=1.9618256,
			"T03.var"=-0.1512376,
			"T08.x1"=2.6373339)))
}

combB.fit <- function(comb)
{
	return(generic.fit(comb,
		c(	"Total.x1",
			"sex",
			"subject.liters.x1",
			"ALT.SGPT..var",
			"Phosphorus.x0")))
}
combB.fit2 <- function(comb)
{
	return(generic.fit(comb,
		c(	"pulse.x0",
			"pulse.var",
			"Eosinophils.x1",
			"T02.x1",
			"subject.liters.x1",
			"T09.x0",
			"T06.x0",
			"T04.x0",
			"Neutrophils.x0",
			"Urine.Protein.x1")))
}

combB.predict <- function(comb)
{
	return(predict.lin(comb,c(
		"intercept"=0.39038,
		"Total.x1"=3.01256,
		"sex"=-0.10471,
		"subject.liters.x1"=13.30749,
		"ALT.SGPT..var"=-0.02484,
        "Phosphorus.x0"=-0.69917)))
}

combB.predict2 <- function(comb)
{
	return(predict.lin(comb,
		c(	"intercept"=0.117583,
			"pulse.x0"=-0.008143,
			"pulse.var"=-0.009881,
			"Eosinophils.x1"=-0.887195,
			"T02.x1"=4.218051,
			"subject.liters.x1"=20.565352,
			"T09.x0"=-0.091629,
			"T06.x0"=-0.059793,
			"T04.x0"=0.082821,
			"Neutrophils.x0"=0.008111,
			"Urine.Protein.x1"=34.081285)))
}


combALS.fit <- function(comb)
{
	return(predict.lin(comb,
		c(	"intercept"=-0.142438,
			"Total.x1"=2.735344,
			"Triglycerides.x0"=-0.031742,
			"Triglycerides.var"=-0.202933)))
}

combALS.predict <- function(comb)
{
	return(generic.fit(comb,
		c(	"Total.x1",
			"diagnosis.delta",
			"Hemoglobin.x1",
			"Lymphocytes.var",
			"Triglycerides.x0",
			"Triglycerides.var",
			"White.Blood.Cells..WBC..x0")))
}

comb.fit <- function(comb)
{
	return(generic.fit(comb,
		c("T01.x0",
		"T01.x1",
		"T02.x1",
		"T03.x0",
		"T04.x0",
		"T05a.x0",
		"T06.x0",
		"T07.x1",
		"subject.liters.x0",
		"Neutrophils.x1",
		"White.Blood.Cells..WBC..x1")))
}

comb.predict <- function(comb)
{
	return(predict.lin(comb,
		c(	"intercept"=-1.10594,
			"Total.x1"=3.20421,
			"pulse.x1"=0.22059,
			"subject.liters.x1"=19.90935,
			"subject.liters.var"=-0.08518,
			"bmi.x1"=5.61068,
			"Neutrophils.x0"=0.02421,
			"Uric.Acid.x0"=0.00152)))
}


comb.predict2 <- function(comb)
{
	return(predict.lin(comb,
		c(	"intercept"=-0.86879,
			"T01.x0"=0.11186,
			"T01.x1"=10.41918,
			"T02.x1"=4.37366,
			"T03.x0"=-0.01953,
			"T04.x0"=-0.11065,
			"T05a.x0"=0.04916,
			"T06.x0"=0.04697,
			"T07.x1"=4.97646,
			"subject.liters.x0"=-0.00560,
			"Neutrophils.x1"=-0.57059,
			"White.Blood.Cells..WBC..x1"=0.40878)))
}

combF.predict <- function(comb)
{
	return(predict.lin(comb,
		c( "intercept"=1.617100e-01,
			"Total.x1"=4.161613e+00,
			"blood.pressure.dia.x0"=-9.825584e-03,
			"T10.x0"=1.562558e-01,
			"Basophils.x0"=-5.216157e-02,
			"Potassium.x0"=-2.116200e-01,
			"Total.var"=1.388601e-01,
			"Triglycerides.x1"=-1.174815e+01,
			"Alkaline.Phosphatase.x1"=-8.116062e-02,
			"Lymphocytes.x1"=-1.158547e+00,
			"T02.x1"=7.359569e+00,
			"Creatine.Kinase.x1"=4.197933e-02,
			"Platelets.x1"=1.065751e-01,
			"Glucose.x0"=5.333003e-02,
			"Monocytes.x1"=1.449043e+00,
			"pulse.var"=-1.656332e-02,
			"subject.liters.x1"=2.605422e+01,
			"diagnosis.delta"=-2.051913e-04
		)))
}

combLmF.predict <- function(comb)
{
     return(predict.lin(comb,
		c( 	"intercept"=-0.1897731449,
			"T04.x0"=-0.0896295849,
			"subject.liters.x1"=11.8989467388,
			"T07.x1"=4.0783058441,
			"T05a.var"=-0.3509283856,
			"pulse.x0"=-0.0040043895,
			"T01.var"=-0.2750455812,
			"diagnosis.delta"=-0.0002986005,
			"Creatinine.x1"=0.1961481627,
			"T06.x0"=0.0810978301)))
}

combBmMF.predict <- function(comb)
{
	return(predict.lin(comb,
		c("intercept"=-5.935106553,
			"Creatine.Kinase.var"=-0.000871967,
			"Sodium.x0"=0.050248814,
			"Phosphorus.x0"=-1.065114372,
			"Creatinine.x1"=1.281682557,
			"HbA1c..Glycated.Hemoglobin..var"=0.487879827,
			"T01.var"=-0.052466644,
			"pulse.x1"=0.641234430,
			"blood.pressure.sys.var"=0.009670242,
			"blood.pressure.dia.x1"=-0.384179508,
			"respiratory.x0"=-0.023440651,
			"Phosphorus.var"=-2.192648195,
			"T07.var"=-0.417501935,
			"Urea.x1"=-2.964874863,
			"Urea.x0"=-0.033962445,
			"age"=0.007980462,
			"T09.x0"=-0.088722714,
			"T01.x1"=17.360637376)))
}

combBpM.predict <- function(comb)
{
	return(predict.lin(comb,c(
		"intercept"=3.90729192,
		"Eosinophils.x1"=3.84766215,
		"Urea.x1"=49.63752237,
		"ALT.SGPT..x0"=-0.01530378,
		"Bilirubin..total..x1"=4.57874734,
		"Protein.x0"=0.04377333,
		"Calcium.var"=6.06262130,
		"Calcium.x0"=-2.30667079,
		"Absolute.Lymphocyte.Count.x1"=-66.94497539,
		"Absolute.Basophil.Count.var"=6.75488938,
		"Potassium.x0"=-0.48723787,
		"T06.var"=-0.62430820,
		"T03.var"=0.50490272,
		"Potassium.var"=-1.42687051,
		"Amylase.x1"=0.25756856,
		"Creatinine.x1"=0.67373900,
		"T01.x1"=4.89163489)))
		
}

combF.fit <- function(comb)
{
	return(generic.fit(comb,
		c(  "Total.x1",
			"blood.pressure.dia.x0",
			"T10.x0",
			"Basophils.x0",
			"Potassium.x0",
			"Total.var",
			"Triglycerides.x1",
			"Alkaline.Phosphatase.x1",
			"Lymphocytes.x1",
			"T02.x1",
			"Creatine.Kinase.x1",
			"Platelets.x1",
			"Glucose.x0",
			"Monocytes.x1",
			"pulse.var",
			"subject.liters.x1",
			"diagnosis.delta"
		)))
}

combLmF.fit <- function(comb) {
	  return(generic.fit(comb,
	 c(
	 "T04.x0",
			 "subject.liters.x1",
	
	
	 "T07.x1",
	
	
	 "T05a.var",
	
	
	 "pulse.x0",
	
	
	 "T01.x0",
	
	
	 "T09.x0",
	
	
	 "T01.var",
	
	
	 "diagnosis.delta",
	
	
	 "Creatinine.x1",
	
	
	 "T06.x0"))) }
combBmMF.fit <- function(comb)
{
	return(generic.fit(comb,
		c(	"Creatine.Kinase.var",
			"Sodium.x0",
			"Phosphorus.x0",
			"Creatinine.x1",
			"HbA1c..Glycated.Hemoglobin..var",
			"T01.var",
			"pulse.x1",
			"blood.pressure.sys.var",
			"blood.pressure.dia.x1",
			"respiratory.x0",
			"Phosphorus.var",
			"T07.var",
			"Urea.x1",
			"Urea.x0",
			"age",
			"T09.x0",
			"T01.x1")))
}

combBpM.fit <- function(comb)
{
	return(generic.fit(comb,c(
		"Eosinophils.x1",
		"Urea.x1",
		"ALT.SGPT..x0",
		"Bilirubin..total..x1",
		"Protein.x0",
		"Calcium.var",
		"Calcium.x0",
		"Absolute.Lymphocyte.Count.x1",
		"Absolute.Basophil.Count.var",
		"Potassium.x0",
		"T06.var",
		"T03.var",
		"Potassium.var",
		"Amylase.x1",
		"Creatinine.x1",
		"T01.x1")))
		
}

comb.plot <- function(file, comb, fit)
{
	png(file,5600,4800)
	par(mfrow = c(14, 12), pty = "s")
	for( name in setdiff(names(comb),c("patient","slope"))) {
		plot(x=comb[abs(fit$residuals)<0.25,name],y=comb$slope[abs(fit$residuals)<0.25],col="red",xlab=name,ylab="slope")
		points(x=comb[abs(fit$residuals)>=0.25,name],y=comb$slope[abs(fit$residuals)>=0.25],col="blue")
	}
	dev.off()
}

hist.plot <- function(dir,combSlow, combMiddle, combFast)
{
	for( name in setdiff(names(comb),c("patient","slope"))) {
		if (!is.numeric(combSlow[,name])) next
		slow <- combSlow[!is.na(combSlow[,name]),name]
		mid <- combMiddle[!is.na(combMiddle[,name]),name]
		fast <- combFast[!is.na(combFast[,name]),name]
		if (length(slow)==0|length(mid)==0|length(fast)==0) {
			cat("Omitting ", name, "; it is zero\n")
			next
		}
		height <- max(length(slow),length(mid),length(fast))
		xlim <- c(min(slow,mid,fast), max(slow,mid,fast))
		breaks <- seq(from=xlim[[1]],to=xlim[[2]],length.out=6)
		png(paste("~/Dropbox/Work/ALS/",dir,"/",name,".png",sep=""),480,900)
		par(mfrow = c(3, 1), pty = "s")
		hist(x=slow,ylim=c(0,length(slow)*3/4),xlim=xlim,breaks=breaks)
		hist(x=mid,ylim=c(0,length(mid)*3/4),xlim=xlim,breaks=breaks)
		hist(x=fast,ylim=c(0,length(fast)*3/4),xlim=xlim,breaks=breaks)
		dev.off()
	}
}

predict.lin <- function(comb,cols)
{
	pred<-rep(cols["intercept"],nrow(comb))
	
	for (col in intersect(names(comb),setdiff(names(cols),c("intercept")))) {
		pred <- pred + cols[col]*comb[,col]
	}
	return(data.frame("patient"=comb$patient,"prediction"=pred))
}

division <- function(comb)
{
	divisor <- nrow(comb)
	cat("Slow progressors:",length(which(comb$slope > -0.2535620))/divisor)
	cat("\n")
	cat("Medium progressors:",length(which(comb$slope <= -0.2535620 & comb$slope >= -1.0145556))/divisor)
	cat("\n")
	cat("Fast progressors:",length(which(comb$slope < -1.0145556))/divisor)
	cat("\n")
}

rvm.run <- function(comb,samp=297)
{
	s <- sample(1:nrow(comb),samp)
	ns <- setdiff(1:nrow(comb),s)
	r <- rvm(as.matrix(rm.cols(comb,c("slope","patient","onset.location"))[ns,]),comb$slope[ns],karg=list(sigma=0.525739))
	r.pred <- predict(r,as.matrix(rm.cols(comb,c("slope","patient","onset.location"))[s,]))
	return(score(comb$slope[s],r.pred))
}

lin.run <- function(comb,samp=.25)
{
	s <- sample(1:nrow(comb),samp*nrow(comb))
	ns <- setdiff(1:nrow(comb),s)
	cols <- lin.cols.select(comb[s,], comb[ns,],setdiff(names(comb),c("slope","patient","onset.location")),12)
	lin.cols.remove(comb[s,],comb[ns,],cols)
	return(cols)
}


selectcol <- function(col, cols, comb, combTrain, min, minCol) {
	coef <- generic.fit(combTrain,c(cols,col))$coefficients
	names(coef) <- c("intercept",c(cols,col))
	lin.pred <- predict.lin(comb,coef)
	comb <- merge(comb,lin.pred,by="patient",all.x=TRUE)
	res <- sqrt(sum((comb$prediction - comb$slope)^2)/nrow(comb))
	if (!is.na(res)&(res < min)) {
		min <- res
		minCol <- col
	}
	return(c(min,minCol))
}

lin.cols.select <- function(comb,combTrain,fromCols,newCols) {

	totalMin <- Inf
	min <- Inf
	minCol <- NA
	cols <- c()
	for (i in 1:newCols) {
		for (col in setdiff(fromCols,cols)) {
			res <- c(min,minCol)
			tryCatch(res <- selectcol(col, cols, comb, combTrain, min, minCol),error=function (e) 1)
			min <- res[[1]]
			minCol <- res[[2]]
		}
		
		if (!is.na(minCol)) {
			cols <- c(cols,minCol)
			print(paste("Adding column ", minCol, " with new score ", min))
		}
			
	}
	return(cols)
}

# colClasses: the values are character data, so that we can interpret numbers
# Change train.txt to test.txt for testing
print.expr("reading data", testdata <- read.csv("test.txt", header=TRUE, sep='|',col.names=c("patient","category","category_name","test_id","key","key_name","value"),colClasses = c("factor","factor","factor","factor","factor","factor","character")))

# Contains most of the data. Most of this is numeric, so that's good.
# J: we're not converting between different units for the same test, which is not so good.
print.expr("LABORATORY FORM", lab <- laboratory.form(testdata))
print.expr("removing small samples from LABORATORY FORM", lab <- remove.small.lab.samples(lab))
print.expr("cleaning LABORATORY FORM", lab <- clean.laboratory.form(lab))
print.expr("removing rest of small samples from LABORATORY FORM", lab <- remove.small.lab.samples(lab))
print.expr("converting units of the LABORATORY FORM", lab <- lab.convert.units(lab))

# In about half of the cases the sex is unknown, otherwise Male=1, Female=2
# Caucasian race: 886 of 918 = 96,5% is.
# J: Either race is not significant for ALS progression or we should exclude
# other races to remove potential bias.
print.expr("DEMOGRAPHY FORM", demography <- demographics.form(testdata))

# Main problem domain. In the first 90 days, there are about 3.5 data points per patient
# J: I don't know how to handle the difference between ALSF-R and ALSF... (tests 1-10 vs 1-9 + R1-3)
# J: if we play it safe, we could save 4 per patient.
print.expr("ALSFR FORM", als <- alsfr.form(testdata))

# Mainly textual values where the ALS began. Most of the data is discarded,
# one data point per patient is kept with onset in Limb or Bulbar, and how long the onset was
print.expr("ALS History", als_hist <- als.hist.form(testdata,demography))

# In the first 90 days, there are about 3.5 data points per patient
# Has a lot of NA values, there is some differentiation between
# test
# test standing or supine
# test baseline or end
# J: I can't find a pattern in when which type of data is recorded.
print.expr("VITAL FORM", vital <- vital.form(testdata))

# Only a value about subject liters (Trial 1). J: I don't know what it means.
#print.expr("VITAL SLOW FORM", vital_slow <- vital.slow.form(testdata))

# In addition to a value subject liters it also has a value subject normal. J: I don't know what they mean.
print.expr("VITAL FORCED FORM", vital_forced <- vital.forced.form(testdata))

# Contains only treatment$study.arm = PLACEBO/ACTIVE.
# J: we could approximate it with NA = 0, PLACEBO = 1, ACTIVE = 2?
#print.expr("TREATMENT FORM", treatment <- treatment.form(testdata))

# 198 data points, with too little information
# Most diseases occur with the mother or the father (probably because of recollection bias as well)
# J: We probably should ignore this.
#print.expr("FAMILY FORM", fam <- family.form(testdata))

# Don't need the raw data anymore
rm(testdata)

#print.expr("reading training slopes", testslopes <- read.csv("../training_slopes_m3-m12.txt", header=FALSE, sep=',',col.names=c("patient","slope"),colClasses = c("factor","numeric")))
print.expr("Combining forms into one", comb <- forms.combine(vital,vital_forced, als_hist,als, demography,lab))

comb$patient <- as.numeric(levels(comb$patient))[comb$patient]
comb <- na.to.mean(comb)

# Female
comb <- merge(comb, combF.predict(comb[comb$sex==2,]),by="patient",all.x=TRUE)

# Limb
comb <- merge(comb, combLmF.predict(comb[comb$onset.location=="Onset: Limb"&comb$sex!=2,]),by="patient",all.x=TRUE)
comb$prediction.x[is.na(comb$prediction.x)] <- comb$prediction.y[is.na(comb$prediction.x)]
comb <- data.frame(rm.cols(comb,c("prediction.x","prediction.y")),prediction=comb$prediction.x)

# Bulbar, Male
comb <- merge(comb, combBpM.predict(comb[comb$onset.location!="Onset: Limb"&comb$sex==1,]),by="patient",all.x=TRUE)
comb$prediction.x[is.na(comb$prediction.x)] <- comb$prediction.y[is.na(comb$prediction.x)]
comb <- data.frame(rm.cols(comb,c("prediction.x","prediction.y")),prediction=comb$prediction.x)

# Other
comb <- merge(comb, combBmMF.predict(comb[comb$onset.location!="Onset: Limb"&comb$sex!=1&comb$sex!=2,]),by="patient",all.x=TRUE)
comb$prediction.x[is.na(comb$prediction.x)] <- comb$prediction.y[is.na(comb$prediction.x)]
comb <- data.frame(rm.cols(comb,c("prediction.x","prediction.y")),prediction=comb$prediction.x)

#combTestExp <- data.frame(combTestExp,expslope=log(combTestExp$slope+10))

# combALS <- comb.select.disease(comb, "ALS")
# predALS <- combALS.predict(combALS)
#         (Intercept)                    comb$Total.x1             comb$diagnosis.delta               comb$Creatinine.x1               comb$Hemoglobin.x1  
#             0.133670                         2.906221                         0.001867                         0.374064                       -22.294547  
# comb$Lymphocytes.var                comb$Potassium.x1           comb$Triglycerides.var  comb$White.Blood.Cells..WBC..x0  
#            -0.176082                         1.272349                        -0.344356                         0.019038

# Values found by
# fitL <- combL.fit(combL)
# fitB <- combR.fit(combB)
# pruned <- (comb$patient %in% combALS$patient)
# combB <- comb.select.location(comb, c("Onset: Bulbar", "Onset: Limb and Bulbar"))
# predB <- combB.predict(combB)
# pruned <- pruned | (comb$patient %in% combB$patient)
# combL <- comb.select.location(comb, "Onset: Limb")
# predL <- combL.predict(combL)

# patients <- data.frame("patient"=comb$patient[order(comb$patient)])
# Merging limb prediction
# patients <- merge(patients,predL,by=c("patient"),all.x=TRUE)

# Merging Bulbar prediction
# patients <- merge(patients,predB,by=c("patient"),all.x=TRUE)
# patients$prediction.x[is.na(patients$prediction.x)] <- patients$prediction.y[is.na(patients$prediction.x)]
# patients <- data.frame("patient"=patients$patient, "prediction"=patients$prediction.x)

# Merging ALS prediction
# patients <- merge(patients,predALS,by=c("patient"),all.x=TRUE)
# patients$prediction.x[is.na(patients$prediction.x)] <- patients$prediction.y[is.na(patients$prediction.x)]
# patients <- data.frame("patient"=patients$patient, "prediction"=patients$prediction.x)

write.table(comb[order(comb$patient),c("patient","prediction")], file="predicted.out", append=FALSE, quote=TRUE, sep=",", col.names=FALSE, row.names=FALSE, qmethod="escape")
