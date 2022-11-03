
# downloaded from:
# file originally called "2021-03 - Analyses - Combine 2011 and 2019 - Share.R"

# ISSUES FOUND BY IAN
#
# Code does not reproduce the tables in the manuscript:
# To calculate median N and k studies, the variables nPS2011, nPS2012, nPS2013 and nPS2014 are filtered for data6$Social_PS==1 (e.g., line 214, 673)
# but no variable 'Social' is in the dataset. 
# If this is changed to 'Social_PS' then the tables can be reproduced. 
#
# Code and manuscript contain errors in calculating median sample sizes and K studies:
# Table 1 reports duplicates of psych science's median Ns and k studies for PSPB (e.g., line 299). this was due to repetitive code that wasn't corrected between lines.
# Table 2 repeats this error when calculating power from median Ns
# Table 3 repeats this error when calculating the FDR (I think, didn't deep dive this one as much)
#
# 
# Confusing/mislabeled code that produces correct results:
# Not an error in the manuscript but confusing in the code: the manuscript reports the False Discovery Rate (FP / [FP + TP]) and the code calculates this correctly 
# but it labels the variable the "fpr" (False Positive Rate, which is instead FP / [FP + TN]) rather than fdr.
#
# Stylistic choice:
# Rounding is done via R's round(), which produces weird behavior that most readers dont expect. 
# Results in manuscript can be reproduced using original code, but janitor::round_half_up() produces more intuitive behavior, but that's just my personal opinion.
# I only note this here because my refactoring of Fraley's code uses janitor::round_half_up() and therefore produces medians that differ by 1 in about 15% of cases in Table 1.




# Analyses for N-Pact 2011-2019
# Raw data are stored in two files. They'll be imported and merged here.

# Read data file from current directory
# Will need to set directory first

    data5 = read.csv("dataAKG.csv", stringsAsFactors=FALSE)	# AKG phase
    data4 = read.csv("data4.csv", stringsAsFactors=FALSE)	# JC phase


#=========================================================
# Install packages and import data
# Run once
#=========================================================


# Requires package "pwr" for power analyses
#    install.packages("pwr")
#    library(pwr)

# Colour pallete package
#    install.packages("viridis")
#    library(viridis)

#==========================================================
# Create something that combines data4 (JC) and data5 (AKG)
# try relabel the cols of JC data4
#==========================================================

	data3 = data4[,c("Index", "Random", "PDF", "PDF","Social","JPSP_Section",
	"Journal","Year","Title","Journal","TotalStudiesA","N1A","Design","CommentsA",
	"CoderA","TotalStudiesB","N1B","Design","CommentsB","CoderB","coder_dif","coder_disagree","ChrisResolve","CommentsChris",
	"Design","N","Design")]


	colnames(data3) = c("Index",          "Random" ,        "Target_PDF",     "PDF",            "Social_PS",     
	 "JPSP_Section",   "Journal",        "Year",           "Title",          "Journal_full",  
	 "num_studiesa",   "na",             "DA",             "commentsa",      "codera",        
	 "num_studiesb",   "nb",             "DB",             "commentsb",      "coderb" ,       
	 "coder_dif",      "coder_disagree", "ChrisResolve",   "ChrisNotes",     "ChrisD",        
	 "N" ,             "D"   )


	data6 = rbind(data3, data5)

	dim(data6)







#=========================================================
# Basic data aggregation and analyses
#=========================================================


# How many studies were coded? (Use !NA to calculate. TRUE is answer.)
# Answer: 4535  4540

	table(!is.na(data6$N))




# How many articles were coded? 
#   Cycle through each article that had a coded N
#   and increment the article counter by 1 when the PDF index
#   for the sample changes
# Answer: 1811  1812

	x = data6[!is.na(data6$N),]
	counter = 1 # error: because this counts *changes* in the index, N articles is changes + 1. Fixed by starting counter at 1 not 0.
	prev.x = 1
	for(i in 2:dim(x)[1]){
		this.x = x[i,4]
		if(prev.x != this.x){
			counter = counter + 1
		}
		prev.x = this.x
	}
	counter


# to simplify things, replace "" with NA for data6$D

	data6$D[which(data6$D=="")]=NA



# How many studies for each design type?
	
	#   b    m    w 
	#3168  975  397 
	# 4540

	table(data6$D)
	round(table(data6$D)/(table(!is.na(data6$N))[2]) ,2)
	sum(round(table(data6$D)/(table(!is.na(data6$N))[2]) ,2))
	# Express this as a percent
	# Answer:
	#0.70 0.21 0.09 

	# misc questions (e.g., JESP vs. JRP)

	temp = data6[data6$Journal=="JESP",]
	table(temp$D)/ (table(!is.na(temp$N))[2])

	temp = data6[data6$Journal=="JRP",]
	table(temp$D)
	table(temp$D)/ (table(!is.na(temp$N))[2])
		

# Some trouble-shooting on missing codes
# here for archival purposes. Not needed for code that follows.

	x = data6
	# about 89 cases where N exist but D does not
	table(is.na(x$N)==FALSE & is.na(x$D)==TRUE)

	# flag these
	x$doublecheck = 0
	x$doublecheck[is.na(x$N)==FALSE & is.na(x$D)==TRUE]=1
	table(x$doublecheck)

	# show case ids for flagged cases for further investigation
	x[x$doublecheck==1,c("Index","PDF","Year","Journal","doublecheck")]


	# 20 cases where N doesn't exist but design does
	table(is.na(x$N)==TRUE & is.na(x$D)==FALSE)


	# flag these
	x$doublecheck = 0
	x$doublecheck[is.na(x$N)==TRUE & is.na(x$D)==FALSE]=1
	table(x$doublecheck)

	# show case ids for flagged cases for further investigation
	x[x$doublecheck==1,c("Index","PDF","Year","Journal","doublecheck")]





#==================================================================================
# Table 1 
# N-pact Factors for each journal by year
# between-subject designs only. 
#==================================================================================



	nEJP2011 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nEJP2012 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nEJP2013 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nEJP2014 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)

	nEJSP2011 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nEJSP2012 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nEJSP2013 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nEJSP2014 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)

	nJESP2011 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nJESP2012 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nJESP2013 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nJESP2014 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)

	nJP2011 = median(data6$N[data6$Journal=="JP" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nJP2012 = median(data6$N[data6$Journal=="JP" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nJP2013 = median(data6$N[data6$Journal=="JP" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nJP2014 = median(data6$N[data6$Journal=="JP" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)

	nJPSP2011 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nJPSP2012 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nJPSP2013 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nJPSP2014 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)

	nJRP2011 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nJRP2012 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nJRP2013 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nJRP2014 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)

	nPS2011 = median(data6$N[data6$Journal=="PS" & data6$Year=="2011" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE) # error was on this line: was `data6$Social`
	nPS2012 = median(data6$N[data6$Journal=="PS" & data6$Year=="2012" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE) # error was on this line: was `data6$Social`
	nPS2013 = median(data6$N[data6$Journal=="PS" & data6$Year=="2013" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE) # error was on this line: was `data6$Social`
	nPS2014 = median(data6$N[data6$Journal=="PS" & data6$Year=="2014" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE) # error was on this line: was `data6$Social`

	nPSPB2011 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nPSPB2012 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nPSPB2013 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nPSPB2014 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)


	nSPPS2011 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2011" & data6$D=="b"], na.rm=TRUE)
	nSPPS2012 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2012" & data6$D=="b"], na.rm=TRUE)
	nSPPS2013 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2013" & data6$D=="b"], na.rm=TRUE)
	nSPPS2014 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2014" & data6$D=="b"], na.rm=TRUE)
	

	# newer

	nEJP2015 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nEJP2016 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nEJP2017 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nEJP2018 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nEJP2019 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)

	nEJSP2015 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nEJSP2016 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nEJSP2017 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nEJSP2018 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nEJSP2019 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)


	nJESP2015 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nJESP2016 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nJESP2017 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nJESP2018 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nJESP2019 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)


	nJP2015 = median(data6$N[data6$Journal=="JP" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nJP2016 = median(data6$N[data6$Journal=="JP" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nJP2017 = median(data6$N[data6$Journal=="JP" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nJP2018 = median(data6$N[data6$Journal=="JP" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nJP2019 = median(data6$N[data6$Journal=="JP" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)


	nJPSP2015 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nJPSP2016 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nJPSP2017 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nJPSP2018 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nJPSP2019 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)


	nJRP2015 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nJRP2016 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nJRP2017 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nJRP2018 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nJRP2019 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)

	nPS2015 = median(data6$N[data6$Journal=="PS" & data6$Year=="2015" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE)
	nPS2016 = median(data6$N[data6$Journal=="PS" & data6$Year=="2016" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE)
	nPS2017 = median(data6$N[data6$Journal=="PS" & data6$Year=="2017" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE)
	nPS2018 = median(data6$N[data6$Journal=="PS" & data6$Year=="2018" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE)
	nPS2019 = median(data6$N[data6$Journal=="PS" & data6$Year=="2019" & data6$Social_PS==1 & data6$D=="b"], na.rm=TRUE)

	nPSPB2015 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nPSPB2016 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nPSPB2017 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nPSPB2018 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nPSPB2019 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)


	nSPPS2015 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2015" & data6$D=="b"], na.rm=TRUE)
	nSPPS2016 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2016" & data6$D=="b"], na.rm=TRUE)
	nSPPS2017 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2017" & data6$D=="b"], na.rm=TRUE)
	nSPPS2018 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2018" & data6$D=="b"], na.rm=TRUE)
	nSPPS2019 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2019" & data6$D=="b"], na.rm=TRUE)



# original:
# 	tempC = rbind(
# 	c(nJRP2011,nJRP2012,nJRP2013,nJRP2014,nJRP2015,nJRP2016,nJRP2017,nJRP2018,nJRP2019),
# 	c(nJP2011,nJP2012,nJP2013,nJP2014,nJP2015,nJP2016,nJP2017,nJP2018,nJP2019),
#  	c(nJPSP2011,nJPSP2012,nJPSP2013,nJPSP2014,nJPSP2015,nJPSP2016,nJPSP2017,nJPSP2018,nJPSP2019),
# 	c(nPSPB2011,nPSPB2012,nPSPB2013,nPSPB2014,nPS2015,nPS2016,nPS2017,nPS2018,nPS2019), # NOTE 2015-2019 ARE THE PS NUMBERS NOT THE PSPB NUMBERS
# 	c(nPS2011,nPS2012,nPS2013,nPS2014,nPS2015,nPS2016,nPS2017,nPS2018,nPS2019),
# 	c(nJESP2011,nJESP2012,nJESP2013,nJESP2014,nJESP2015,nJESP2016,nJESP2017,nJESP2018,nJESP2019),
# 	c(nEJP2011,nEJP2012,nEJP2013,nEJP2014,nEJP2015,nEJP2016,nEJP2017,nEJP2018,nEJP2019),
# 	c(nEJSP2011,nEJSP2012,nEJSP2013,nEJSP2014,nEJSP2015,nEJSP2016,nEJSP2017,nEJSP2018,nEJSP2019),
# 	c(nSPPS2011,nSPPS2012,nSPPS2013,nSPPS2014,nSPPS2015,nSPPS2016,nSPPS2017,nSPPS2018,nSPPS2019)
# 	)
	
	# corrected:
	tempC = rbind(
	  c(nJRP2011,nJRP2012,nJRP2013,nJRP2014,nJRP2015,nJRP2016,nJRP2017,nJRP2018,nJRP2019),
	  c(nJP2011,nJP2012,nJP2013,nJP2014,nJP2015,nJP2016,nJP2017,nJP2018,nJP2019),
	  c(nJPSP2011,nJPSP2012,nJPSP2013,nJPSP2014,nJPSP2015,nJPSP2016,nJPSP2017,nJPSP2018,nJPSP2019),
	  c(nPSPB2011,nPSPB2012,nPSPB2013,nPSPB2014,nPSPB2015,nPSPB2016,nPSPB2017,nPSPB2018,nPSPB2019), 
	  c(nPS2011,nPS2012,nPS2013,nPS2014,nPS2015,nPS2016,nPS2017,nPS2018,nPS2019),
	  c(nJESP2011,nJESP2012,nJESP2013,nJESP2014,nJESP2015,nJESP2016,nJESP2017,nJESP2018,nJESP2019),
	  c(nEJP2011,nEJP2012,nEJP2013,nEJP2014,nEJP2015,nEJP2016,nEJP2017,nEJP2018,nEJP2019),
	  c(nEJSP2011,nEJSP2012,nEJSP2013,nEJSP2014,nEJSP2015,nEJSP2016,nEJSP2017,nEJSP2018,nEJSP2019),
	  c(nSPPS2011,nSPPS2012,nSPPS2013,nSPPS2014,nSPPS2015,nSPPS2016,nSPPS2017,nSPPS2018,nSPPS2019)
	)

	# Average test-retest stability for the six journals
	# that were coded in both the F&V2014 report and here
	# (exclude diagonal)
	# Answer: .62

	  tempC
	  round(cor(tempC),2)
	  z = cor(tempC)
	  diag(z) = NA
	  round(mean(z,na.rm=TRUE),2)	# Average test-retest stability 

	# Combine info into a frame
	  info2C= data.frame(tempC)
	  colnames(info2C) = c("2011","2012","2013","2014","2015","2016","2017","2018","2019")
	  info2C$journal = c("JRP","JP","JPSP","PSPB","PS:S","JESP","EJP","EJSP","SPPS")

	# overall npact (note: this is medians of medians. each year is weighted equally)
	  npact = apply(info2C[,1:9],1,median)
	  npact

  	  info2C$npact = apply(info2C[,1:9],1,median)	
	  info2C

	# Create Table 1, sorted by overall Npact

	  info2C[order(info2C[,11],  decreasing=TRUE), c(10,11,1:9)]


# Subsections, between-designs only and across years due to smaller cases
# JPSP sections
# social and non-social PS

	nJPSP_b_ASC  = median(data6$N[data6$Journal=="JPSP" & data6$D=="b" & data6$JPSP_Section=="Attitudes and Social Cognition"], na.rm=TRUE)
	nJPSP_b_IRGP = median(data6$N[data6$Journal=="JPSP" & data6$D=="b" & data6$JPSP_Section=="Interpersonal Relations and Group Processes"], na.rm=TRUE)
	nJPSP_b_PPID = median(data6$N[data6$Journal=="JPSP" & data6$D=="b" & data6$JPSP_Section=="Personality Processes and Individual Differences"], na.rm=TRUE)

	nJPSP_b_ASC
	nJPSP_b_IRGP
	nJPSP_b_PPID

	nPS2011_ns = median(data6$N[data6$Journal=="PS" & data6$D=="b" & data6$Social_PS==0], na.rm=TRUE)
	nPS2011_s = median(data6$N[data6$Journal=="PS" & data6$D=="b" & data6$Social_PS==1], na.rm=TRUE)

	nPS2011_ns
	nPS2011_s
	

	median(data6$N[data6$Journal=="PS" & data6$D=="b" & data6$Social_PS==0], na.rm=TRUE)
	median(data6$N[data6$Journal=="PS" & (data6$D=="w" | data6$D=="m")& data6$Social_PS==0], na.rm=TRUE)

	median(data6$N[data6$Journal=="PS" & data6$D=="b" & data6$Social_PS==1], na.rm=TRUE)
	median(data6$N[data6$Journal=="PS" & (data6$D=="w" | data6$D=="m")& data6$Social_PS==1], na.rm=TRUE)


# Create an updated version of Table 1 that includes the number of studies
# in each "cell"

	numberJRP2011 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2011" & data6$D=="b"]))
	numberJRP2012 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2012" & data6$D=="b"]))
	numberJRP2013 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2013" & data6$D=="b"]))
	numberJRP2014 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2014" & data6$D=="b"]))
	numberJRP2015 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2015" & data6$D=="b"]))
	numberJRP2016 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2016" & data6$D=="b"]))
	numberJRP2017 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2017" & data6$D=="b"]))
	numberJRP2018 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2018" & data6$D=="b"]))
	numberJRP2019 = sum(!is.na(data6$N[data6$Journal=="JRP" & data6$Year=="2019" & data6$D=="b"]))



	numberEJP2011 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2011" & data6$D=="b"]))
	numberEJP2012 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2012" & data6$D=="b"]))
	numberEJP2013 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2013" & data6$D=="b"]))
	numberEJP2014 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2014" & data6$D=="b"]))
	numberEJP2015 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2015" & data6$D=="b"]))
	numberEJP2016 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2016" & data6$D=="b"]))
	numberEJP2017 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2017" & data6$D=="b"]))
	numberEJP2018 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2018" & data6$D=="b"]))
	numberEJP2019 = sum(!is.na(data6$N[data6$Journal=="EJP" & data6$Year=="2019" & data6$D=="b"]))

	numberJP2011 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2011" & data6$D=="b"]))
	numberJP2012 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2012" & data6$D=="b"]))
	numberJP2013 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2013" & data6$D=="b"]))
	numberJP2014 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2014" & data6$D=="b"]))
	numberJP2015 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2015" & data6$D=="b"]))
	numberJP2016 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2016" & data6$D=="b"]))
	numberJP2017 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2017" & data6$D=="b"]))
	numberJP2018 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2018" & data6$D=="b"]))
	numberJP2019 = sum(!is.na(data6$N[data6$Journal=="JP" & data6$Year=="2019" & data6$D=="b"]))

	numberPSPB2011 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2011" & data6$D=="b"]))
	numberPSPB2012 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2012" & data6$D=="b"]))
	numberPSPB2013 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2013" & data6$D=="b"]))
	numberPSPB2014 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2014" & data6$D=="b"]))
	numberPSPB2015 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2015" & data6$D=="b"]))
	numberPSPB2016 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2016" & data6$D=="b"]))
	numberPSPB2017 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2017" & data6$D=="b"]))
	numberPSPB2018 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2018" & data6$D=="b"]))
	numberPSPB2019 = sum(!is.na(data6$N[data6$Journal=="PSPB" & data6$Year=="2019" & data6$D=="b"]))


	numberPS2011 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2011" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2012 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2012" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2013 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2013" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2014 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2014" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2015 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2015" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2016 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2016" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2017 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2017" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2018 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2018" & data6$D=="b" & data6$Social_PS==1 ]))
	numberPS2019 = sum(!is.na(data6$N[data6$Journal=="PS" & data6$Year=="2019" & data6$D=="b" & data6$Social_PS==1 ]))

	numberSPPS2011 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2011" & data6$D=="b"]))
	numberSPPS2012 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2012" & data6$D=="b"]))
	numberSPPS2013 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2013" & data6$D=="b"]))
	numberSPPS2014 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2014" & data6$D=="b"]))
	numberSPPS2015 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2015" & data6$D=="b"]))
	numberSPPS2016 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2016" & data6$D=="b"]))
	numberSPPS2017 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2017" & data6$D=="b"]))
	numberSPPS2018 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2018" & data6$D=="b"]))
	numberSPPS2019 = sum(!is.na(data6$N[data6$Journal=="SPPS" & data6$Year=="2019" & data6$D=="b"]))

	numberJPSP2011 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2011" & data6$D=="b"]))
	numberJPSP2012 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2012" & data6$D=="b"]))
	numberJPSP2013 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2013" & data6$D=="b"]))
	numberJPSP2014 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2014" & data6$D=="b"]))
	numberJPSP2015 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2015" & data6$D=="b"]))
	numberJPSP2016 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2016" & data6$D=="b"]))
	numberJPSP2017 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2017" & data6$D=="b"]))
	numberJPSP2018 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2018" & data6$D=="b"]))
	numberJPSP2019 = sum(!is.na(data6$N[data6$Journal=="JPSP" & data6$Year=="2019" & data6$D=="b"]))

	numberEJSP2011 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2011" & data6$D=="b"]))
	numberEJSP2012 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2012" & data6$D=="b"]))
	numberEJSP2013 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2013" & data6$D=="b"]))
	numberEJSP2014 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2014" & data6$D=="b"]))
	numberEJSP2015 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2015" & data6$D=="b"]))
	numberEJSP2016 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2016" & data6$D=="b"]))
	numberEJSP2017 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2017" & data6$D=="b"]))
	numberEJSP2018 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2018" & data6$D=="b"]))
	numberEJSP2019 = sum(!is.na(data6$N[data6$Journal=="EJSP" & data6$Year=="2019" & data6$D=="b"]))

	numberJESP2011 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2011" & data6$D=="b"]))
	numberJESP2012 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2012" & data6$D=="b"]))
	numberJESP2013 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2013" & data6$D=="b"]))
	numberJESP2014 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2014" & data6$D=="b"]))
	numberJESP2015 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2015" & data6$D=="b"]))
	numberJESP2016 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2016" & data6$D=="b"]))
	numberJESP2017 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2017" & data6$D=="b"]))
	numberJESP2018 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2018" & data6$D=="b"]))
	numberJESP2019 = sum(!is.na(data6$N[data6$Journal=="JESP" & data6$Year=="2019" & data6$D=="b"]))


	
# original:
# 	tempCnumber = rbind(
# 	c(numberJRP2011,  numberJRP2012,  numberJRP2013,  numberJRP2014,  numberJRP2015,  numberJRP2016,  numberJRP2017,  numberJRP2018,  numberJRP2019),
# 	c(numberJP2011,   numberJP2012,   numberJP2013,   numberJP2014,   numberJP2015,   numberJP2016,   numberJP2017,   numberJP2018,   numberJP2019),
#  	c(numberJPSP2011, numberJPSP2012, numberJPSP2013, numberJPSP2014, numberJPSP2015, numberJPSP2016, numberJPSP2017, numberJPSP2018, numberJPSP2019),
# 	c(numberPSPB2011, numberPSPB2012, numberPSPB2013, numberPSPB2014, numberPS2015,   numberPS2016,   numberPS2017,   numberPS2018,   numberPS2019),  # NOTE 2015-2019 ARE THE PS NUMBERS NOT THE PSPB NUMBERS
# 	c(numberPS2011,   numberPS2012,   numberPS2013,   numberPS2014,   numberPS2015,   numberPS2016,   numberPS2017,   numberPS2018,   numberPS2019),
# 	c(numberJESP2011, numberJESP2012, numberJESP2013, numberJESP2014, numberJESP2015, numberJESP2016, numberJESP2017, numberJESP2018, numberJESP2019),
# 	c(numberEJP2011,  numberEJP2012,  numberEJP2013,  numberEJP2014,  numberEJP2015,  numberEJP2016,  numberEJP2017,  numberEJP2018,  numberEJP2019),
# 	c(numberEJSP2011, numberEJSP2012, numberEJSP2013, numberEJSP2014, numberEJSP2015, numberEJSP2016, numberEJSP2017, numberEJSP2018, numberEJSP2019),
# 	c(numberSPPS2011, numberSPPS2012, numberSPPS2013, numberSPPS2014, numberSPPS2015, numberSPPS2016, numberSPPS2017, numberSPPS2018, numberSPPS2019)
# 	)
	
# corrected:
	tempCnumber = rbind(
	  c(numberJRP2011,  numberJRP2012,  numberJRP2013,  numberJRP2014,  numberJRP2015,  numberJRP2016,  numberJRP2017,  numberJRP2018,  numberJRP2019),
	  c(numberJP2011,   numberJP2012,   numberJP2013,   numberJP2014,   numberJP2015,   numberJP2016,   numberJP2017,   numberJP2018,   numberJP2019),
	  c(numberJPSP2011, numberJPSP2012, numberJPSP2013, numberJPSP2014, numberJPSP2015, numberJPSP2016, numberJPSP2017, numberJPSP2018, numberJPSP2019),
	  c(numberPSPB2011, numberPSPB2012, numberPSPB2013, numberPSPB2014, numberPSPB2015, numberPSPB2016, numberPSPB2017, numberPSPB2018, numberPSPB2019), 
	  c(numberPS2011,   numberPS2012,   numberPS2013,   numberPS2014,   numberPS2015,   numberPS2016,   numberPS2017,   numberPS2018,   numberPS2019),
	  c(numberJESP2011, numberJESP2012, numberJESP2013, numberJESP2014, numberJESP2015, numberJESP2016, numberJESP2017, numberJESP2018, numberJESP2019),
	  c(numberEJP2011,  numberEJP2012,  numberEJP2013,  numberEJP2014,  numberEJP2015,  numberEJP2016,  numberEJP2017,  numberEJP2018,  numberEJP2019),
	  c(numberEJSP2011, numberEJSP2012, numberEJSP2013, numberEJSP2014, numberEJSP2015, numberEJSP2016, numberEJSP2017, numberEJSP2018, numberEJSP2019),
	  c(numberSPPS2011, numberSPPS2012, numberSPPS2013, numberSPPS2014, numberSPPS2015, numberSPPS2016, numberSPPS2017, numberSPPS2018, numberSPPS2019)
	)

	# Combine info into a frame
	  info2Cnumber= data.frame(tempCnumber)
	  
	  min(tempCnumber)
	  max(tempCnumber)
 	  sum(tempCnumber)

	# Create Table 1, sorted by overall Npact

	  
	  info2CC = info2C
	  info2CC = cbind(info2CC,info2Cnumber)
	  info2CC = info2CC[order(info2CC[,11],  decreasing=TRUE),]
	  info2CC[,c(1:9,11:20)]=round(info2CC[,c(1:9,11:20)])


	for(i in 1:9){
		cat(info2CC[i,"journal"]   ,",",   info2CC[i,"npact"],",",
		info2CC[i,"2011"]," (",info2CC[i,"X1"],"),",
		info2CC[i,"2012"]," (",info2CC[i,"X2"],"),",
		info2CC[i,"2013"]," (",info2CC[i,"X3"],"),",
		info2CC[i,"2014"]," (",info2CC[i,"X4"],"),",
		info2CC[i,"2015"]," (",info2CC[i,"X5"],"),",
		info2CC[i,"2016"]," (",info2CC[i,"X6"],"),",
		info2CC[i,"2017"]," (",info2CC[i,"X7"],"),",
		info2CC[i,"2018"]," (",info2CC[i,"X8"],"),",
		info2CC[i,"2019"]," (",info2CC[i,"X9"],")",
		"\n", sep="")
	}


#=========================================================
# Table 2. Find power to detect a correlation of .21 
# Using between-persons designs for power only
# Use the "pwr" package
# Add these results to the info object
#=========================================================

	info2C$power20_2011 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2011"])$power , 2)
	info2C$power20_2012 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2012"])$power , 2)
	info2C$power20_2013 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2013"])$power , 2)
	info2C$power20_2014 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2014"])$power , 2)
	info2C$power20_2015 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2015"])$power , 2)
	info2C$power20_2016 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2016"])$power , 2)
	info2C$power20_2017 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2017"])$power , 2)
	info2C$power20_2018 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2018"])$power , 2)
	info2C$power20_2019 = round( pwr.r.test(r = .2, sig.level = .05,n=info2C[,"2019"])$power , 2)

	info2C

	# Table 2 Info - Power

	  info2C[order(info2C[,11],  decreasing=TRUE), c(10,12:20)]






#=========================================================
# Estimate False Positive Rates for journals
# based on the estimated statistical power of a typical study to detect rho = .20
# (power based on between-person designs only)
# and various assumptions about the P(null = TRUE)
#=========================================================

# power<-c(.77,.63,.49,.48,.46,.40,.43,.49,.60) (from Fraley and Vazire 2014 paper)


  # 2011

	# Base rate of null = 80%

	baserate_null = .80
	power = info2C$power20_2011

	r =  1-baserate_null
	fpr = (baserate_null*.05) / (  (baserate_null*.05) + ((1-baserate_null)*power)  )

	round(fpr,2)
	info2C$fpr80_2011 = round(fpr,2)

	# Baserate of null is 50%

	baserate_null = .50
	r =  1-baserate_null
	fpr = (baserate_null*.05) / (  (baserate_null*.05) + ((1-baserate_null)*power)  )

	round(fpr,2)

	info2C$fpr50_2011 = round(fpr,2)
	info2C

  # 2019

	# Base rate of null = 80%

	baserate_null = .80
	power = info2C$power20_2019

	r =  1-baserate_null
	fpr = (baserate_null*.05) / (  (baserate_null*.05) + ((1-baserate_null)*power)  )

	round(fpr,2)
	info2C$fpr80_2019 = round(fpr,2)

	# Baserate of null is 50%

	baserate_null = .50
	r =  1-baserate_null
	fpr = (baserate_null*.05) / (  (baserate_null*.05) + ((1-baserate_null)*power)  )

	round(fpr,2)

	info2C$fpr50_2019 = round(fpr,2)
	info2C



  # note to self: expected FDR with baserate of .80 even with perfect power
	baserate_null = .80
	power = 1
	fpr = (baserate_null*.05) / (  (baserate_null*.05) + ((1-baserate_null)*power)  )
	fpr


  # Table 3 Info - FPR

	  info2C[order(info2C[,11],  decreasing=TRUE), c(10,c(22,21,24,23))]







#=========================================================
# Figure 2. Change over time
# Examine median sample size by year, within journals
# Not limited to between-only designs
#=========================================================




	nEJP2011 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2011"], na.rm=TRUE)
	nEJP2012 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2012"], na.rm=TRUE)
	nEJP2013 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2013"], na.rm=TRUE)
	nEJP2014 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2014"], na.rm=TRUE)

	nEJSP2011 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2011"], na.rm=TRUE)
	nEJSP2012 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2012"], na.rm=TRUE)
	nEJSP2013 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2013"], na.rm=TRUE)
	nEJSP2014 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2014"], na.rm=TRUE)

	nJESP2011 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2011"], na.rm=TRUE)
	nJESP2012 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2012"], na.rm=TRUE)
	nJESP2013 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2013"], na.rm=TRUE)
	nJESP2014 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2014"], na.rm=TRUE)

	nJP2011 = median(data6$N[data6$Journal=="JP" & data6$Year=="2011"], na.rm=TRUE)
	nJP2012 = median(data6$N[data6$Journal=="JP" & data6$Year=="2012"], na.rm=TRUE)
	nJP2013 = median(data6$N[data6$Journal=="JP" & data6$Year=="2013"], na.rm=TRUE)
	nJP2014 = median(data6$N[data6$Journal=="JP" & data6$Year=="2014"], na.rm=TRUE)

	nJPSP2011 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2011"], na.rm=TRUE)
	nJPSP2012 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2012"], na.rm=TRUE)
	nJPSP2013 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2013"], na.rm=TRUE)
	nJPSP2014 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2014"], na.rm=TRUE)

	nJRP2011 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2011"], na.rm=TRUE)
	nJRP2012 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2012"], na.rm=TRUE)
	nJRP2013 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2013"], na.rm=TRUE)
	nJRP2014 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2014"], na.rm=TRUE)

	nPS2011 = median(data6$N[data6$Journal=="PS" & data6$Year=="2011" & data6$Social_PS==1], na.rm=TRUE) # error was on this line; was `data6$Social`
	nPS2012 = median(data6$N[data6$Journal=="PS" & data6$Year=="2012" & data6$Social_PS==1], na.rm=TRUE) # error was on this line; was `data6$Social`
	nPS2013 = median(data6$N[data6$Journal=="PS" & data6$Year=="2013" & data6$Social_PS==1], na.rm=TRUE) # error was on this line; was `data6$Social`
	nPS2014 = median(data6$N[data6$Journal=="PS" & data6$Year=="2014" & data6$Social_PS==1], na.rm=TRUE) # error was on this line; was `data6$Social`

	nPSPB2011 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2011"], na.rm=TRUE)
	nPSPB2012 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2012"], na.rm=TRUE)
	nPSPB2013 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2013"], na.rm=TRUE)
	nPSPB2014 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2014"], na.rm=TRUE)


	nSPPS2011 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2011"], na.rm=TRUE)
	nSPPS2012 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2012"], na.rm=TRUE)
	nSPPS2013 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2013"], na.rm=TRUE)
	nSPPS2014 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2014"], na.rm=TRUE)

	# a = all
	nPS2011A = median(data6$N[data6$Journal=="PS" & data6$Year=="2011" ], na.rm=TRUE)
	nPS2012A = median(data6$N[data6$Journal=="PS" & data6$Year=="2012" ], na.rm=TRUE)
	nPS2013A = median(data6$N[data6$Journal=="PS" & data6$Year=="2013" ], na.rm=TRUE)
	nPS2014A = median(data6$N[data6$Journal=="PS" & data6$Year=="2014" ], na.rm=TRUE)

# newer

	nEJP2015 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2015"], na.rm=TRUE)
	nEJP2016 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2016"], na.rm=TRUE)
	nEJP2017 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2017"], na.rm=TRUE)
	nEJP2018 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2018"], na.rm=TRUE)
	nEJP2019 = median(data6$N[data6$Journal=="EJP" & data6$Year=="2019"], na.rm=TRUE)

	nEJSP2015 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2015"], na.rm=TRUE)
	nEJSP2016 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2016"], na.rm=TRUE)
	nEJSP2017 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2017"], na.rm=TRUE)
	nEJSP2018 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2018"], na.rm=TRUE)
	nEJSP2019 = median(data6$N[data6$Journal=="EJSP" & data6$Year=="2019"], na.rm=TRUE)


	nJESP2015 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2015"], na.rm=TRUE)
	nJESP2016 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2016"], na.rm=TRUE)
	nJESP2017 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2017"], na.rm=TRUE)
	nJESP2018 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2018"], na.rm=TRUE)
	nJESP2019 = median(data6$N[data6$Journal=="JESP" & data6$Year=="2019"], na.rm=TRUE)


	nJP2015 = median(data6$N[data6$Journal=="JP" & data6$Year=="2015"], na.rm=TRUE)
	nJP2016 = median(data6$N[data6$Journal=="JP" & data6$Year=="2016"], na.rm=TRUE)
	nJP2017 = median(data6$N[data6$Journal=="JP" & data6$Year=="2017"], na.rm=TRUE)
	nJP2018 = median(data6$N[data6$Journal=="JP" & data6$Year=="2018"], na.rm=TRUE)
	nJP2019 = median(data6$N[data6$Journal=="JP" & data6$Year=="2019"], na.rm=TRUE)


	nJPSP2015 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2015"], na.rm=TRUE)
	nJPSP2016 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2016"], na.rm=TRUE)
	nJPSP2017 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2017"], na.rm=TRUE)
	nJPSP2018 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2018"], na.rm=TRUE)
	nJPSP2019 = median(data6$N[data6$Journal=="JPSP" & data6$Year=="2019"], na.rm=TRUE)


	nJRP2015 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2015"], na.rm=TRUE)
	nJRP2016 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2016"], na.rm=TRUE)
	nJRP2017 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2017"], na.rm=TRUE)
	nJRP2018 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2018"], na.rm=TRUE)
	nJRP2019 = median(data6$N[data6$Journal=="JRP" & data6$Year=="2019"], na.rm=TRUE)

	nPS2015 = median(data6$N[data6$Journal=="PS" & data6$Year=="2015" & data6$Social_PS==1], na.rm=TRUE)
	nPS2016 = median(data6$N[data6$Journal=="PS" & data6$Year=="2016" & data6$Social_PS==1], na.rm=TRUE)
	nPS2017 = median(data6$N[data6$Journal=="PS" & data6$Year=="2017" & data6$Social_PS==1], na.rm=TRUE)
	nPS2018 = median(data6$N[data6$Journal=="PS" & data6$Year=="2018" & data6$Social_PS==1], na.rm=TRUE)
	nPS2019 = median(data6$N[data6$Journal=="PS" & data6$Year=="2019" & data6$Social_PS==1], na.rm=TRUE)

	nPSPB2015 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2015"], na.rm=TRUE)
	nPSPB2016 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2016"], na.rm=TRUE)
	nPSPB2017 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2017"], na.rm=TRUE)
	nPSPB2018 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2018"], na.rm=TRUE)
	nPSPB2019 = median(data6$N[data6$Journal=="PSPB" & data6$Year=="2019"], na.rm=TRUE)


	nSPPS2015 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2015"], na.rm=TRUE)
	nSPPS2016 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2016"], na.rm=TRUE)
	nSPPS2017 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2017"], na.rm=TRUE)
	nSPPS2018 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2018"], na.rm=TRUE)
	nSPPS2019 = median(data6$N[data6$Journal=="SPPS" & data6$Year=="2019"], na.rm=TRUE)

	# a = all
	nPS2015A = median(data6$N[data6$Journal=="PS" & data6$Year=="2015" ], na.rm=TRUE)
	nPS2016A = median(data6$N[data6$Journal=="PS" & data6$Year=="2016" ], na.rm=TRUE)
	nPS2017A = median(data6$N[data6$Journal=="PS" & data6$Year=="2017" ], na.rm=TRUE)
	nPS2018A = median(data6$N[data6$Journal=="PS" & data6$Year=="2018" ], na.rm=TRUE)
	nPS2019A = median(data6$N[data6$Journal=="PS" & data6$Year=="2019" ], na.rm=TRUE)


	# Add data from Fraley and Vazire (2014)

	nJP2006 = 211
	nJP2007 = 160
	nJP2008 = 162
	nJP2009 = 184.5
	nJP2010 = 173

	nJRP2006 = 81
	nJRP2007 = 126
	nJRP2008 = 165
	nJRP2009 = 133
	nJRP2010 = 140

	nPSPB2006 = 112
	nPSPB2007 = 86
	nPSPB2008 = 89
	nPSPB2009 = 96.5
	nPSPB2010 = 89.5

	nJPSP2006 = 80
	nJPSP2007 = 86.5
	nJPSP2008 = 93
	nJPSP2009 = 95
	nJPSP2010 = 96

	nJESP2006 = 114
	nJESP2007 = 55.5
	nJESP2008 = 88
	nJESP2009 = 77
	nJESP2010 = 98

	nPS2006 = 67
	nPS2007 = 91
	nPS2008 = 51.5
	nPS2009 = 78.5
	nPS2010 = 76
	
	Year = c(2011,2012,2013,2014,2015,2016,2017,2018,2019)						# Years for this study
	Years = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)	# All years, combining previous study


	# Graph NF from year-to-year as a function of journal

	par(pty="m", mar=c(5,5,5,5))
	plot(Years, c(nPS2006,nPS2007,nPS2008,nPS2009,nPS2010,nPS2011,nPS2012,nPS2013,nPS2014,
	nPS2015,nPS2016,nPS2017,nPS2018,nPS2019),
		ylim=c(50,560),type="n",ylab="Median N",xlab="Year", xlim=c(2005, 2020),
		cex.lab=1.4, cex.axis=1.3, axes=FALSE)
	axis(1, cex.axis=1.3)
	axis(2, cex.axis=1.3)



	k = 3
	z = plasma(11)
	lines(Years, c(nJRP2006,nJRP2007,nJRP2008,nJRP2009,nJRP2010,nJRP2011,nJRP2012,nJRP2013,nJRP2014,nJRP2015,nJRP2016,nJRP2017,nJRP2018,nJRP2019),col=z[1], lwd=k, type="b", pch=1)
	lines(Years, c(nJP2006,nJP2007,nJP2008,nJP2009,nJP2010,nJP2011,nJP2012,nJP2013,nJP2014,nJP2015,nJP2016,nJP2017,nJP2018,nJP2019),col=z[2], lwd=k, type="b", pch=2)
	lines(Year, c(nEJP2011,nEJP2012,nEJP2013,nEJP2014,nEJP2015,nEJP2016,nEJP2017,nEJP2018,nEJP2019),col=z[3], lwd=k, type="b", pch=3)
	lines(Year, c(nSPPS2011,nSPPS2012,nSPPS2013,nSPPS2014,nSPPS2015,nSPPS2016,nSPPS2017,nSPPS2018,nSPPS2019), lwd=k,col=z[4], type="b", pch=4)
	lines(Years, c(nJPSP2006,nJPSP2007,nJPSP2008,nJPSP2009,nJPSP2010,nJPSP2011,nJPSP2012,nJPSP2013,nJPSP2014,nJPSP2015,nJPSP2016,nJPSP2017,nJPSP2018,nJPSP2019), lwd=k,col=z[5], type="b", pch=5)
	lines(Years, c(nPSPB2006,nPSPB2007,nPSPB2008,nPSPB2009,nPSPB2010,nPSPB2011,nPSPB2012,nPSPB2013,nPSPB2014,nPSPB2015,nPSPB2016,nPSPB2017,nPSPB2018,nPSPB2019), lwd=k,col=z[6], type="b", pch=6)

	# Need to use ALL PS (social vs. not) to make this comparable to previous years
	# correction: The 2014 numbers are based on SP only. Remove this line below.
	# lines(Years, c(nPS2006,nPS2007,nPS2008,nPS2009,nPS2010,nPS2011A,nPS2012A,nPS2013A,nPS2014A) , lwd=k,col=z[7], type="b", pch=7)
	lines(Years, c(nPS2006,nPS2007,nPS2008,nPS2009,nPS2010,nPS2011,nPS2012,nPS2013,nPS2014,nPS2015,nPS2016,nPS2017,nPS2018,nPS2019) , lwd=k,col=z[7], type="b", pch=7)


	lines(Year, c(nEJSP2011,nEJSP2012,nEJSP2013,nEJSP2014,nEJSP2015,nEJSP2016,nEJSP2017,nEJSP2018,nEJSP2019), lwd=k,col=z[8], type="b", pch=8)
	lines(Years, c(nJESP2006,nJESP2007,nJESP2008,nJESP2009,nJESP2010,nJESP2011,nJESP2012,nJESP2013,nJESP2014,nJESP2015,nJESP2016,nJESP2017,nJESP2018,nJESP2019),col=z[9], lwd=k, type="b", pch=9)


	# right-hand labels
	k = .0	# jitter
	cexk = 1.2	# text size
	text(2019.1, nJRP2019, labels="JRP", cex=cexk,col=z[1], pos=4)
	text(2019.1, nJP2019, labels="JP", cex=cexk,col=z[2], pos=4)
	text(2019.1, nEJP2019 + 10, labels="EJP", cex=cexk,col=z[3], pos=4)
	text(2019.1, nSPPS2019, labels="SPPS", cex=cexk,col=z[4], pos=4)
	
	text(2019.1, nJPSP2019 + 20, labels="JPSP", cex=cexk,col=z[5], pos=4)

	text(2019.1, nPS2019 -10 , labels="PS:S", cex=cexk,col=z[7], pos=4)
	text(2019.1, nJESP2019 - 0, labels="JESP", cex=cexk,col=z[9], pos=4)
	text(2019.1, nPSPB2019 - 0, labels="PSPB", cex=cexk,col=z[6], pos=4)
	text(2019.1, nEJSP2019 -20, labels="EJSP", cex=cexk,col=z[8], pos=4)











# Create Figure 3: A version of the previous graph but plotting power estimates (rho = .20) instead of N

	x1=c(nJRP2006,nJRP2007,nJRP2008,nJRP2009,nJRP2010,nJRP2011,nJRP2012,nJRP2013,nJRP2014,nJRP2015,nJRP2016,nJRP2017,nJRP2018,nJRP2019)
	x2=c(nJP2006,nJP2007,nJP2008,nJP2009,nJP2010,nJP2011,nJP2012,nJP2013,nJP2014,nJP2015,nJP2016,nJP2017,nJP2018,nJP2019)
	x3=c(nEJP2011,nEJP2012,nEJP2013,nEJP2014,nEJP2015,nEJP2016,nEJP2017,nEJP2018,nEJP2019)
	x4=c(nSPPS2011,nSPPS2012,nSPPS2013,nSPPS2014,nSPPS2015,nSPPS2016,nSPPS2017,nSPPS2018,nSPPS2019)
	x5=c(nJPSP2006,nJPSP2007,nJPSP2008,nJPSP2009,nJPSP2010,nJPSP2011,nJPSP2012,nJPSP2013,nJPSP2014,nJPSP2015,nJPSP2016,nJPSP2017,nJPSP2018,nJPSP2019)
	x6=c(nPSPB2006,nPSPB2007,nPSPB2008,nPSPB2009,nPSPB2010,nPSPB2011,nPSPB2012,nPSPB2013,nPSPB2014,nPSPB2015,nPSPB2016,nPSPB2017,nPSPB2018,nPSPB2019)
	x7=c(nPS2006,nPS2007,nPS2008,nPS2009,nPS2010,nPS2011,nPS2012,nPS2013,nPS2014,nPS2015,nPS2016,nPS2017,nPS2018,nPS2019)
	x8=c(nEJSP2011,nEJSP2012,nEJSP2013,nEJSP2014,nEJSP2015,nEJSP2016,nEJSP2017,nEJSP2018,nEJSP2019)
	x9=c(nJESP2006,nJESP2007,nJESP2008,nJESP2009,nJESP2010,nJESP2011,nJESP2012,nJESP2013,nJESP2014,nJESP2015,nJESP2016,nJESP2017,nJESP2018,nJESP2019)


	JRPpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x1 )$power , 2)
	JPpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x2 )$power , 2)
	EJPpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x3 )$power , 2)
	SPPSpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x4 )$power , 2)
	JPSPpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x5 )$power , 2)
	PSPBpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x6 )$power , 2)
	PSpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x7 )$power , 2)
	EJSPpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x8 )$power , 2)
	JESPpower20 = round(pwr.r.test(r = .2, sig.level = .05, n=x9 )$power , 2)





	par(pty="m", mar=c(5,5,5,5))
	plot(Years, c(nPS2006,nPS2007,nPS2008,nPS2009,nPS2010,nPS2011,nPS2012,nPS2013,nPS2014,
	nPS2015,nPS2016,nPS2017,nPS2018,nPS2019),
		ylim=c(.2,1),type="n",ylab="Power",xlab="Year", xlim=c(2005, 2020),
		cex.lab=1.4, cex.axis=1.3, axes=FALSE)
	axis(1, cex.axis=1.3)
	axis(2, cex.axis=1.3)
	abline(h=.80, lty=2)

	k = 3
	z = plasma(11)
	lines(Years, JRPpower20 ,col=z[1], lwd=k, type="b", pch=1)
	lines(Years, JPpower20 ,col=z[2], lwd=k, type="b", pch=2)
	lines(Year, EJPpower20 ,col=z[3], lwd=k, type="b", pch=3)
	lines(Year, SPPSpower20 , lwd=k,col=z[4], type="b", pch=4)
	lines(Years, JPSPpower20 , lwd=k,col=z[5], type="b", pch=5)
	lines(Years, PSPBpower20 , lwd=k,col=z[6], type="b", pch=6)
	lines(Years, PSpower20 , lwd=k,col=z[7], type="b", pch=7)
	lines(Year, EJSPpower20 , lwd=k,col=z[8], type="b", pch=8)
	lines(Years, JESPpower20 ,col=z[9], lwd=k, type="b", pch=9)




	# right-hand labels
	k = .0	# jitter
	cexk = 1.2	# text size
	text(2019.1, tail(JRPpower20, n = 1) +.02 , labels="JRP", cex=cexk,col=z[1], pos=4)
	text(2019.1, tail(EJPpower20, n = 1) - .01, labels="EJP", cex=cexk,col=z[3], pos=4)
	text(2019.1, tail(SPPSpower20, n = 1)-.02 , labels="SPPS", cex=cexk,col=z[4], pos=4)
	text(2019.1, tail(JPpower20, n = 1) - .03 , labels="JP", cex=cexk,col=z[2], pos=4)
	

	text(2019.1, tail(JESPpower20, n = 1) + .02, labels="JESP", cex=cexk,col=z[9], pos=4)
	text(2019.1, tail(JPSPpower20, n = 1) + .00, labels="JPSP", cex=cexk,col=z[5], pos=4)
	text(2019.1, tail(PSpower20, n = 1) - .02 , labels="PS:S", cex=cexk,col=z[7], pos=4)


	text(2019.1, tail(PSPBpower20, n = 1), labels="PSPB", cex=cexk,col=z[6], pos=4)
	text(2019.1, tail(EJSPpower20, n = 1)-.02, labels="EJSP", cex=cexk,col=z[8], pos=4)








#=========================================================
# Quantify change over time using linear growth curves for each journal
# report intercepts and slopes
# Not included in final paper, based on review process
#=========================================================

	temp = rbind(
	c(nJRP2011,nJRP2012,nJRP2013,nJRP2014,nJRP2015,nJRP2016,nJRP2017,nJRP2018,nJRP2019),
	c(nJP2011,nJP2012,nJP2013,nJP2014,nJP2015,nJP2016,nJP2017,nJP2018,nJP2019),
	c(nEJP2011,nEJP2012,nEJP2013,nEJP2014,nEJP2015,nEJP2016,nEJP2017,nEJP2018,nEJP2019),
 	c(nJPSP2011,nJPSP2012,nJPSP2013,nJPSP2014,nJPSP2015,nJPSP2016,nJPSP2017,nJPSP2018,nJPSP2019),
	c(nSPPS2011,nSPPS2012,nSPPS2013,nSPPS2014,nSPPS2015,nSPPS2016,nSPPS2017,nSPPS2018,nSPPS2019),
	c(nPSPB2011,nPSPB2012,nPSPB2013,nPSPB2014,nPSPB2015,nPSPB2016,nPSPB2017,nPSPB2018,nPSPB2019), 
	c(nEJSP2011,nEJSP2012,nEJSP2013,nEJSP2014,nEJSP2015,nEJSP2016,nEJSP2017,nEJSP2018,nEJSP2019), 
	c(nPS2011,nPS2012,nPS2013,nPS2014,nPS2015,nPS2016,nPS2017,nPS2018,nPS2019),
	c(nJESP2011,nJESP2012,nJESP2013,nJESP2014,nJESP2015,nJESP2016,nJESP2017,nJESP2018,nJESP2019)
	)

	# Average test-retest stability for journals 2011-2019
	# Answer: .65

	
	temp
	round(cor(temp),2)
	z = cor(temp)
	diag(z) = NA
	round(mean(z,na.rm=TRUE),2)	# Average test-retest stability 



	# Create a dataframe to hold NF by year and int/slope info below
	# These are the NFs for the Figure, not from Table 1. Thus,
	# ALL designs (not just between) are included

	info2 = data.frame(temp)
	colnames(info2) = c("2011","2012","2013","2014","2015","2016","2017","2018","2019")
	info2$journal = c("JRP","JP","EJP","JPSP","SPPS","PSPB","EJSP","PS:S","JESP")
	cor(info2[,1:9])
	
	info2

	# Find intercept and slope of simple linear growth for each journal
	# Note: Coding time such that 0 = 2011

	info2$int = NA
	info2$slope = NA
	info2$intSE = NA
	info2$slopeSE = NA


	for(i in 1:9){
	  info2$int[i] = round(lm(as.numeric(info2[i,1:9])~c(0,1,2,3,4,5,6,7,8))$coef[1],2)
	  info2$slope[i] = round(lm(as.numeric(info2[i,1:9])~c(0,1,2,3,4,5,6,7,8))$coef[2],2)

	  # save SEs
	  info2$intSE[i] = round(summary(lm(as.numeric(info2[i,1:9])~c(0,1,2,3,4,5,6,7,8)))$coef[1,2],2)
	  info2$slopeSE[i] = round(summary(lm(as.numeric(info2[i,1:9])~c(0,1,2,3,4,5,6,7,8)))$coef[2,2],2)
	
	}
	
	# cor of int and slope across journals
	# typically neg. 
	# Answer: -.15 

		cor(info2$int, info2$slope)

	# average int and slope

		mean(info2$int)
		mean(info2$slope)

	# Table of intercepts and slopes for 2011-2019

	info2[order(info2$slope),10:12]

	info2[order(info2$slope),c(10, 11, 12)] 	# ascending by slope
	info2[order(-info2$int),c(10, 11, 12)] 	# descending by int

	info2[order(-info2$slope),c(10,11,12)] 	# descending by slope

	info2[order(-info2$slope),c(10,11,13, 12, 14)] 	# descending by slope with SEs





# How does the median N at PS for between-person studies compare
# to that in within-person or mixed studies?
# Social/Personality articles only

	median(data6$N[data6$Journal=="PS"  & (data6$D=="w" | data6$D=="m") & data6$Social_PS==1], na.rm=TRUE)
	median(data6$N[data6$Journal=="PS"  & (data6$D=="b" ) & data6$Social_PS==1], na.rm=TRUE)

# How does the meadian N at PS for between-person studies compare
# to that in within-person or mixed studies?
# Non-S/P articles

	median(data6$N[data6$Journal=="PS"  & (data6$D=="w" | data6$D=="m") & data6$Social_PS==0], na.rm=TRUE)
	median(data6$N[data6$Journal=="PS"  & (data6$D=="b" ) & data6$Social_PS==0], na.rm=TRUE)

# How does N differ among designs, excluding Psychological Science

	dim(data6[data6$D=="b",] )
	median(data6[data6$D=="b" & data6$Journal!="PS", "N"], na.rm=TRUE)
	median(data6[data6$D=="w" & data6$Journal!="PS", "N"], na.rm=TRUE)
	median(data6[data6$D=="m" & data6$Journal!="PS", "N"], na.rm=TRUE)




#=========================================================
# Impact factor plot
# Using 2016 IF
# Obtained from webpage for each journal on March 17, 2017
#=========================================================


	# order for collecting data:
	#journal.names = c("EJP","EJSP","JESP","JP",  "JPSP","JRP", "PS","PSPB","SPPS")
	#IF2016 = c(3.707, 1.973, 2.159, 3.590, 5.017, 2.417, 5.667, 2.504, 1.883)
	#IF2019 =  c(3.910, 2.415, 3.254, 3.667, 6.335, 2.767, 5.389, 2.961, 4.38)

	# ordered same as info2C
	IF2019 =  c(2.767 ,3.667 , 6.335, 2.961,5.389 ,3.254 , 3.910, 2.415 , 4.38  )

	NP = info2C$npact 	# between-person designs only
	journal.names = as.character(info2C$journal)


	z = plasma(11)
	par(pty="s", mfrow=c(1,1), mar=c(5,5,5,5))
	plot(NP,IF2019, ylab="Impact Factor 2019", xlab="N-Pact 2011-2019",
		pch=15,cex.lab=1.4, cex.axis=1.3,
		ylim=c(1,7), xlim=c(50,400),
		col=c(z[1],z[2],z[3],z[4],z[5],z[6],z[7],z[8],z[9]))
	text(NP,IF2019,journal.names,cex=1.3,pos=c(4,	1,	2,	1,	4,	3,	2,	2,	3),
		col=c(z[1],z[2],z[3],z[4],z[5],z[6],z[7],z[8],z[9]))

	cor(NP, IF2019)   # Answer: -0.08 


# How many studies per article?



	nper1b = mean(data6$num_studiesa[data6$Journal=="EJP"  ], na.rm=TRUE)
	nper2b = mean(data6$num_studiesa[data6$Journal=="EJSP"  ], na.rm=TRUE)
	nper3b = mean(data6$num_studiesa[data6$Journal=="JESP" ], na.rm=TRUE)
	nper4b = mean(data6$num_studiesa[data6$Journal=="JP"  ], na.rm=TRUE)
	nper5b = mean(data6$num_studiesa[data6$Journal=="JPSP"  ], na.rm=TRUE)
	nper6b = mean(data6$num_studiesa[data6$Journal=="JRP"  ], na.rm=TRUE)
	nper7b = mean(data6$num_studiesa[data6$Journal=="PS" & data6$Social_PS==1  ], na.rm=TRUE)
	nper8b = mean(data6$num_studiesa[data6$Journal=="PSPB"  ], na.rm=TRUE)
	nper9b = mean(data6$num_studiesa[data6$Journal=="SPPS"  ], na.rm=TRUE)


	nper10b = mean(data6$num_studiesa[data6$Journal=="JPSP" & data6$JPSP_Section=="Attitudes and Social Cognition"], na.rm=TRUE)
	nper11b = mean(data6$num_studiesa[data6$Journal=="JPSP" & data6$JPSP_Section=="Interpersonal Relations and Group Processes" ], na.rm=TRUE)
	nper12b = mean(data6$num_studiesa[data6$Journal=="JPSP" & data6$JPSP_Section=="Personality Processes and Individual Differences" ], na.rm=TRUE)
	nper13b = mean(data6$num_studiesa[data6$Journal=="PS" & data6$Social_PS==0 ], na.rm=TRUE)


	nper3b # JESP	
	nper6b # JRP



	# get sum of N across all studies in an article

	# This will create a new list of total n per article
	# need to stick that back into the main data object
	# and then extract those values by journal

	# create an articleindex variable
	z = (dim(data6)[1])/10
	z

	zz=rep(1:z, each=10)

	length(zz)
	dim(data6)

	data6$ArticleIndex = zz


	# check an example
	data6[data6$ArticleIndex == 15,"N"]

	z = aggregate(data6$N, by=list(data6$ArticleIndex), FUN=sum, na.rm=TRUE)
	colnames(z)=c("ArticleIndex", "totalN")
	temp = merge(data6, z, by="ArticleIndex")

	temp$totalN[temp$totalN==0]=NA

	temp[1:100,c("ArticleIndex","Year","Journal","N","totalN")]

	median(temp$totalN[temp$Journal=="JPSP"], na.rm=TRUE)
	median(temp$totalN[temp$Journal=="JRP"], na.rm=TRUE)
	median(temp$totalN[temp$Journal=="JESP"], na.rm=TRUE)






























