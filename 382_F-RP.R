#NAMES: 

# Framework
data <- read.csv("Kickstarter.csv",na.strings=c("","NA"))
sum<-summary(data)

# 1)	Set your hypothesis, have a specific goal.

# Success of project based on backers, money raised, and category. (Quanitative variables are)
# For hyp testing (how do these attributes effect the data)

# Project with #(of backers) is successful, or 
# Average project in technology raises #($) or
# Average successful project has (# of backers) or
# ....... (this can be whatever u guys want.)

# 2)	Review your data to ensure that they are appropriate and complete and can help you prove or disprove your hypothesis.

# we want to only base our research on project on USD income.
data<-data[data$currency == "USD",]

# 3)	Complete literature review on the subject/hypothesis and determine if there is any relevant research/study has been already completed. Study the literature and include citations in your final report.

# https://towardsdatascience.com/predicting-the-success-of-kickstarter-campaigns-3f4a976419b9

# 4)	Once you ensure that your data are sufficient and you can initially rely on them to run your modeling, clean them up.
# 
# Exploratory Analysis
# 5)	Run Exploratory analysis on at least 5 total variables where 2 of them are quantitative.

# Varialbles we want to focus on / analyze :
# (1) backers_count , (2) ?category? , (3) goal [$]
# (4) pledged [amount of people supporting], (5) usd_pledged [total raised]

dataR<-data[c(1,3,5,6,9,17,23,27,33,37)]

parseType<-function(test){
test<-gsub('"', "",test)
sub(".*:name",",",test)
test<-sub(".*,name:","",test)
sub(",.*","",test)
}

dataR$category<-lapply(data$category,parseType)
dataR$location<-lapply(data$location,parseType)

# a.	Check for missing data
dataFinal<-dataR[rowSums(is.na(dataR))==0,]

# b.	Check for outliers, IQR, and summarize the statistics.
summary(dataFinal$backers_count)
boxplot.stats(dataFinal$backers_count)$out
IQR(dataFinal$backers_count)

summary(dataFinal$goal)
boxplot.stats(dataFinal$goal)$out
IQR(dataFinal$goal)

summary(dataFinal$pledged)
boxplot.stats(dataFinal$pledged)$out
IQR(dataFinal$pledged)

summary(dataFinal$usd_pledged)
boxplot.stats(dataFinal$usd_pledged)$out
IQR(dataFinal$usd_pledged)

# c.	Disect your variables in a way that will help you with your analysis.
# d.	Determine the distribution ( if any that your data follow, experimentally and theoretically)
# e.	Show your analysis in both tables/charts and visually ( histograms, qqnorm plots, boxplots etc. Statistical Modeling)
# 6)	Determine correlations, do correlation comparisons ( technically and visually ) use both plots or pairs for your graphical representations. Split your graphs in ways that will help you to conclude and infer based on your model.
# 7)	Your model and main hypothesis should be answered either using ANOVA or Regression Analysis, or both. - This may mean, depending on your data, that you may need to use a categorical variable to dissect your data, and that you may need to have data with many more than just 2 quantitative variables.
# 
# 
# Report
# The report should be written in R Markdown and then transformed to HTML.  Your report needs to have the following sections:
#   .	Introduction - In this section, explain briefly the purpose of your analysis. Identify your hypothesis, and in a single sentence refer to the results of your work.
# .	Data - a section describing the data set and how you loaded and transformed it in R.  Include R code blocks within your comments and explain what the code is doing.
# .	Analysis - walk through the analysis that you performed. Include R code blocks within your comments and explain what the code is doing.
# .	Issues - Refer to any issues you had with collecting your data, cleaning your data, or implementing your model.
# .	Results - any plots, tables, or other results which gave you the answers to the questions that you were asked. Include R code blocks for any plots and explain what the code is doing.
# .	Discussion - a brief (one or two paragraphs) discussion of the results and how they validate or not your initial claims. 
# 
# Presentation
# You should prepare a brief (5-8 minutes) presentation on your project
# You should create a Powerpoint presentation on your report.
# Your Presentation should be narrated, or you should create a recorded video, presenting the Powerpoint.
# The presentation should include:
#   .	Introduction - quick reference to the problem you are analyzing and your data
# .	Hypothesis - main claim or claims you worked on
# .	Methodology - what statistical tools you used to complete your analysis and why?
#   .	Issues - any issues you had while you run your analysis and how did you resolve them
# .	Tables, Charts and Graphs and measures with explanation for your EDA
# .	Results of Correlation, Regression, and ANOVA analysis
# .	Conclusions
# .	Future questions and further analysis not addressed in this project
# .	References


