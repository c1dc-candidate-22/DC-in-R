library(data.table)
library(tm)
library(NLP)
library(jsonlite)
library(rjson)
library(stats)
library(graphics)
library(ggplot2)
library(grDevices)
library(codetools)
library(datasets)
#set working directory to foldedr where Data is present
setwd("D:/UTD/DC/data-challenge-data-master")
df1<-read.csv("2012_to_2014_loans_data.csv")
df2<-read.csv("2012_to_2014_institutions_data.csv")
a<-join(df1,df2,type="inner") # 2 data tables

hmda_to_json(a,states,Conventional_Conforming_Flag) 
{ 
  hmda<-subset(a,Conventional_Conforming_Flag)
  write.table(hmda,"hmda_json.txt")
}
Dat<-subset(a,Conventional_Conforming_Flag="y")

#is.na(a$Loan_Amount_000)
LnAmt_MissingCount<-sum(is.nan(a$Loan_Amount_000))
Respondent_MissingCount<-sum(is.nan(a$Respondent_Name))
ApplicantInc_MissingCount<-sum(is.na(a$Applicant_Income_000))
a[is.na(a$Applicant_Income_000)] <- 0 #replace missing values of applicant income with 0

summary(a)
x<-nlevels(a$Conventional_Conforming_Flag)
#check if x = 2, to verify that the flag does not have any 3rd valus apart from Y or N
if(x==2) print("No error in Flag") else print ("Data error in Flag")

#Observe the increase/decrease in loan applications count through the years
Number_of_Ln_applications<-table(a$As_of_Year)
barplot(Number_of_Ln_applications,main="Number of Loan applications each year",xlab="Number of applications")

#Observe the agencies with maximum market share
Number_of_Ln_applPerAgency<-table(a$Agency_Code_Description)
barplot(Number_of_Ln_applPerAgency,main="Number of Loan applications responded by agencies",xlab="Agencies")

#Observe the market share state wise
Number_of_Ln_applPerState<-table(a$State)
barplot(Number_of_Ln_applPerState,main="Number of Loan applications statewise",xlab="States")

#Observe the number of Conventional & Conforming applications each year
FlagDat<-table(a$Conventional_Conforming_Flag,a$As_of_Year)
barplot(FlagDat,main="Conforming & Conventional loans in each year", xlab="Year", col=c("darkblue","red"),legend=rownames(FlagDat))

#Observe the number  of loan applications purpose wise
Number_of_LnPurposePerYear<-table(a$Loan_Purpose_Description)
barplot(Number_of_LnPurposePerYear,main="Number of Loan applications purpose wise",xlab="Loan Purpose")

#Since from the above graph we see the maximum of the loan applications in 3 years are received for the purpose of Refinancing, we analyse it deeper.
#Observe the Refinance type of loans through the 3 years.
RefinData<-subset(Dat,Loan_Purpose_Description="Refinance")
RefinDat<-table(RefinData$As_of_Year)
barplot(RefinDat,main="Loans for refinance in each year", xlab="Year")

#Since from the above graph we see the maximum of the loan applications in 3 years are received for the purpose of Refinancing, we analyse it deeper.
#Observe the Refinance type of loans for each state.
RefinDatState<-table(RefinData$State)
barplot(RefinDatState,main="Loans for refinance in each State", xlab="State")

#Looking at the graphs, introducing home loans would not be advisable because of 2 competitors already in market CFPB & HUD especially in VA & MD states

#(Number_Of_Conf_Conv_Loans<-table(a$Conventional_Conforming_Flag="y")
#Refin2012<-subset(a,Loan_Purpose_Description='Refinance', As_of_Year='2012')
#Purc2012<-subset(a,Loan_Purpose_Description='Purchasing', As_of_Year='2012')
#Refin2013<-subset(a,Loan_Purpose_Description='Refinance', As_of_Year='2013')
#Purc2013<-subset(a,Loan_Purpose_Description='Purchasing', As_of_Year='2013')
#Refin2014<-subset(a,Loan_Purpose_Description='Refinance', As_of_Year='2014')
#Purc2014<-subset(a,Loan_Purpose_Description='Purchasing', As_of_Year='2014')
#Data2012<-subset(a,As_of_Year='2012')
##Number_of_Ln_applications2012<-table(Data2012$Loan_Amount_000)
#Number_of_Ln_applications2013<-table(Data2013$Loan_Amount_000)
#Number_of_Ln_applications2014<-table(Data2014$Loan_Amount_000)
#Data2013<-subset(a,As_of_Year='2013')
#Data2014<-subset(a,As_of_Year='2014')
#Dat<-sum(a$Applicant_Income_000)
#g<-ggplot(a,aes(a$As_of_Year,a$Loan_Amount_000)) +
#            geom_point(aes(colour="blue")) +
#  geom_smooth(method='lm') +
#  coord_cartesian() +
#  scale_color_gradient() +
#  theme_bw()
#          g+geom_boxplot(a,las=2)
#          
#boxplot(a$State~a$Loan_Amount_000, data=a, main=toupper("Loan Amount"), font.main=3, cex.main=1.2, xlab="Year", ylab="Loan Amount", font.lab=3, col="darkgreen")
#LoanSum2012<-sum(Data2012$Loan_Amount_000)
#LoanSum2013<-sum(Data2013$Loan_Amount_000)
#LoanSum2014<-sum(Data2014$Loan_Amount_000)
#IncSum2012<-sum(Data2012$Applicant_Income_000)
#IncSum2013<-sum(Data2013$Applicant_Income_000)
#IncSum2014<-sum(Data2014$Applicant_Income_000)
#L_to_I_2012ratio<-LoanSum2012/IncSum2012
#L_to_I_2013ratio<-LoanSum2013/IncSum2013
#L_to_I_2014ratio<-LoanSum2014/IncSum2014
#AsYear<-c(2012,2013,2014)
#L_to_I_Ratios<-c(L_to_I_2012ratio,L_to_I_2013ratio,L_to_I_2014ratio)

qplot(x=AsYear,y=L_to_I_Ratios,data=a,geom='point')
#qplot(aes(x=a$As_of_Year,y=L_to_I_Ratios),data=a,geom='point')
#geom_point(data=a,aes(x=a$As_of_Year,y=L_to_I_Ratios),fill="green",alpha=0.8,size=5,shape=21)
#p<-ggplot(a,aes(AsYear,L_to_I_Ratios))
#p+ geom_point()
attach(a)
plot(AsYear,L_to_I_Ratios,type="p")
#abline(lm(AsYear~L_to_I_Ratios))
title('Loan amount to Income ratio')
