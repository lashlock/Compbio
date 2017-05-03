##Model from QTLS

##Modify model to use for comp bio

##Potential changes... changing error factor to 9 and relationship factor
##to .5 -> making it fecundity instead of hatch rate

###Each xvar relates to your yvar (hatch rate)

### xvars = Temp, HAB, Pop, TempXPop, TempXHAB, HABXPop, TempXHABXPop

###Steps...

##Make a data frame - Columns for yvar, xvars, and yerror - rows for sample size
#12 outputs X 6 replicates = 72 y outputs -> 72 rows
# 8 yvars + 7 xvars + 1 error column = 16 columns
cope.df<- data.frame(matrix(0, 72, 16))
cope.df

colnames(cope.df)<- c("Fecundity","Temp","Y1", "HAB", "Y2", "Pop", "Y3", "TempXPop", "Y4", "TempXHAB", "Y5", "HABXPop", "Y6", "TempXHABXPop", "Y7", "Error" )
head(cope.df)

##Use distributions to populate dummy data for xvars
#Temp will either be ambient or elevated for each pop

cope.df[1:12,2]<-"ambient"
cope.df[13:24,2]<-"elevated"
cope.df[25:36,2]<-"ambient"
cope.df[37:48,2]<-"elevated"
cope.df[49:60,2]<-"ambient"
cope.df[61:72,2]<-"elevated"
head(cope.df)

#HAB will either be present or absent

cope.df[1:6,4]<-"absent"
cope.df[7:12,4]<-"present"
cope.df[13:18,4]<-"absent"
cope.df[19:24,4]<-"present"
cope.df[25:30,4]<-"absent"
cope.df[31:36,4]<-"present"
cope.df[37:42,4]<-"absent"
cope.df[43:48,4]<-"present"
cope.df[49:54,4]<-"absent"
cope.df[55:60,4]<-"present"
cope.df[61:66,4]<-"absent"
cope.df[67:72,4]<-"present"
head(cope.df)


#population will be either ME, CT, or NJ

cope.df[1:24,6]<-"ME"
cope.df[25:48,6]<-"CT"
cope.df[49:72,6]<-"NJ"
head(cope.df)

#TempXPop
#MEXambient, MEXelevated, CTXambient, CTXelevated, NJXambient, NJXelevated

cope.df[1:12,8]<-"MEXambient"
cope.df[13:24,8]<-"MEXelevated"
cope.df[25:36,8]<-"CTXambient"
cope.df[37:48,8]<-"CTXelevated"
cope.df[49:60,8]<-"NJXambient"
cope.df[61:72,8]<-"NJXelevated"
head(cope.df)

#TempXHAB
#ambientXabsent, ambientXpresent, elevatedXabsent, elevatedXpresent

cope.df[1:6,10]<-"ambientXabsent"
cope.df[7:12,10]<-"ambientXpresent"
cope.df[13:18,10]<-"elevatedXabsent"
cope.df[19:24,10]<-"elevatedXpresent"
cope.df[25:30,10]<-"ambientXabsent"
cope.df[31:36,10]<-"ambientXpresent"
cope.df[37:42,10]<-"elevatedXabsent"
cope.df[43:48,10]<-"elevatedXpresent"
cope.df[49:54,10]<-"ambientXabsent"
cope.df[55:60,10]<-"ambientXpresent"
cope.df[61:66,10]<-"elevatedXabsent"
cope.df[67:72,10]<-"elevatedXpresent"
cope.df 

#HABXPop
#absentXME, presentXME, absentXCT, presentXCT, absentXNJ, presentXNJ

cope.df[1:6,12]<-"absentXME"
cope.df[7:12,12]<-"presentXME"
cope.df[13:18,12]<-"absentXME"
cope.df[19:24,12]<-"presentXME"
cope.df[25:30,12]<-"absentXCT"
cope.df[31:36,12]<-"presentXCT"
cope.df[37:42,12]<-"absentXCT"
cope.df[43:48,12]<-"presentXCT"
cope.df[49:54,12]<-"absentXNJ"
cope.df[55:60,12]<-"presentXNJ"
cope.df[61:66,12]<-"absentXNJ"
cope.df[67:72,12]<-"presentXNJ"
cope.df

#TXHXP
#ambientXabsentXME, ambientXpresentXME, elevatedXabsentXME, elevatedXpresentXME 
#ambientXabsentXCT, ambientXpresentXCT, elevatedXabsentXCT, elevatedXpresentXCT 
#ambientXabsentXNJ, ambientXpresentXNJ, elevatedXabsentXNJ, elevatedXpresentXNJ

cope.df[1:6,14]<-"ambientXabsentXME"
cope.df[7:12,14]<-"ambientXpresentXME"
cope.df[13:18,14]<-"elevatedXabsentXME"
cope.df[19:24,14]<-"elevatedXpresentXME"
cope.df[25:30,14]<-"ambientXabsentXCT"
cope.df[31:36,14]<-"ambientXpresentXCT"
cope.df[37:42,14]<-"elevatedXabsentXCT"
cope.df[43:48,14]<-"elevatedXpresentXCT"
cope.df[49:54,14]<-"ambientXabsentXNJ"
cope.df[55:60,14]<-"ambientXpresentXNJ"
cope.df[61:66,14]<- "elevatedXabsentXNJ"
cope.df[67:72,14]<-"elevatedXpresentXNJ"
cope.df

##Calculate Column 1 using your quantified relationships

##f(temp)
for(t in 1:72){
  temp.func<-{if (cope.df[t,2]=="ambient") {
    cope.df[t,3]=25
  } else {
    cope.df[t,3]=15
  }
    
  }
  
}
head(cope.df)
tail(cope.df)

##f(HAB)

for(t in 1:72){
  HAB.func<- {if (cope.df[t,4]=="absent") {
    cope.df[t,5]=25
  } else {
    cope.df[t,5]=20
  }
  }
}
cope.df

##f(pop)

pop.func<-55
cope.df[,7]<-55+rnorm(mean=0, sd=.5, n=72)

##f(tempxpop)
tempxpop.func<-55
cope.df[,9]<-55+rnorm(mean=0, sd=.5, n=72)

cope.df

##f(tempxHAB)
for(t in 1:72){
  tempxHAB.func<- {(if(cope.df[t,10]=="ambientXabsent"){
    cope.df[t,11]=70}) + (if(cope.df[t,10]=="ambientXpresent"){
      cope.df[t,11]=40}) + (if(cope.df[t,10]=="elevatedXabsent"){
        cope.df[t,11]=50}) + (if(cope.df[t,10]=="elevatedXpresent"){
          cope.df[t,11]=30})
  }
}
cope.df

##f(HABxpop)

for(t in 1:72){
  HABxpop.func<-{(if(cope.df[t,12]=="absentXME"){
    cope.df[t,13]=70}) + (if(cope.df[t,12]=="presentXME"){
      cope.df[t,13]=55}) + (if(cope.df[t,12]=="absentXCT"){
        cope.df[t,13]=70}) + (if(cope.df[t,12]=="presentXCT"){
          cope.df[t,13]=35}) + (if(cope.df[t,12]=="absentXNJ"){
            cope.df[t,13]=70}) + (if(cope.df[t,12]=="presentXNJ"){
              cope.df[t,13]=30})
  }
}
cope.df

##f(TxHxP)

for(t in 1:72){
  txhxp.func<-{(if(cope.df[t,14]=="ambientXabsentXME"){
    cope.df[t,15]=70}) + (if(cope.df[t,14]=="ambientXpresentXME"){
      cope.df[t,15]=55}) + (if(cope.df[t,14]=="elevatedXabsentXME"){
        cope.df[t,15]=50}) + (if(cope.df[t,14]=="elevatedXpresentXME"){
          cope.df[t,15]=30}) + (if(cope.df[t,14]=="ambientXabsentXCT"){
            cope.df[t,15]=70}) + (if(cope.df[t,14]=="ambientXpresentXCT"){
              cope.df[t,15]=35}) + (if(cope.df[t,14]=="elevatedXabsentXCT"){
                cope.df[t,15]=50}) + (if(cope.df[t,14]=="elevatedXpresentXCT"){
                  cope.df[t,15]=30}) + (if(cope.df[t,14]=="ambientXabsentXNJ"){
                    cope.df[t,15]=70}) + (if(cope.df[t,14]=="ambientXpresentXNJ"){
                      cope.df[t,15]=30}) + (if(cope.df[t,14]=="elevatedXabsentXNJ"){
                        cope.df[t,15]=50}) + (if(cope.df[t,14]=="elevatedXpresentXNJ"){
                          cope.df[t,15]=30})
  }
}
cope.df


##Calculate error for y data
#I estimate my xars explain 30% yvar.... so my error will be 70%
#finding error
cope.df[,16]<-rnorm(mean=0, sd=3.5, n=72)
cope.df
###Setting up the code to Test The Model

#cope.df[,3]=as.numeric(cope.df[,3])													
#cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
#cope.df	

#lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 

#mod.cope=lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#mod.cope
#summary(mod.cope)	##lm summary##



#aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
#			cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#aov.cope=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#summary(aov.cope)	##anova summary


#TukeyHSD(aov.cope)
#Tukey.cope=TukeyHSD(aov.cope)
#Tukey.cope	##summary Tukey HSD



#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`	#This gives you the differences between the three way interacrions and the significance of these differences


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,]	#CT elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-3.615597e+00 -5.443440e+00 -1.787753e+00  4.681260e-07  

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,]	#ME elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-6.146331e+00 -7.974174e+00 -4.318487e+00  1.994305e-11  

Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,]	#NJ elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-2.5283295448 -4.3561730485 -0.7004860412  0.0008778966   

##These are all super significant... but I do not really expect NJ or CT to be significant... ME does have the largest diff though... this makes sense
##Slightly less significant now that I added error to y3 and y4 

###########################Simulate times 1000#########################

##These are your outputs
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval


#Make vectors for these outputs

CT.diff=rep(0,1000)

CT.pval=rep(0,1000)

ME.diff=rep(0,1000)

ME.pval=rep(0,1000)

NJ.diff=rep(0,1000)

NJ.pval=rep(0,1000)

#Run the TukeyHSD 1000 times and populate your vectors with the outputs

for(i in 1:1000){
  cope.df[,7]<-55+rnorm(mean=0, sd=.5, n=72)
  cope.df[,9]<-55+rnorm(mean=0, sd=.5, n=72)
  cope.df[,16]<-rnorm(mean=0, sd=3.5, n=72)
  cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
  aov.cope=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
                 cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
  TukeyHSD(aov.cope)
  Tukey.cope=TukeyHSD(aov.cope)
  
  
  CT.diff[i]=Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
  CT.pval[i]=Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval
  
  
  ME.diff[i]=Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
  ME.pval[i]=Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 
  
  NJ.diff[i]=Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
  NJ.pval[i]=Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval
}

#data check
CT.diff
#
exp.vs.obs.df<- data.frame(matrix(0, 72, 3))
exp.vs.obs.df

colnames(exp.vs.obs.df)<- c("Expected","Observed", "Exp-Obs")
exp.vs.obs.df
predict(aov.cope)	

exp.vs.obs.df[,1]<-cope.df[,1]

exp.vs.obs.df[,2]<-predict(aov.cope)

exp.vs.obs.df[,3]<-exp.vs.obs.df[,1]-exp.vs.obs.df[,2]

exp.vs.obs.df


#
#summarize simulation
#mean(CT.diff)
#mean(CT.pval)
#mean(ME.diff)
#mean(ME.pval)
#mean(NJ.diff)
#mean(NJ.pval)

#Make a results table

pop.results.df<- data.frame(matrix(0, 7, 9))
pop.results.df

colnames(pop.results.df)<- c("Distribution","RF","EF","ME.effectsze", "ME.pval", "CT.effectsze", "CT.pval", "NJ.effectsze", "NJ.pval")
pop.results.df
pop.results.df[1,1]="Expected"
pop.results.df[1,2]=1
pop.results.df[1,3]=3.5
pop.results.df[1,4]<-mean(ME.diff)
pop.results.df[1,5]<-mean(ME.pval)
pop.results.df[1,6]<-mean(CT.diff)
pop.results.df[1,7]<-mean(CT.pval)
pop.results.df[1,8]<-mean(NJ.diff)
pop.results.df[1,9]<-mean(NJ.pval)
pop.results.df

######Now to summarize this you need to graphically show the distribution of effect sizes and the percent time it is significant for each run####
hist(ME.diff, main="ME*Present*Ambient vs ME*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(CT.diff, main="CT*Present*Ambient vs CT*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(NJ.diff, main="NJ*Present*Ambient vs NJ*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")

###Relative frequency of significant p values

sum(ME.pval<.05)
ME.pval	#This is always the same number... which is weird... none of my other populations are doing this


sum(CT.pval<.05)
CT.pval

sum(NJ.pval<.05)
NJ.pval

###All of these are significant 100% of the time... seems unlikely though...

###############Now try with different error factors###########################

###When SD = 10 and Variables explain 40%
####Setting up the loop####
#cope.df[,16]<-rnorm(mean=0, sd=6, n=72)


#cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
#cope.df	

#lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 

#mod.cope=lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
#			cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 
#mod.cope
#summary(mod.cope)	##lm summary


#aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
#			cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 
#aov.cope=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 
#summary(aov.cope)	##anova summary


#TukeyHSD(aov.cope)				
#Tukey.cope=TukeyHSD(aov.cope)
#Tukey.cope	##summary Tukey HSD


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`	#This gives you the differences between the three way interacrions and the significance of these differences


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,]	#CT elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-3.326963e+00 -4.790901e+00 -1.863025e+00  9.305149e-09  

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,]	#ME elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-6.684632e+00 -8.148570e+00 -5.220694e+00  1.988842e-11   

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,]	#NJ elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-3.216431e+00 -4.680369e+00 -1.752493e+00  2.544431e-08 

###These Results are ever so slightly less significant...

#Make vectors for these outputs

CT.diff.ef6=rep(0,1000)

CT.pval.ef6=rep(0,1000)

ME.diff.ef6=rep(0,1000)

ME.pval.ef6=rep(0,1000)

NJ.diff.ef6=rep(0,1000)

NJ.pval.ef6=rep(0,1000)

#Run the TukeyHSD 1000 times and populate your vectors with the outputs

for(i in 1:1000){
  cope.df[,7]<-55+rnorm(mean=0, sd=.5, n=72) 
  cope.df[,9]<-55+rnorm(mean=0, sd=.5, n=72)
  cope.df[,16]<-rnorm(mean=0, sd=6, n=72)
  cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
  aov.cope.ef6=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
                     cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
  TukeyHSD(aov.cope.ef6)
  Tukey.cope.ef6=TukeyHSD(aov.cope.ef6)
  
  
  CT.diff.ef6[i]=Tukey.cope.ef6$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
  CT.pval.ef6[i]=Tukey.cope.ef6$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval
  
  
  ME.diff.ef6[i]=Tukey.cope.ef6$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
  ME.pval.ef6[i]=Tukey.cope.ef6$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 
  
  NJ.diff.ef6[i]=Tukey.cope.ef6$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
  NJ.pval.ef6[i]=Tukey.cope.ef6$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval
}

#data check
CT.diff.ef6
predict(aov.cope.ef6)
exp.vs.obs.df.ef6<- data.frame(matrix(0, 72, 3))
exp.vs.obs.df.ef6

colnames(exp.vs.obs.df.ef6)<- c("Expected","Observed", "Exp-Obs")
exp.vs.obs.df.ef6
predict(aov.cope.ef6)	

exp.vs.obs.df.ef6[,1]<-cope.df[,1]

exp.vs.obs.df.ef6[,2]<-predict(aov.cope.ef6)

exp.vs.obs.df.ef6[,3]<-exp.vs.obs.df.ef6[,1]-exp.vs.obs.df.ef6[,2]

exp.vs.obs.df.ef6
#

#summarize simulation
mean(CT.diff.ef6)
mean(CT.pval.ef6)
mean(ME.diff.ef6)
mean(ME.pval.ef6)
mean(NJ.diff.ef6)
mean(NJ.pval.ef6)

#populate results table

pop.results.df[2,1]="Expected"
pop.results.df[2,2]=1
pop.results.df[2,3]=6
pop.results.df[2,4]<-mean(ME.diff.ef6)
pop.results.df[2,5]<-mean(ME.pval.ef6)
pop.results.df[2,6]<-mean(CT.diff.ef6)
pop.results.df[2,7]<-mean(CT.pval.ef6)
pop.results.df[2,8]<-mean(NJ.diff.ef6)
pop.results.df[2,9]<-mean(NJ.pval.ef6)
pop.results.df													

######Now to summarize this you need to graphically show the distribution of effect sizes and the percent time it is significant for each run####
hist(ME.diff.ef6, main="Error Factor = 6 \n ME*Present*Ambient vs ME*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(CT.diff.ef6, main=" Error Factor = 6 \n CT*Present*Ambient vs CT*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(NJ.diff.ef6, main=" Error Factor = 6 \n NJ*Present*Ambient vs NJ*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")

###Relative frequency of significant p values

sum(ME.pval.ef6<.05)
ME.pval.ef6	
#100%
sum(CT.pval.ef6<.05)
CT.pval.ef6
#99.9%

sum(NJ.pval.ef6<.05)
NJ.pval.ef6
#97.8%

#######Starting to look more reasonable here

########################When SD = 15 and Variables explain 40%#######################
##Setting up the loop
#cope.df[,16]<-rnorm(mean=0, sd=9, n=72)


#cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
#cope.df	

#lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 

#mod.cope=lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 
#mod.cope
#summary(mod.cope)	##lm summary


#aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
#			cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 
#aov.cope=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
#			cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 
#summary(aov.cope)	##anova summary


#TukeyHSD(aov.cope)				
#Tukey.cope=TukeyHSD(aov.cope)
#Tukey.cope	##summary Tukey HSD


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`	#This gives you the differences between the three way interacrions and the significance of these differences


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,]	#CT elevated present vs ambient present

#      diff        lwr        upr      p adj 
#-2.4638792 -5.3361001  0.4083417  0.1614111   

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,]	#ME elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-6.645583e+00 -9.517803e+00 -3.773362e+00  5.384436e-09    

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,]	#NJ elevated present vs ambient present

#      diff        lwr        upr      p adj 
#-2.4531528 -5.3253737  0.4190681  0.1659150

###This seems about right to me... 

#Make vectors for these outputs

CT.diff.ef9=rep(0,1000)

CT.pval.ef9=rep(0,1000)

ME.diff.ef9=rep(0,1000)

ME.pval.ef9=rep(0,1000)

NJ.diff.ef9=rep(0,1000)

NJ.pval.ef9=rep(0,1000)

#Run the TukeyHSD 1000 times and populate your vectors with the outputs

for(i in 1:1000){
  cope.df[,7]<-55+rnorm(mean=0, sd=.5, n=72) 
  cope.df[,9]<-55+rnorm(mean=0, sd=.5, n=72)
  cope.df[,16]<-rnorm(mean=0, sd=9, n=72)
  cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
  aov.cope.ef9=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
                     cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
  TukeyHSD(aov.cope.ef9)
  Tukey.cope.ef9=TukeyHSD(aov.cope.ef9)
  
  
  CT.diff.ef9[i]=Tukey.cope.ef9$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
  CT.pval.ef9[i]=Tukey.cope.ef9$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval
  
  
  ME.diff.ef9[i]=Tukey.cope.ef9$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
  ME.pval.ef9[i]=Tukey.cope.ef9$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 
  
  NJ.diff.ef9[i]=Tukey.cope.ef9$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
  NJ.pval.ef9[i]=Tukey.cope.ef9$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval
}

#data check
CT.diff.ef9
predict(aov.cope.ef9)
exp.vs.obs.df.ef9<- data.frame(matrix(0, 72, 3))
exp.vs.obs.df.ef9

colnames(exp.vs.obs.df.ef9)<- c("Expected","Observed", "Exp-Obs")
exp.vs.obs.df.ef9
predict(aov.cope.ef9)	

exp.vs.obs.df.ef9[,1]<-cope.df[,1]

exp.vs.obs.df.ef9[,2]<-predict(aov.cope.ef9)

exp.vs.obs.df.ef9[,3]<-exp.vs.obs.df.ef9[,1]-exp.vs.obs.df.ef9[,2]

exp.vs.obs.df.ef9
#

#summarize simulation
mean(CT.diff.ef9)
mean(CT.pval.ef9)
mean(ME.diff.ef9)
mean(ME.pval.ef9)
mean(NJ.diff.ef9)
mean(NJ.pval.ef9)

#populate results table

pop.results.df[3,1]="Expected"
pop.results.df[3,2]=1
pop.results.df[3,3]=9
pop.results.df[3,4]<-mean(ME.diff.ef9)
pop.results.df[3,5]<-mean(ME.pval.ef9)
pop.results.df[3,6]<-mean(CT.diff.ef9)
pop.results.df[3,7]<-mean(CT.pval.ef9)
pop.results.df[3,8]<-mean(NJ.diff.ef9)
pop.results.df[3,9]<-mean(NJ.pval.ef9)
pop.results.df													

######Now to summarize this you need to graphically show the distribution of effect sizes and the percent time it is significant for each run####
hist(ME.diff.ef9, main="Error Factor = 9 \n ME*Present*Ambient vs ME*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(CT.diff.ef9, main=" Error Factor = 9 \n CT*Present*Ambient vs CT*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(NJ.diff.ef9, main=" Error Factor = 9 \n NJ*Present*Ambient vs NJ*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")

###Relative frequency of significant p values

sum(ME.pval.ef9<.05)
ME.pval.ef9	
#100%
sum(CT.pval.ef9<.05)
CT.pval.ef9
#91.4%

sum(NJ.pval.ef9<.05)
NJ.pval.ef9
#66.5%

####### This is more along the lines with what I am expecting
##More noise... less significance for the CT and NJ pops


########Try this with a few different relationship factors##########
rf=.75
###Adding in new factors with new relationship factor
cope.df$f.Y1=(cope.df$Y1-mean(cope.df$Y1))*rf+mean(cope.df$Y1)
cope.df$f.Y2=(cope.df$Y2-mean(cope.df$Y2))*rf+mean(cope.df$Y2)
cope.df$f.Y3=(cope.df$Y3-mean(cope.df$Y3))*rf+mean(cope.df$Y3)
cope.df$f.Y4=(cope.df$Y4-mean(cope.df$Y4))*rf+mean(cope.df$Y4)
cope.df$f.Y5=(cope.df$Y5-mean(cope.df$Y5))*rf+mean(cope.df$Y5)
cope.df$f.Y6=(cope.df$Y6-mean(cope.df$Y6))*rf+mean(cope.df$Y6)
cope.df$f.Y7=(cope.df$Y7-mean(cope.df$Y7))*rf+mean(cope.df$Y7)
cope.df
#data check
mean(cope.df$Y1)
mean(cope.df$f.Y1)

####Setting up the loop...
#cope.df[,1]=(cope.df[,17]+cope.df[,18]+cope.df[,19]+cope.df[,20]+cope.df[,21]+cope.df[,22]+cope.df[,23]+cope.df[,16])/7
#cope.df	

#lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 

#mod.cope=lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#mod.cope
#summary(mod.cope)	##lm summary##



#aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#aov.cope=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#summary(aov.cope)	##anova summary


#TukeyHSD(aov.cope)
#Tukey.cope=TukeyHSD(aov.cope)
#Tukey.cope	##summary Tukey HSD



#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`	#This gives you the differences between the three way interacrions and the significance of these differences


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,]	#CT elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-2.495797e+00 -4.091663e+00 -8.999309e-01  9.842482e-05   

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,]	#ME elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-4.383633e+00 -5.979499e+00 -2.787767e+00  3.740730e-11    

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,]	#NJ elevated present vs ambient present

#       diff         lwr         upr       p adj 
#-1.82127444 -3.41714049 -0.22540840  0.01275514  
##########Moving forward all have ef of 9#########

####Setting up vectors####

CT.diff.rf75=rep(0,1000)

CT.pval.rf75=rep(0,1000)

ME.diff.rf75=rep(0,1000)

ME.pval.rf75=rep(0,1000)

NJ.diff.rf75=rep(0,1000)

NJ.pval.rf75=rep(0,1000)

#Run the TukeyHSD 1000 times and populate your vectors with the outputs

for(i in 1:1000){
  cope.df[,7]<-55+rnorm(mean=0, sd=.5, n=72) 
  cope.df[,9]<-55+rnorm(mean=0, sd=.5, n=72)
  cope.df[,16]<-rnorm(mean=0, sd=9, n=72)
  cope.df[,1]=(cope.df[,17]+cope.df[,18]+cope.df[,19]+cope.df[,20]+cope.df[,21]+cope.df[,22]+cope.df[,23]+cope.df[,16])/7
  aov.cope.rf75=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
                      cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
  TukeyHSD(aov.cope.rf75)
  Tukey.cope.rf75=TukeyHSD(aov.cope.rf75)
  
  
  CT.diff.rf75[i]=Tukey.cope.rf75$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
  CT.pval.rf75[i]=Tukey.cope.rf75$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval
  
  
  ME.diff.rf75[i]=Tukey.cope.rf75$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
  ME.pval.rf75[i]=Tukey.cope.rf75$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 
  
  NJ.diff.rf75[i]=Tukey.cope.rf75$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
  NJ.pval.rf75[i]=Tukey.cope.rf75$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval
}

#data check
CT.diff.rf75
predict(aov.cope.rf75)
exp.vs.obs.df.rf75<- data.frame(matrix(0, 72, 3))
exp.vs.obs.df.rf75

colnames(exp.vs.obs.df.rf75)<- c("Expected","Observed", "Exp-Obs")
exp.vs.obs.df.rf75
predict(aov.cope.rf75)	

exp.vs.obs.df.rf75[,1]<-cope.df[,1]

exp.vs.obs.df.rf75[,2]<-predict(aov.cope.rf75)

exp.vs.obs.df.rf75[,3]<-exp.vs.obs.df.rf75[,1]-exp.vs.obs.df.rf75[,2]

exp.vs.obs.df.rf75
#

#summarize simulation
mean(CT.diff.rf75)
mean(CT.pval.rf75)
mean(ME.diff.rf75)
mean(ME.pval.rf75)
mean(NJ.diff.rf75)
mean(NJ.pval.rf75)

#populate results table

pop.results.df[4,1]="Expected"
pop.results.df[4,2]=.75
pop.results.df[4,3]=9
pop.results.df[4,4]<-mean(ME.diff.rf75)
pop.results.df[4,5]<-mean(ME.pval.rf75)
pop.results.df[4,6]<-mean(CT.diff.rf75)
pop.results.df[4,7]<-mean(CT.pval.rf75)
pop.results.df[4,8]<-mean(NJ.diff.rf75)
pop.results.df[4,9]<-mean(NJ.pval.rf75)
pop.results.df															

######Now to summarize this you need to graphically show the distribution of effect sizes and the percent time it is significant for each run####
hist(ME.diff.rf75, main="Relationship Factor = 0.75 \n ME*Present*Ambient vs ME*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(CT.diff.rf75, main=" Relationship Factor = 0.75 \n CT*Present*Ambient vs CT*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(NJ.diff.rf75, main=" Relationship Factor = 0.75 \n NJ*Present*Ambient vs NJ*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")

###Relative frequency of significant p values
ME.diff.rf75
sum(ME.pval.rf75<.05)
ME.pval.rf75	
#99.9%
sum(CT.pval.rf75<.05)
CT.pval.rf75
#58%

sum(NJ.pval.rf75<.05)
NJ.pval.rf75
#32%

##### I think I would expect to see CT significant more frequently...

###########################try now when rf is .5
rf=.5

f.Y1=(cope.df$Y1-mean(cope.df$Y1))*rf+mean(cope.df$Y1)
f.Y2=(cope.df$Y2-mean(cope.df$Y2))*rf+mean(cope.df$Y2)
f.Y3=(cope.df$Y3-mean(cope.df$Y3))*rf+mean(cope.df$Y3)
f.Y4=(cope.df$Y4-mean(cope.df$Y4))*rf+mean(cope.df$Y4)
f.Y5=(cope.df$Y5-mean(cope.df$Y5))*rf+mean(cope.df$Y5)
f.Y6=(cope.df$Y6-mean(cope.df$Y6))*rf+mean(cope.df$Y6)
f.Y7=(cope.df$Y7-mean(cope.df$Y7))*rf+mean(cope.df$Y7)
cope.df

#data check
mean(cope.df$Y1)
mean(cope.df$f.Y1)

#cope.df[,1]=(cope.df[,17]+cope.df[,18]+cope.df[,19]+cope.df[,20]+cope.df[,21]+cope.df[,22]+cope.df[,23]+cope.df[,16])/7
#cope.df	

#lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6]) 

#mod.cope=lm(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#mod.cope
#summary(mod.cope)	##lm summary##


#aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
#			cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#aov.cope=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
#summary(aov.cope)	##anova summary


#TukeyHSD(aov.cope)
#Tukey.cope=TukeyHSD(aov.cope)
#Tukey.cope	##summary Tukey HSD



#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`	#This gives you the differences between the three way interacrions and the significance of these differences


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,]	#CT elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-2.495797e+00 -4.091663e+00 -8.999309e-01  9.842482e-05   

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,]	#ME elevated present vs ambient present

#         diff           lwr           upr         p adj 
#-4.383633e+00 -5.979499e+00 -2.787767e+00  3.740730e-11  

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,]	#NJ elevated present vs ambient present

#       diff         lwr         upr       p adj 
#-1.82127444 -3.41714049 -0.22540840  0.01275514  


####Setting up vectors####

CT.diff.rf5=rep(0,1000)

CT.pval.rf5=rep(0,1000)

ME.diff.rf5=rep(0,1000)

ME.pval.rf5=rep(0,1000)

NJ.diff.rf5=rep(0,1000)

NJ.pval.rf5=rep(0,1000)

#Run the TukeyHSD 1000 times and populate your vectors with the outputs

for(i in 1:1000){
  cope.df[,7]<-55+rnorm(mean=0, sd=.5, n=72) 
  cope.df[,9]<-55+rnorm(mean=0, sd=.5, n=72)
  cope.df[,16]<-rnorm(mean=0, sd=9, n=72)
  cope.df[,1]=(cope.df[,17]+cope.df[,18]+cope.df[,19]+cope.df[,20]+cope.df[,21]+cope.df[,22]+cope.df[,23]+cope.df[,16])/7
  aov.cope.rf5=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
                     cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
  TukeyHSD(aov.cope.rf5)
  Tukey.cope.rf5=TukeyHSD(aov.cope.rf5)
  
  
  CT.diff.rf5[i]=Tukey.cope.rf5$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
  CT.pval.rf5[i]=Tukey.cope.rf5$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval
  
  
  ME.diff.rf5[i]=Tukey.cope.rf5$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
  ME.pval.rf5[i]=Tukey.cope.rf5$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 
  
  NJ.diff.rf5[i]=Tukey.cope.rf5$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
  NJ.pval.rf5[i]=Tukey.cope.rf5$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval
}

#data check
CT.diff.rf5
predict(aov.cope.rf5)
exp.vs.obs.df.rf5<- data.frame(matrix(0, 72, 3))
exp.vs.obs.df.rf5

colnames(exp.vs.obs.df.rf5)<- c("Expected","Observed", "Exp-Obs")
exp.vs.obs.df.rf5
predict(aov.cope.rf5)	

exp.vs.obs.df.rf5[,1]<-cope.df[,1]

exp.vs.obs.df.rf5[,2]<-predict(aov.cope.rf5)

exp.vs.obs.df.rf5[,3]<-exp.vs.obs.df.rf5[,1]-exp.vs.obs.df.rf5[,2]

exp.vs.obs.df.rf5
#

#summarize simulation
mean(CT.diff.rf5)
mean(CT.pval.rf5)
mean(ME.diff.rf5)
mean(ME.pval.rf5)
mean(NJ.diff.rf5)
mean(NJ.pval.rf5)

#populate results table

pop.results.df[5,1]="Expected"
pop.results.df[5,2]=0.50
pop.results.df[5,3]=9
pop.results.df[5,4]<-mean(ME.diff.rf5)
pop.results.df[5,5]<-mean(ME.pval.rf5)
pop.results.df[5,6]<-mean(CT.diff.rf5)
pop.results.df[5,7]<-mean(CT.pval.rf5)
pop.results.df[5,8]<-mean(NJ.diff.rf5)
pop.results.df[5,9]<-mean(NJ.pval.rf5)
pop.results.df													

######Now to summarize this you need to graphically show the distribution of effect sizes and the percent time it is significant for each run####
hist(ME.diff.rf5, main="Relationship Factor = 0.50 \n ME*Present*Ambient vs ME*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(CT.diff.rf5, main=" Relationship Factor = 0.50 \n CT*Present*Ambient vs CT*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(NJ.diff.rf5, main=" Relationship Factor = 0.50 \n NJ*Present*Ambient vs NJ*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")

###Relative frequency of significant p values
ME.diff.rf5
sum(ME.pval.rf5<.05)
ME.pval.rf5	
#100%
sum(CT.pval.rf5<.05)
CT.pval.rf5
#57.3%

sum(NJ.pval.rf5<.05)
NJ.pval.rf5
#33.4%

######Looks about the same to the other relationship factor

#########^^^^These were all run with expected distribution (40-80)

#######Let's try running with a wider distribution (20-90)

##f(temp)
for(t in 1:72){
  temp.func<-{if (cope.df[t,2]=="ambient") {
    cope.df[t,3]=80
  } else {
    cope.df[t,3]=50
  }
    
  }
  
}
cope.df

##f(HAB)

for(t in 1:72){
  HAB.func<- {if (cope.df[t,4]=="absent") {
    cope.df[t,5]=80
  } else {
    cope.df[t,5]=50
  }
  }
}
cope.df

##f(pop)

pop.func<-55
cope.df[,7]<-65+rnorm(mean=0, sd=.5, n=72)

##f(tempxpop)
tempxpop.func<-55
cope.df[,9]<-65+rnorm(mean=0, sd=.5, n=72)

cope.df

##f(tempxHAB)
for(t in 1:72){
  tempxHAB.func<- {(if(cope.df[t,10]=="ambientXabsent"){
    cope.df[t,11]=80}) + (if(cope.df[t,10]=="ambientXpresent"){
      cope.df[t,11]=60}) + (if(cope.df[t,10]=="elevatedXabsent"){
        cope.df[t,11]=70}) + (if(cope.df[t,10]=="elevatedXpresent"){
          cope.df[t,11]=30})
  }
}
cope.df

##f(HABxpop)

for(t in 1:72){
  HABxpop.func<-{(if(cope.df[t,12]=="absentXME"){
    cope.df[t,13]=80}) + (if(cope.df[t,12]=="presentXME"){
      cope.df[t,13]=70}) + (if(cope.df[t,12]=="absentXCT"){
        cope.df[t,13]=80}) + (if(cope.df[t,12]=="presentXCT"){
          cope.df[t,13]=50}) + (if(cope.df[t,12]=="absentXNJ"){
            cope.df[t,13]=80}) + (if(cope.df[t,12]=="presentXNJ"){
              cope.df[t,13]=30})
  }
}
cope.df

##f(TxHxP)

for(t in 1:72){
  txhxp.func<-{(if(cope.df[t,14]=="ambientXabsentXME"){
    cope.df[t,15]=80}) + (if(cope.df[t,14]=="ambientXpresentXME"){
      cope.df[t,15]=70}) + (if(cope.df[t,14]=="elevatedXabsentXME"){
        cope.df[t,15]=70}) + (if(cope.df[t,14]=="elevatedXpresentXME"){
          cope.df[t,15]=30}) + (if(cope.df[t,14]=="ambientXabsentXCT"){
            cope.df[t,15]=80}) + (if(cope.df[t,14]=="ambientXpresentXCT"){
              cope.df[t,15]=50}) + (if(cope.df[t,14]=="elevatedXabsentXCT"){
                cope.df[t,15]=70}) + (if(cope.df[t,14]=="elevatedXpresentXCT"){
                  cope.df[t,15]=30}) + (if(cope.df[t,14]=="ambientXabsentXNJ"){
                    cope.df[t,15]=80}) + (if(cope.df[t,14]=="ambientXpresentXNJ"){
                      cope.df[t,15]=30}) + (if(cope.df[t,14]=="elevatedXabsentXNJ"){
                        cope.df[t,15]=70}) + (if(cope.df[t,14]=="elevatedXpresentXNJ"){
                          cope.df[t,15]=30})
  }
}
cope.df


###########################Test and simulate times 1000#########################

##These are your outputs
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval


#Make vectors for these outputs

CT.diff.lrgspread=rep(0,1000)

CT.pval.lrgspread=rep(0,1000)

ME.diff.lrgspread=rep(0,1000)

ME.pval.lrgspread=rep(0,1000)

NJ.diff.lrgspread=rep(0,1000)

NJ.pval.lrgspread=rep(0,1000)

#Run the TukeyHSD 1000 times and populate your vectors with the outputs

for(i in 1:1000){
  cope.df[,7]<-65+rnorm(mean=0, sd=.5, n=72)
  cope.df[,9]<-65+rnorm(mean=0, sd=.5, n=72)
  cope.df[,16]<-rnorm(mean=0, sd=9, n=72)
  cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
  aov.cope.lrgspread=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
                           cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
  TukeyHSD(aov.cope.lrgspread)
  Tukey.cope.lrgspread=TukeyHSD(aov.cope.lrgspread)
  
  
  CT.diff.lrgspread[i]=Tukey.cope.lrgspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
  CT.pval.lrgspread[i]=Tukey.cope.lrgspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval
  
  
  ME.diff.lrgspread[i]=Tukey.cope.lrgspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
  ME.pval.lrgspread[i]=Tukey.cope.lrgspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 
  
  NJ.diff.lrgspread[i]=Tukey.cope.lrgspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
  NJ.pval.lrgspread[i]=Tukey.cope.lrgspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval
}

#data check
CT.diff
#
exp.vs.obs.df<- data.frame(matrix(0, 72, 3))
exp.vs.obs.df

colnames(exp.vs.obs.df)<- c("Expected","Observed", "Exp-Obs")
exp.vs.obs.df
predict(aov.cope.lrgspread)	

exp.vs.obs.df[,1]<-cope.df[,1]

exp.vs.obs.df[,2]<-predict(aov.cope.lrgspread)

exp.vs.obs.df[,3]<-exp.vs.obs.df[,1]-exp.vs.obs.df[,2]

exp.vs.obs.df


#
#summarize simulation
#mean(CT.diff.lrgspread)
#mean(CT.pval.lrgspread)
#mean(ME.diff.lrgspread)
#mean(ME.pval.lrgspread)
#mean(NJ.diff.lrgspread)
#mean(NJ.pval.lrgspread)

#Populate Results Table

pop.results.df[6,1]="lrgspread"
pop.results.df[6,2]=1
pop.results.df[6,3]=9
pop.results.df[6,4]<-mean(ME.diff.lrgspread)
pop.results.df[6,5]<-mean(ME.pval.lrgspread)
pop.results.df[6,6]<-mean(CT.diff.lrgspread)
pop.results.df[6,7]<-mean(CT.pval.lrgspread)
pop.results.df[6,8]<-mean(NJ.diff.lrgspread)
pop.results.df[6,9]<-mean(NJ.pval.lrgspread)
pop.results.df

######Now to summarize this you need to graphically show the distribution of effect sizes and the percent time it is significant for each run####
hist(ME.diff.lrgspread, main="Distribution = lrgspread \n ME*Present*Ambient vs ME*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(CT.diff.lrgspread, main="Distribution = lrgspread \n CT*Present*Ambient vs CT*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(NJ.diff.lrgspread, main="Distribution = lrgspread \n NJ*Present*Ambient vs NJ*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")

###Relative frequency of significant p values

sum(ME.pval.lrgspread<.05)
ME.pval.lrgspread	
#100%

sum(CT.pval.lrgspread<.05)
CT.pval.lrgspread
#100%


sum(NJ.pval.lrgspread<.05)
NJ.pval.lrgspread
#100%

############I really don't expect effect size to be so large or all of my pvalues to be <0.05


#######Let's try running with a narrow distribution (50-70)

##f(temp)
for(t in 1:72){
  temp.func<-{if (cope.df[t,2]=="ambient") {
    cope.df[t,3]=65
  } else {
    cope.df[t,3]=60
  }
    
  }
  
}
cope.df

##f(HAB)

for(t in 1:72){
  HAB.func<- {if (cope.df[t,4]=="absent") {
    cope.df[t,5]=65
  } else {
    cope.df[t,5]=60
  }
  }
}
cope.df

##f(pop)

pop.func<-55
cope.df[,7]<-62.5+rnorm(mean=0, sd=.5, n=72)

##f(tempxpop)
tempxpop.func<-55
cope.df[,9]<-62.5+rnorm(mean=0, sd=.5, n=72)

cope.df

##f(tempxHAB)
for(t in 1:72){
  tempxHAB.func<- {(if(cope.df[t,10]=="ambientXabsent"){
    cope.df[t,11]=65}) + (if(cope.df[t,10]=="ambientXpresent"){
      cope.df[t,11]=60}) + (if(cope.df[t,10]=="elevatedXabsent"){
        cope.df[t,11]=60}) + (if(cope.df[t,10]=="elevatedXpresent"){
          cope.df[t,11]=55})
  }
}
cope.df

##f(HABxpop)

for(t in 1:72){
  HABxpop.func<-{(if(cope.df[t,12]=="absentXME"){
    cope.df[t,13]=65}) + (if(cope.df[t,12]=="presentXME"){
      cope.df[t,13]=65}) + (if(cope.df[t,12]=="absentXCT"){
        cope.df[t,13]=65}) + (if(cope.df[t,12]=="presentXCT"){
          cope.df[t,13]=60}) + (if(cope.df[t,12]=="absentXNJ"){
            cope.df[t,13]=65}) + (if(cope.df[t,12]=="presentXNJ"){
              cope.df[t,13]=55})
  }
}
cope.df

##f(TxHxP)

for(t in 1:72){
  txhxp.func<-{(if(cope.df[t,14]=="ambientXabsentXME"){
    cope.df[t,15]=65}) + (if(cope.df[t,14]=="ambientXpresentXME"){
      cope.df[t,15]=65}) + (if(cope.df[t,14]=="elevatedXabsentXME"){
        cope.df[t,15]=60}) + (if(cope.df[t,14]=="elevatedXpresentXME"){
          cope.df[t,15]=55}) + (if(cope.df[t,14]=="ambientXabsentXCT"){
            cope.df[t,15]=65}) + (if(cope.df[t,14]=="ambientXpresentXCT"){
              cope.df[t,15]=60}) + (if(cope.df[t,14]=="elevatedXabsentXCT"){
                cope.df[t,15]=60}) + (if(cope.df[t,14]=="elevatedXpresentXCT"){
                  cope.df[t,15]=55}) + (if(cope.df[t,14]=="ambientXabsentXNJ"){
                    cope.df[t,15]=65}) + (if(cope.df[t,14]=="ambientXpresentXNJ"){
                      cope.df[t,15]=55}) + (if(cope.df[t,14]=="elevatedXabsentXNJ"){
                        cope.df[t,15]=60}) + (if(cope.df[t,14]=="elevatedXpresentXNJ"){
                          cope.df[t,15]=55})
  }
}
cope.df


###########################Test and simulate times 1000#########################

##These are your outputs
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval


#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 

#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
#Tukey.cope$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval


#Make vectors for these outputs

CT.diff.smlspread=rep(0,1000)

CT.pval.smlspread=rep(0,1000)

ME.diff.smlspread=rep(0,1000)

ME.pval.smlspread=rep(0,1000)

NJ.diff.smlspread=rep(0,1000)

NJ.pval.smlspread=rep(0,1000)

#Run the TukeyHSD 1000 times and populate your vectors with the outputs

for(i in 1:1000){
  cope.df[,7]<-62.5+rnorm(mean=0, sd=.5, n=72)
  cope.df[,9]<-62.5+rnorm(mean=0, sd=.5, n=72)
  cope.df[,16]<-rnorm(mean=0, sd=9, n=72)
  cope.df[,1]=(cope.df[,3]+cope.df[,5]+cope.df[,7]+cope.df[,9]+cope.df[,11]+cope.df[,13]+cope.df[,15]+cope.df[,16])/7
  aov.cope.smlspread=aov(cope.df[,1]~cope.df[,2]+cope.df[,4]+cope.df[,6]+cope.df[,2]*cope.df[,4]+cope.df[,2]*cope.df[,6]+
                           cope.df[,4]*cope.df[,6]+cope.df[,2]*cope.df[,4]*cope.df[,6])
  TukeyHSD(aov.cope.smlspread)
  Tukey.cope.smlspread=TukeyHSD(aov.cope.smlspread)
  
  
  CT.diff.smlspread[i]=Tukey.cope.smlspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,1]	#CT elevated present vs ambient present - diff
  CT.pval.smlspread[i]=Tukey.cope.smlspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[22,4]	#CT elevated present vs ambient present - pval
  
  
  ME.diff.smlspread[i]=Tukey.cope.smlspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,1]	#ME elevated present vs ambient present - diff
  ME.pval.smlspread[i]=Tukey.cope.smlspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[52,4]	#ME elevated present vs ambient present - pval 
  
  NJ.diff.smlspread[i]=Tukey.cope.smlspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,1]	#NJ elevated present vs ambient present - diff
  NJ.pval.smlspread[i]=Tukey.cope.smlspread$`cope.df[, 2]:cope.df[, 4]:cope.df[, 6]`[66,4]	#NJ elevated present vs ambient present - pval
}

#data check
CT.diff.smlspread
#
exp.vs.obs.df<- data.frame(matrix(0, 72, 3))
exp.vs.obs.df

colnames(exp.vs.obs.df)<- c("Expected","Observed", "Exp-Obs")
exp.vs.obs.df
predict(aov.cope.smlspread)	

exp.vs.obs.df[,1]<-cope.df[,1]

exp.vs.obs.df[,2]<-predict(aov.cope.smlspread)

exp.vs.obs.df[,3]<-exp.vs.obs.df[,1]-exp.vs.obs.df[,2]

exp.vs.obs.df


#
#summarize simulation
#mean(CT.diff.smlspread)
#mean(CT.pval.smlspread)
#mean(ME.diff.smlspread)
#mean(ME.pval.smlspread)
#mean(NJ.diff.smlspread)
#mean(NJ.pval.smlspread)

#Populate Results Table

pop.results.df[7,1]="smlspread"
pop.results.df[7,2]=1
pop.results.df[7,3]=9
pop.results.df[7,4]<-mean(ME.diff.smlspread)
pop.results.df[7,5]<-mean(ME.pval.smlspread)
pop.results.df[7,6]<-mean(CT.diff.smlspread)
pop.results.df[7,7]<-mean(CT.pval.smlspread)
pop.results.df[7,8]<-mean(NJ.diff.smlspread)
pop.results.df[7,9]<-mean(NJ.pval.smlspread)
pop.results.df

######Now to summarize this you need to graphically show the distribution of effect sizes and the percent time it is significant for each run####
hist(ME.diff.smlspread, main="Distribution = smlspread \n ME*Present*Ambient vs ME*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(CT.diff.smlspread, main="Distribution = smlspread \n CT*Present*Ambient vs CT*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")
hist(NJ.diff.smlspread, main="Distribution = smlspread \n NJ*Present*Ambient vs NJ*Present*Elevated", xlab= "Effect Size", ylab= "Frequency")

###Relative frequency of significant p values

sum(ME.pval.smlspread<.05)
ME.pval.smlspread	
#68.3%

sum(CT.pval.smlspread<.05)
CT.pval.smlspread
#31.3%


sum(NJ.pval.smlspread<.05)
NJ.pval.smlspread
#8.6%

#######I expect ME and CT to be more significant than this based on prior knowledge


#########Polish Figures for Writeup########


##Simulation Summary Table
pop.results.df

library(xlsx)
write.xlsx(pop.results.df, file="C:\\Users\\Lauren\\Desktop\\Gradschool\\UVM\\Quantitative thinking for Life Sciences\\summary table.xlsx", sheetName="Sheet1")

##Effect Size Histograms

##Initial conditions

par(mfrow=c(1,3)) 
hist(ME.diff, main="Maine \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(CT.diff, main="Connecticut \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(NJ.diff, main="New Jersey \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)




##Error 6

par(mfrow=c(1,3))
hist(ME.diff.ef6, main="Maine \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(CT.diff.ef6, main="Connecticut \n p < 0.05 99.9%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(NJ.diff.ef6, main="New Jersey \n p < 0.05 97.8%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)

##Error 9

par(mfrow=c(1,3))
hist(ME.diff.ef9, main="Maine \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(CT.diff.ef9, main=" Connecticut \n p < 0.05 91.4%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(NJ.diff.ef9, main=" New Jersey \n p < 0.05 66.5%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)

##RF 0.75

par(mfrow=c(1,3))
hist(ME.diff.rf75, main="Maine \n p < 0.05 99.9%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(CT.diff.rf75, main=" Connecticut \n p < 0.05 58%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(NJ.diff.rf75, main=" New Jersey \n p < 0.05 32%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)

##RF 0.50

par(mfrow=c(1,3))
hist(ME.diff.rf5, main="Maine \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(CT.diff.rf5, main=" Connecticut \n p < 0.05 57.3%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(NJ.diff.rf5, main=" New Jersey \n p < 0.05 33.4%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)

##lrgspread

par(mfrow=c(1,3))
hist(ME.diff.lrgspread, main="Maine \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(CT.diff.lrgspread, main="Connecticut \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(NJ.diff.lrgspread, main="New Jersey \n p < 0.05 100%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)

##smlspread

par(mfrow=c(1,3))
hist(ME.diff.smlspread, main="Maine \n p < 0.05 68.3%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(CT.diff.smlspread, main="Connecticut \n p < 0.05 31.3%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)
hist(NJ.diff.smlspread, main="New Jersey \n p < 0.05 8.6%", xlab= "Effect Size", ylab= "Frequency", cex.main=2,cex.lab=1.5, cex.axis=1.5)

citation()