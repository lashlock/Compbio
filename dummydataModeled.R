#Updated model of dummy data

###Each xvar relates to your yvar (hatch rate)

### xvars = Temp, HAB, Pop, TempXPop, TempXHAB, HABXPop, TempXHABXPop

###Steps...

##Make a data frame - Columns for yvar, xvars, and yerror - rows for sample size
#12 outputs X 6 replicates = 72 y outputs -> 72 rows
# 8 yvars + 7 xvars + 1 error column = 16 columns
cope.df<- data.frame(matrix(0, 72, 16))
head(cope.df)
tail(cope.df)

colnames(cope.df)<- c("Fecundity","Temp","Y1", "HAB", "Y2", "Pop", "Y3", "TempXPop", "Y4", "TempXHAB", "Y5", "HABXPop", "Y6", "TempXHABXPop", "Y7", "Error" )
head(cope.df)
tail(cope.df)

##Use distributions to populate dummy data for xvars
#Temp will either be ambient or elevated for each pop

cope.df[1:12,2]<-"ambient"
cope.df[13:24,2]<-"elevated"
cope.df[25:36,2]<-"ambient"
cope.df[37:48,2]<-"elevated"
cope.df[49:60,2]<-"ambient"
cope.df[61:72,2]<-"elevated"
head(cope.df)
tail(cope.df)

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
tail(cope.df)

#population will be either ME, CT, or NJ

cope.df[1:24,6]<-"ME"
cope.df[25:48,6]<-"CT"
cope.df[49:72,6]<-"NJ"
head(cope.df)
tail(cope.df)

#TempXPop
#MEXambient, MEXelevated, CTXambient, CTXelevated, NJXambient, NJXelevated

cope.df[1:12,8]<-"MEXambient"
cope.df[13:24,8]<-"MEXelevated"
cope.df[25:36,8]<-"CTXambient"
cope.df[37:48,8]<-"CTXelevated"
cope.df[49:60,8]<-"NJXambient"
cope.df[61:72,8]<-"NJXelevated"
head(cope.df)
tail(cope.df)

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
head(cope.df)
tail(cope.df)

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
head(cope.df)
tail(cope.df)


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
head(cope.df)
tail(cope.df)

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

for(t in 1:72){
  HAB.func<- {if (cope.df[t,4]=="absent") {
    cope.df[t,5]=25
  } else {
    cope.df[t,5]=15
  }
  }
}
head(cope.df)
tail(cope.df)

##f(pop)

pop.func<-20                                                                                                                                                                                                                                                 
cope.df[,7]<-20+rnorm(mean=0, sd=1, n=72) #population will be fecundity avg plus noise


##f(tempxpop)
tempxpop.func<-20
cope.df[,9]<-20+rnorm(mean=0, sd=1, n=72)

head(cope.df)
tail(cope.df)

##f(tempxHAB)

for(t in 1:72){
  tempxHAB.func<- {(if(cope.df[t,10]=="ambientXabsent"){
    cope.df[t,11]=25}) + (if(cope.df[t,10]=="ambientXpresent"){
      cope.df[t,11]=15}) + (if(cope.df[t,10]=="elevatedXabsent"){
        cope.df[t,11]=15}) + (if(cope.df[t,10]=="elevatedXpresent"){
          cope.df[t,11]=10})
  }
}
#the avg for this is quite low...

head(cope.df)
tail(cope.df)


##f(HABxpop)

for(t in 1:72){
  HABxpop.func<-{(if(cope.df[t,12]=="absentXME"){
    cope.df[t,13]=25}) + (if(cope.df[t,12]=="presentXME"){
      cope.df[t,13]=19}) + (if(cope.df[t,12]=="absentXCT"){
        cope.df[t,13]=25}) + (if(cope.df[t,12]=="presentXCT"){
          cope.df[t,13]=15}) + (if(cope.df[t,12]=="absentXNJ"){
            cope.df[t,13]=25}) + (if(cope.df[t,12]=="presentXNJ"){
              cope.df[t,13]=13})
  }
}

head(cope.df)
tail(cope.df)

# This mean is right on point

##f(TxHxP)

for(t in 1:72){
  txhxp.func<-{(if(cope.df[t,14]=="ambientXabsentXME"){
    cope.df[t,15]=25}) + (if(cope.df[t,14]=="ambientXpresentXME"){
      cope.df[t,15]=19}) + (if(cope.df[t,14]=="elevatedXabsentXME"){
        cope.df[t,15]=15}) + (if(cope.df[t,14]=="elevatedXpresentXME"){
          cope.df[t,15]=15}) + (if(cope.df[t,14]=="ambientXabsentXCT"){
            cope.df[t,15]=25}) + (if(cope.df[t,14]=="ambientXpresentXCT"){
              cope.df[t,15]=15}) + (if(cope.df[t,14]=="elevatedXabsentXCT"){
                cope.df[t,15]=15}) + (if(cope.df[t,14]=="elevatedXpresentXCT"){
                  cope.df[t,15]=13}) + (if(cope.df[t,14]=="ambientXabsentXNJ"){
                    cope.df[t,15]=25}) + (if(cope.df[t,14]=="ambientXpresentXNJ"){
                      cope.df[t,15]=13}) + (if(cope.df[t,14]=="elevatedXabsentXNJ"){
                        cope.df[t,15]=15}) + (if(cope.df[t,14]=="elevatedXpresentXNJ"){
                          cope.df[t,15]=10})
  }
}
head(cope.df)
tail(cope.df)

# This mean is quite low


##Calculate error for y data
########################When SD = 15 and Variables explain 40%#######################

cope.df[,16]<-rnorm(mean=0, sd=9, n=72)

##Relationship factor of 0.5
##you're adding in new columns here with the new relationship factor...
##need to refresh my memory about this

rf=.5

cope.df$f.Y1=(cope.df$Y1-mean(cope.df$Y1))*rf+mean(cope.df$Y1)
cope.df$f.Y2=(cope.df$Y2-mean(cope.df$Y2))*rf+mean(cope.df$Y2)
cope.df$f.Y3=(cope.df$Y3-mean(cope.df$Y3))*rf+mean(cope.df$Y3)
cope.df$f.Y4=(cope.df$Y4-mean(cope.df$Y4))*rf+mean(cope.df$Y4)
cope.dff.Y5=(cope.df$Y5-mean(cope.df$Y5))*rf+mean(cope.df$Y5)
cope.df$f.Y6=(cope.df$Y6-mean(cope.df$Y6))*rf+mean(cope.df$Y6)
cope.df$f.Y7=(cope.df$Y7-mean(cope.df$Y7))*rf+mean(cope.df$Y7)
head(cope.df)

#data check
mean(cope.df$Y1)
mean(cope.df$f.Y1)

#looks okay, so run your model

cope.df[,1]=(cope.df[,17]+cope.df[,18]+cope.df[,19]+cope.df[,20]+cope.df[,21]+cope.df[,22]+cope.df[,16])/7
cope.df

write.table(x=cope.df, file="copeDD", sep=" ")

dataCheck <- read.csv(file="copeDD", header=TRUE, sep=" ")

dataCheck

#this works

##You can continue to work with the means but this looks good for the 
#purposes of hw 9


