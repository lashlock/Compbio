install.packages("animation")
library(animation)
#ani.options = argument for the animation parameters (nested within)

#interval = a positive number to set the time interval of the animation (unit in seconds); default is 1, which is very slow

#nmax = maximum number of steps in a loop (or iterations) to create animation frames. The actual number of frames can be less than nmax, depending on the animation. The default is 50

#ani.width & ani.height = width and height of the image frames (unit in pixels) typically for png, jpeg; default is 480; units might differ between exporting formats

#ani.pause =  it will pause for a time interval  (specified in ani.options('interval')) 



#2-faced coin with equal probability (0.5 or NULL). 
#quartz()
flip.coin(faces = 2, prob = NULL, border = "white", grid = "white", col = 1:2,
          type = "p", pch = 21, bg = "transparent", digits = 3) 

#2-faced coin with unequal probability (0.9/0.1).
flip.coin(faces = 2, prob = c(0.9, 0.1), border = "white", grid = "white", col = 1:2,
          type = "p", pch = 21, bg = "transparent", digits = 3) 

#Coin with 3 potential outcomes
oopt = ani.options(interval = 0.3, nmax = ifelse(interactive(), #if else(interactive() meaning assumed human operator
                                                 50, 2))

## A simulation where a coin could either be heads, tails or "stand on a table"
flip.coin(faces = c("Head", "Stand", "Tail"), type = "n", prob = c(0.475, 0.05, 0.475), col = c(1, 2, 4))

## Save as a HTML animation page (links to your working directory)
saveHTML({
  ani.options(interval = 0.3, nmax = ifelse(interactive(), 50, 
                                            2)) 
  par(mar = c(2, 3, 2, 1.5), mgp = c(1.5, 0.5, 0))
  flip.coin(faces = c("Head", "Stand", "Tail"), type = "n", 
            prob = c(0.475, 0.05, 0.475), col = c(1, 2, 4))
}, img.name = "flip.coin", htmlfile = "flip.coin.html", ani.height = 500, 
ani.width = 600, title = "Probability of flipping coins", 
description = c("This animation has provided a simulation of flipping coins", "which might be helpful in understanding the concept of probability."))


# Suppose there are two plant species in a field: A and B. 
# One of them will die at each time and a new plant will grow in the place where the old plant died. 
# The species of the new plant depends on the proportions of two species: the larger the proportion is, the greater the probability for this species to come up will be

oopt = ani.options(nmax = ifelse(interactive(), 25, 2), interval = 0.3)
par(ann = FALSE, mar = rep(0, 4))
ecol.death.sim(nr = 10, nc = 10, num.sp = c(50, 50), col.sp = c(8, 2), pch.sp = c(20,17), col.die = 1, pch.die = 4, cex = 3)

## Save as a HTML animation page (links to your working directory)
saveHTML({
  ani.options(interval = 0.3, nmax = ifelse(interactive(), 25, 
                                            2)) 
  par(ann = FALSE, mar = rep(0, 4))
  ecol.death.sim(nr = 10, nc = 10, num.sp = c(50, 50), col.sp = c(8, 2), pch.sp = c(20,17), col.die = 1, pch.die = 4, cex = 3)
}, img.name = "ecol.death.sim", htmlfile = "ecol.death.sim.html", ani.height = 500, 
ani.width = 600, title = "Ecological Death Simulation", 
description = c("This animation shows the simulation of the death of two species with certain probabilities."))
#ani.options(oopt)

#Changing the proportion of species/probability of species replacing another: prob = 0.25/0.75
ecol.death.sim = function(
  nr = 10, nc = 10, num.sp = c(50, 50), col.sp = c(1, 2), pch.sp = c(1, 2),
  col.die = 1, pch.die = 4, cex = 3, ...
) {
  x = rep(1:nc, nr)
  y = rep(1:nr, each = nc)
  p = factor(sample(c(rep(1, 25), rep(2,75))), levels = 1:2)
  nmax = ani.options('nmax')
  for (i in 1:nmax) {
    dev.hold() #this gives a way to hold/flush output on certain on-screen devices
    plot(1:nc, 1:nr, type = 'n', xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5), ...)
    abline(h = 1:nr, v = 1:nc, col = 'lightgray', lty = 3)
    points(x, y, col = col.sp[p], pch = pch.sp[p], cex = cex)
    ani.pause()
    idx = sample(nr * nc, 1)
    points(x[idx], y[idx], pch = pch.die, col = col.die, cex = cex, lwd = 3)
    tbl = as.vector(table(p))
    tbl = tbl + if (as.integer(p[idx]) > 1) c(0, -1) else c(-1, 0)
    p[idx] = sample(1:2, 1, prob = tbl)
    ani.pause()
  }
  invisible(p)
}

ecol.death.sim()

oopt = ani.options(interval = 1.5)
saveHTML({
  for (i in 1:5) {
    plot(runif(10), ylim = c(0, 1))
    ani.pause()
  }
}, htmlfile = "simple.plot.html", img.name = "simple.plot")

RanWalk <- function(times=100, 
                    n1=50,
                    lambda=1.001, 
                    noiseSD=10) {
  n <- rep(NA, times)
  n[1] <- n1
  noise <- rnorm(n=times, mean=0, sd=noiseSD)
  
  for (i in 1:(times-1)) {
    n[i + 1] <- lambda*n[i] + noise[i] #would write in a manusript as n(t+1) = lambda(nt) + e
    if(n[i + 1] <= 0){
      n[i+ 1] <- NA
      cat("Population extinction at time", 
          i-1, "\n")
      #tkbell()
      break} #end of conditional statement
  } #end of for loop
  return(n)
} #end of function
head(RanWalk())

saveHTML({
  for (i in 1:10) {
    plot(RanWalk(), type="o")
    ani.pause()
  }
}, htmlfile = "RanWalk.html", img.name = "RanWalk.plot", description = "Animation of stochastic random walks (model population growth)")

#For this example expand your plot window!
#This animation plots the density functions of 150 draws of 100 values from a normally distributed random variable
#Set delay between frames when replaying
ani.options(interval=.05)

# Set up a vector of colors for use below 
col.range <- heat.colors(15)

# Begin animation loop
saveHTML({
  layout(matrix(c(1, rep(2, 5)), 6, 1))
  par(mar=c(3,3,1,1) + 0.1)
  for (i in 1:150) { # Begin the loop that creates the 150 individual graphs
    chunk <- rnorm(100)+sqrt(abs((i)-51))  # Pull 100 observations from a normal distribution
    # and add a constant based on the iteration to move the distribution
    par(fg=1) # Reset the color of the top chart every time (so that it doesn't change as the 
    # bottom chart changes)
    # Set up the top chart that keeps track of the current frame/iteration
    plot(-5, xlim = c(1,150), ylim = c(0, .3), axes = F, xlab = "", ylab = "", main = "Iteration") 
    abline(v=i, lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    abline(v=i-1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    abline(v=i-2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    # Bring back the X axis
    axis(1)
    # Set the color of the bottom chart based on the distance of the distribution's mean from 0
    par(fg = col.range[mean(chunk)+3])
    # Set up the bottom chart
    plot(density(chunk), xlab = "x-value", xlim = c(-5, 15), ylim = c(0, .6))
    # Add a line that indicates the mean of the distribution. Add additional lines to track
    # previous means
    abline(v=mean(chunk), col = rgb(255, 0, 0, 255, maxColorValue=255))
    if (exists("lastmean")) {abline(v=lastmean, col = rgb(255, 0, 0, 50, maxColorValue=255)); prevlastmean <- lastmean;}
    #Fix last mean calculation
    lastmean <- mean(chunk)
  }
}, img.name = "density_normal_dist", htmlfile = "normal.dist.html", ani.height = 500, 
ani.width = 600, title = "Normal Distribution", 
description = c("This animation plots the density functions of 150 draws of 100 values from a normally distributed random variable.") )

EcoSimR program

# read in associated species data 
sppDat <- read.table("AssociatedSppData_Serp.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
head(sppDat)

install.packages("EcoSimR")
install.packages("MASS")
# reshape data using dcast function in reshape2 package
library(reshape2)
PA <- dcast(sppDat,formula=SpeciesName~SitePatch)
head(PA)

library(EcoSimR)
library(MASS)
# Run null model with SIM9 algorithm & CHECKER index
adMod1 <- cooc_null_model(PA,algo= "sim9",metric="checker",nReps=1000,suppressProg=T)

# Summary and plots
summary(adMod1)

plot(adMod1,type="hist")


plot(adMod1,type="cooc")

plot(adMod1,type="burn_in")




#install.packages("plyr")
library(plyr)

climate <- read.csv(file="ClimateData.csv")
climate$Month <- as.factor(climate$Month)
climate$Year <- as.factor(climate$Year)
str(climate)

# Use 'ddply' to calculate the mean air temperature for each month
ddply(climate, # input data frame
      "Month", # variable to subset by
      function(x){ # function to run on each subset
        mean(x$AvgAirTemp)
      }
)

# Alter the function slightly so that the output is easier to work with
monthlyData <- ddply(climate,
                     "Month",
                     function(x){ 
                       MeanAirTemp <- mean(x$AvgAirTemp)
                       data.frame(MeanAirTemp=MeanAirTemp)
                     }
)
print(monthlyData)


# Use 'ddply' to calculate the mean air temperature for each month-year combination
monthYearData <- ddply(climate,
                       c("Month","Year"),
                       function(x){ 
                         MeanAirTemp <- mean(x$AvgAirTemp)
                         data.frame(MeanAirTemp=MeanAirTemp)
                       }
)
print(monthYearData)


# Use 'ddply' to calculate, for each month, means and standard deviations for daily air
# temperature and precipitation
monthlyData <- ddply(climate,
                     "Month",
                     function(x){ 
                       
                       meanAirTemp <- mean(x$AvgAirTemp)
                       sdAirTemp <- sd(x$AvgAirTemp)
                       meanPrecip <- mean(x$Precip)
                       sdPrecip <- sd(x$Precip)
                       
                       data.frame(meanAirTemp=meanAirTemp,sdAirTemp=sdAirTemp,
                                  meanPrecip=meanPrecip,sdPrecip=sdPrecip)
                     }
)
print(monthlyData)


# Calculate the mean air temperature for each month
monthlyData <- ddply(climate, # input data frame 
                     "Month", # variable to subset by
                     summarise, # "helper function" to run
                     MeanAirTemp = mean(AvgAirTemp)) # function to apply to each subset
print(monthlyData)

# Calculate the mean air temperature for each month-year combination
monthYearData <- ddply(climate, 
                       c("Month","Year"), 
                       summarise, 
                       MeanAirTemp = mean(AvgAirTemp))
print(monthYearData)


# Calculate, for each month, the mean and standard deviation for air temperature 
monthlyData <- ddply(climate, 
                     "Month", 
                     summarise, 
                     meanAirTemp=mean(AvgAirTemp), 
                     sdAirTemp=sd(AvgAirTemp))
print(monthlyData)


# Use 'transform' within 'ddply' to split your data into subsets, perform a calculation on
# each subset, and add the results to a copy of your input data frame as a new column
x <- ddply(climate, 
           "Month", 
           transform, 
           MonthlyMeanTemp = mean(AvgAirTemp))
head(x)

# 'Mutate' works similarly to 'transform', but allows you to do calculations within 'ddply'
# using columns you just created
x <- ddply(climate, 
           "Month", 
           mutate, 
           AvgMaxTemp = mean(MaxAirTemp),
           AvgMinTemp = mean(MinAirTemp),
           MonthlyMeanTempRange = AvgMaxTemp - AvgMinTemp)
head(x)

# Create boxplots of the mean daily air temperatures for each weather station
par(mfrow = c(1,2))
d_ply(climate, 
      "StationName", 
      summarise, 
      boxplot(AvgAirTemp, 
              xlab=unique(StationName), 
              ylab="Mean Daily Air Temperature (degrees C)"))



# Use a linear model to examine how precipitation changes with air temperature within 
# each month
precipTemp <- ddply(climate, 
                    "Month", 
                    function(x) {
                      model <- lm(Precip ~ AvgAirTemp, data=x)
                      setNames(coef(model), c("Intercept", "Slope"))
                    }
)
print(precipTemp)


# For each month, calculate and graph the mean and standard deviation for air temperature 

monthlyData <- ddply(climate, 
                     "Month", 
                     summarise, 
                     meanAirTemp=mean(AvgAirTemp), 
                     sdAirTemp=sd(AvgAirTemp))
print(monthlyData)


library(ggplot2)


p <- ggplot(monthlyData, aes(x=Month,y=meanAirTemp,colour=Month))
p <- p + geom_point(position=position_dodge(width=0.3), stat="identity", size = 3) 
p <- p + geom_errorbar(aes(ymin=meanAirTemp-sdAirTemp, ymax=meanAirTemp+sdAirTemp),
                       width=.1,position=position_dodge(.3))
print(p)

#################################################################

install.packages("vegan")
library(vegan)

data(package = "vegan") ## names of data sets in the package
data(dune) # Vegetation and Environment in Dutch Dune Meadows
str(dune) #a data frame of observations of 30 species at 20 sites

diversity(dune,index = "simpson") # calculate Simpson's 1-D Index of Diversity 
#for each site.  closer to 1 = greater diversity

simpson <- diversity(dune, "simpson") # or assign to var.
simpson 

shannon <- diversity(dune) # note that Shannon's is default
shannon #Typically ranges from 1.5 - 3.4, higher = more diverse 

# lets compare the two
par(mfrow = c(1, 2))  # use par to generate panels with 1 row of 2 graphs
hist(simpson)
hist(shannon)

par(mfrow = c(1, 2))
bray = vegdist(dune, "bray") 
gower = vegdist(dune, "gower")
hist(bray, xlim = range(0.0,1.0))
hist(gower, xlim = range(0.0,1.0))

spAbund <- rowSums(dune)  #gives the number of individuals found in each plot
spAbund # view observations per plot 

raremin <- min(rowSums(dune))  #rarefaction uses the smallest number of observations per sample to extrapolate the expected number if all other samples only had that number of observations
raremin # view smallest # of obs (site 17)

sRare <- rarefy(dune, raremin) # now use function rarefy
sRare #gives an "expected"rarefied" number of species (not obs) if only 15 individuals were present

rarecurve(dune, col = "blue") # produces rarefaction curves # squares are site numbers positioned at observed space. To "rarefy" a larger site, follow the rarefaction curve until the curve corresponds with the lesser site obs. This gives you rarefied species richness


#Non metric MDS

set.seed(2) # random no. generator / way to specify seeds, 2=no. of integers?
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10, # counts up to 100, 300 cells
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))
head(community_matrix)

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions. Increase if high stress is problem. 




par(mfrow=c(1,1))

plot(example_NMDS)

ordiplot(example_NMDS,type="n") #Ordination plot function especially for congested plots
orditorp(example_NMDS,display="species",col="red",air=0.01) #The function adds text or points to ordination plots
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)

treat=c(rep("Treatment1",5),rep("Treatment2",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

#spider plot
ordiplot(example_NMDS,type="n")
ordispider(example_NMDS,groups=treat)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

# Define random elevations for previous example
elevation=runif(10,0.5,1.5)
# Use the function ordisurf to plot contour lines
ordisurf(example_NMDS,elevation,main="",col="forestgreen")


# Finally, display species on plot
orditorp(example_NMDS,display="species",col="grey30",air=0.1,
         cex=1)

############ randomForest #######################

# Begin by installing the randomForest package
#install.packages("randomForest")
library("randomForest")

#### EXAMPLE 1: Iris data ####
# Call to "iris" data set available in R 
data(iris) 
View(iris) 
str(iris) # numeric predictor variables, Species variable is catagorical
summary(iris)

# Store iris in a new data fram 

Dframe <- iris

# Let's get started 
set.seed(123) # to get reproducible random results

# Split iris data to training data and testing data
# Train the model with 70% of data and test it 
# with the remaing %30 of the data. 
help(sample)
spl <- sample(2,nrow(Dframe),replace=TRUE,prob=c(0.7,0.3)) 
print(spl)

str(spl)


# define the training data 
trainData <- Dframe[spl==1,]
head(trainData)

# Test data 
testData <- Dframe[spl==2,]
head(testData)

# Generate random forest with training data 
irisRF <- randomForest(Species~.,data=trainData, mtry= 3, ntree=200,proximity=TRUE) 
help(randomForest)

# Print Random Forest model and see the importance features
print(irisRF)

# Confusion matrix for train data
table(predict(irisRF),trainData$Species) 

# Plot random forest 
plot(irisRF)

# Look at importance of independant vars
importance(irisRF)

# Plot importance 
varImpPlot(irisRF)

# Now build random forest for testing data
help("predict.randomForest")

irisPred <- predict(irisRF,newdata=testData)
print(irisPred)


table(irisPred, testData$Species)

#Now, let's look at the margin, positive or negative,
# if positive it means correct classification
help("margin.randomForest")
plot(margin(irisRF,testData$Species))


library(shiny)
ui <- fluidPage(
  selectInput(inputId= "num", # unlike the first example of a histogram of random numbers, we can use a drop-down menu rather than a slider. Easy fix by replacing sliderInput with selectInput and change values to choices
              label = "Choose a number",
              choices = c(10,20,30,40,50,60,70,80,90,100)), # choices reflect what values the user can select
  plotOutput("hist"))

server <- function(input, output) {
  output$hist <- renderPlot({
    title <- "random normal values"
    hist(rnorm(input$num), main= title)})
}

shinyApp(ui = ui, server = server)


########################R tools#############################

# graphics.off();
# rm(list=ls());
# ############################################################################
# # using the library RCUrl
# install.packages("RCurl")
# library(RCurl)
# 
# getData = function(){
#   
#   output = getURL() # gets the data
#   # return the output
#   return()
# }
# 
# ############################################################################
# # read user input
# read_dw = function(){
#   w = readline() # input of the user URL (where to DL data)
#   return() # return the web URL
# }
# 
# write.csv() # output the data to your directory


#Download data from NOAA or the USGS

# graphics.off();
# rm(list=ls());
# ############################################################################
# # # using the library RCUrl
# # install.packages("RCurl")
# # library(RCurl)
# ############################################################################
# 
# readInputs = function(){
#   out = readlines()
#   return(out)
# }
# 
# getWLcsv.USGS = function(site_no, start_date, end_date, delim = "\t"){
#   
#   web = "https://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no=01509000&referred_module=sw&period=&begin_date=1996-01-01&end_date=2016-12-31"
#   
#   x = getURL(web)
#   
#   return(x)
# }
# 
# for(d in 1:length(dates)){
#   
#   data = getWLcsv.USGS()
#   
#   write.csv()
# }







































