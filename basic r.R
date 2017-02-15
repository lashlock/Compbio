##Homework 5
##2/15/17
##Lauren Ashlock

#Question 1
#Suppose x = 1.1, a = 2.2, and b = 3.3. Assign each expression to the value of the variable z and 
#print the value stored in z.

##Assigning values to x, a, and b

x<- 1.1

a<- 2.2

b<- 3.3

#pt a

z<- x^a^b
z
#output
#[1] 3.61714

#pt b

z<- (x^a)^b
z
#output
#[1] 1.997611

#pt c

z<- 3*x^3 + 2*x^2 + 1
z
#output
#[1] 7.413

#pt d

floor((z%%1)*10)
#output
#[1] 4
#the z%%1 will grab everything after the 1s place (all decimals)
#the floor(*10) grabs the digit in the first decimal place





#problem 2 Using the rep and seq functions create the following vectors

#pt a (1,2,3,4,5,6,7,8,7,6,5,4,3,2,1)


SEQ1<- seq(1,8,1)
SEQ2<- seq(7,1,-1)
SEQ3<-c(SEQ1,SEQ2)
SEQ3
#output
# [1] 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1


#pt b (1,2,2,3,3,3,4,4,4,4,5,5,5,5,5)

c(1,rep(2,2),rep(3,3),rep(4,4),rep(5,5))

#output
#[1] 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5


#pt c (5,4,4,3,3,3,2,2,2,2,1,1,1,1,1)

c(5,rep(4,2),rep(3,3),rep(2,4),rep(1,5))
#output
#[1] 5 4 4 3 3 3 2 2 2 2 1 1 1 1 1


#question 3
#Create a vector of two random uniform numbers. In a spatial map, these can be interpreted as x and y coordinates 
#that give the location of an individual (such as a marked forest tree in a plot that has been mapped). 
#Using one of R’s inverse trigonometry functions (asin(), acos(), or atan()), convert these numbers into 
#polar coordinates (If you don’t know what polar coordinates are, read about them on the web or in your calculus 
#textbook).
xy<-c(runif(2))
xy
#output
#[1] 0.2168062 0.5739645


theta<-atan(xy[2]/xy[1])
theta

r<- sqrt((xy[2])^2+(xy[1])^2)
r

polarcoord<- c(r,theta)
polarcoord

#output
#[1] 0.6135472 1.2096304

#Question 4 Suppose that queue <- c("sheep", "fox", "owl", "ant") and that queue represents the animals 
#that are lined up to enter Noah’s Ark, with the sheep at the front of the line. Using R expressions, 
#update the queue successively as

queue<- c("sheep","fox","owl","ant")
queue
#output
#[1] "sheep" "fox"   "owl"   "ant" 

#pt a the serpent arrives;

append(queue,"serpent")
queue[5]<-"serpent"
#output
#[1] "sheep"   "fox"     "owl"     "ant"     "serpent"

#pt b the sheep enters the ark;
queue<- queue[2:5]
queue
#output
#[1] "fox"     "owl"     "ant"     "serpent"

#pt c the donkey arrives and talks his way to the front of the line;
queue<- c("donkey",queue)
queue
#output
[1] "donkey"  "fox"     "owl"     "ant"     "serpent"

#pt d the serpent gets impatient and leaves;
queue<-queue[1:4]
queue
#output
#[1] "donkey" "fox"    "owl"    "ant" 


#pt e the owl gets bored and leaves;
queue<-queue[-3]
queue
#output
#[1] "donkey" "fox"    "ant" 


#pt f the aphid arrives and the ant invites him to cut in line.
queue<-c(queue[1:2],"aphid","ant")
queue
#output
#[1] "donkey" "fox"    "aphid"  "ant"




#pt g Finally, determine the position of the aphid in the line.

which(queue=="aphid")

#output
#[1] 3


#Question 5 Use R to create a vector of all of the integers from 1 to 100 that are not divisible by 2, 3, or 7


vec<- seq(1,100)
vec
vec2<- (vec %% 2!=0)&(vec %% 3!=0)&(vec %% 7!=0)

final<-vec[vec2]
final
#output
# [1]  1  5 11 13 17 19 23 25 29 31 37 41 43 47 53 55 59 61 65 67 71 73 79 83 85 89 95 97


#Question 6 Create a vector z of 1000 random uniform numbers

z<- runif(1000)
z


#pt a create a vector that contains 3 numbers: the proportion 
#of the numbers in z that are less than 0.10, greater than 0.90, and between 0.45 and 0.55.


a<-which(z<0.1)

a<-length(a)
a
element1<-a/1000
element1

b<-which(z<0.9)

b<-length(b)
b
element2<-b/1000
element2

c<-which((z>.45)&(z<.55))
c<-length(c)
c

element3<-c/1000
element3


propvec<-c(element1,element2,element3)

propvec
#output
#[1] 0.093 0.884 0.093

#pt b Making successive copies of z, transform your vector of uniform numbers in the following ways:
##pt c for each case calculate your vector of 3 numbers to get the new proportions.
#log (base 10) of z
z<-log(z)
head(z)
#output
#[1] -2.34846594 -2.12129607 -0.72273758 -0.09531328 -0.36719235 -1.11207483

a<-which(z<0.1)

a<-length(a)
a
element1<-a/1000
element1

b<-which(z<0.9)

b<-length(b)
b
element2<-b/1000
element2

c<-which((z>.45)&(z<.55))
c<-length(c)
c

element3<-c/1000
element3


logzpropvec<-c(element1,element2,element3)

logzpropvec
#output
#[1] 0.354 0.729 0.038

#z^2
z<- z^2
head(z)
#output
#[1] 5.515292275 4.499897027 0.522349611 0.009084621 0.134830225 1.236710429

a<-which(z<0.1)

a<-length(a)
a
element1<-a/1000
element1

b<-which(z<0.9)

b<-length(b)
b
element2<-b/1000
element2

c<-which((z>.45)&(z<.55))
c<-length(c)
c

element3<-c/1000
element3


zsquarepropvec<-c(element1,element2,element3)

zsquarepropvec
#output
#[1] 0.546 0.740 0.016

#e^z
z<-exp(1)^z
head(z)
#output
#248.462586  90.007862   1.685984   1.009126   1.144342   3.444265

a<-which(z<0.1)

a<-length(a)
a
element1<-a/1000
element1

b<-which(z<0.9)

b<-length(b)
b
element2<-b/1000
element2

c<-which((z>.45)&(z<.55))
c<-length(c)
c

element3<-c/1000
element3


ezpropvec<-c(element1,element2,element3)

ezpropvec
#output
#[1] 0 0 0



#square root of z

z<-sqrt(z)
head(z)
#output
#[1] 15.762696  9.487247  1.298455  1.004553  1.069739  1.855873

a<-which(z<0.1)

a<-length(a)
a
element1<-a/1000
element1

b<-which(z<0.9)

b<-length(b)
b
element2<-b/1000
element2

c<-which((z>.45)&(z<.55))
c<-length(c)
c

element3<-c/1000
element3


sqrtpropvec<-c(element1,element2,element3)

sqrtpropvec
#output
#[1] 0 0 0



































