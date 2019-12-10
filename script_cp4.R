

     4.Displaying Categorical Data
  
  4.1 Introduction

 #apply flexclust package 
data(btw2009, package = "flexclust")
 #Within function keeps track of the items in list and returns a new object 
btw2009 <- within (btw2009, stateA <- state) 
 # we insert the german states into the list
btw2009 <- within (btw2009,
levels(stateA) <- c("BW", "BY", "BE",
"BB", "HB", "HH", "HE", "MV", "NI", "NW",
"RP", "SL", "SN", "ST", "SH", "TH"))
 #display states by number of eligible voters in alphabetical order
Voters <- with(btw2009, size <- tapply(eligible, stateA, sum))
Bundesland <- rownames(Voters)
 # display states by number of voters
btw9s <- data.frame(Bundesland, Voters)
 # extract elements by name from a named list
btw9s$EW <- c("West")
 # insert the eastern states
btw9s[c("BB", "BE", "MV","SN","ST","TH"), "EW"] <- "East"
 #; numbers of eligible voters are aggregated by Bundesland
ls <- with(btw9s, Bundesland[order(EW, -Voters)])
 #display the states starting with the eastern ones
btw9s <- within(btw9s, State1 <- factor(Bundesland, levels=ls))
 #display the three barcharts
   # the default alphabetical order
b1 <- ggplot(btw9s, aes(Bundesland, Voters/1000000)) +
geom_bar(stat="identity") +
ylab("Voters (millions)")
   #ordered by decreasing number of voters
b2 <- ggplot(btw9s, aes(reorder(Bundesland, -Voters),
Voters/1000000)) + geom_bar(stat="identity") +
xlab("Bundesland") + ylab("Voters (millions)")
   #ordered by voter numbers within the former East/West Germanies
b3 <- ggplot(btw9s, aes(State1, Voters/1000000)) +
geom_bar(stat="identity") + xlab("Bundesland") +
ylab("Voters (millions)")
   # arrange the grids in specified order
grid.arrange(b1, b2, b3)
   4.2 What features might categorical variables have?

 #Unexpected patterns of results
Some categories missing completely
 #Uneven distributions
Too focused on certain groups
 #Extra categories
Genders M and F also m and f
 #Unbalanced experiments
Data missing might lead to unbalanced group sizes
 #Large numbers of categories
Too many categories
 #Don’t knows, refusals, errors, missings,


   4.3 Nominal data—no fixed category order

   #Meta analyses—how big was each study?
 
 #we use the meta-analsys package
data(Fleiss93, package="meta")
 #studies ordered by total number of patients ( control + experimental group)
 Fleiss93 <- within(Fleiss93, {
total <- n.e + n.c
st <- reorder(study, -(total)) })
 #display the barchart
ggplot(Fleiss93, aes(st, total)) + geom_bar(stat="identity") +
xlab("") + ylab("") + ylim(0,20000)
 #we group all the studies with less than 2000 cases
Fleiss93 <- within(Fleiss93, st1 <- as.character(study))
Fleiss93$st1[Fleiss93$total < 2000] <- "REST"
 # we display the small studies <2000 cases as one bar and the big studies
ggplot(Fleiss93, aes(st1, total)) + geom_bar(stat="identity") +
xlab("") + ylab("") + ylim(0,20000)
 #apply the MASS package
data(anorexia, package="MASS")
 # barchart of the group sizes in the anorexia dataset drawn with barplot
ggplot(anorexia, aes(Treat)) + geom_bar() + xlab("Treatment")
 #same thing with a table
with(anorexia, table(Treat))

 #Titanic barchart

 # we use the data we know from the Titanic sinking
Titanic1 <- data.frame(Titanic)
p <- ggplot(Titanic1, aes(weight=Freq)) +
ylab("") + ylim(0,2250)
 # the blue bar indicating the class of the people onboard
cs <- p + aes(Class) + geom_bar(fill="blue")
 # green bar reprezenting the people by sex
sx <- p + aes(Sex) + geom_bar(fill="green")
 # tan bar reprezenting people by age
ag <- p + aes(Age) + geom_bar(fill="tan2")
 # red bar reprezenting people who survived or died
su <- p + aes(Survived) + geom_bar(fill="red")
 # display the results
grid.arrange(cs, sx, ag, su, nrow=1, widths=c(3, 2, 2, 2))

 #2013 Irish opinion polls
 
 #we insert all the parties
Party <- c("Fine Gael", "Labour", "Fianna Fail",
"Sinn Fein", "Indeps", "Green", "Don’t know")
 # we insert the pool responses for each party
nos <- c(181, 51, 171, 119, 91, 4, 368)
 # we link the data
IrOP <- data.frame(Party, nos)
IrOP <- within(IrOP, {
 # we calculate where the 'Don't know' votes will go 
percwith <- nos/sum(nos)
percnot <- nos/sum(nos[-7])})
par(mfrow=c(2,1), mar = c(2.1, 2.1, 2.1, 2.1))
 # we display the piecharts
with(IrOP, pie(percwith, labels=Party, clockwise=TRUE,
col=c("blue", "red", "darkgreen", "black",
"grey", "lightgreen", "white"), radius=1))
with(IrOP, pie(percnot[-7], labels=Party, clockwise=TRUE,
col=c("blue", "red", "darkgreen", "black",
"grey", "lightgreen"), radius=1))

   4.4 Ordinal data—fixed category order

  #Surveys
 
# apply the BEPS dataset used for graded responses
data("BEPS", package="effects")
# results for William Hague conservative
a1 <- ggplot(BEPS, aes(factor(Hague))) +
geom_bar(fill="blue") + ylab("") +
xlab("Hague (Conservative)") + ylim(0, 900)
# results for Tony Blair labour
a2 <- ggplot(BEPS, aes(factor(Blair))) +
geom_bar(fill="red") + ylab("") +
xlab("Blair (Labour)") + ylim(0, 900)
# results for Charles Kennedy liberal
a3 <- ggplot(BEPS, aes(factor(Kennedy))) +
geom_bar(fill="yellow") + ylab("") +
xlab("Kennedy (Liberal)") + ylim(0, 900)
# display the candidates results
grid.arrange(a1, a2, a3, nrow=1)

# similar barcharts with public knowledge of political parties and attitude towards european integration

b1 <- ggplot(BEPS, aes(factor(political.knowledge))) +
geom_bar(fill="tan2") + coord_flip() + ylab("") +
xlab("Knowledge of policies on Europe")
b2 <- ggplot(BEPS, aes(factor(Europe))) +
geom_bar(fill="lightgreen") + ylab("") +
xlab("Attitudes to European integration")
grid.arrange(b1, b2, nrow=1, widths=c(4, 8))

   4.5 Discrete data—counts and integers

  #Deaths by horsekicks

# death by horsekick dataset 
data(VonBort, package="vcd")
ggplot(VonBort, aes(x=factor(deaths))) + geom_bar() +
xlab("Deaths by horse kick")

 #Benford’s Law

 "That the ten digits do not occur with equal
frequency must be evident to any one making much use of logarithmic tables, and
noticing how much faster the first pages wear out than the last ones"

#The package benford.analysis provides six datasets, including
census.2009, which gives the populations of 19,509 U.S. towns and cities.
xx <- 1:9
Ben <- data.frame(xx, pdf=log10(1+1/xx))
ggplot(Ben, aes(factor(xx), weight=pdf)) + geom_bar() +
xlab("") + ylab("") + ylim(0,0.35)

   4.6 Formats, factors, estimates, and barcharts

 #Shape of the dataset
Depending how the data is presented , some restructuring might be in order

 #Coding of variables
How we assign meaning to the data

 #Estimates shown as bars
In some fields of application estimates are displayed as bars

   4.7 Modelling and testing for categorical variables

1. Testing by simulation
?2 tests in one form or another are standard. An alternative to relying on the
asymptotic distribution of the test statistic is to simulate many datasets and compare the actual test statistic value with the distribution of simulated values. This
option is available in the R function chisq.test.

2. Evenness of distribution
If numbers are supposed to have been drawn at random, for instance in a lottery, or if a random number generator is to be checked directly, then the null
hypothesis of equally likely probabilities can be carried out with a ?2 test.

3. Fitting discrete distributions
As always with categorical data, ?2 tests are important, but it is useful to inspect
any lack of fit visually. Tukey’s hanging rootograms are one approach; plotting
components of ?2-statistics is another.



 		
  