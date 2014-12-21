#################################################################
# Statistics 133, Lecture 1, Fall 2014
# Final Exam, Friday December 19th, 7 pm - 10 pm

# Please read all instructions carefully.

# When you are done SAVE the file final.R
# plug ethernet back in and please email it to
# ingileif@gmail.com
#################################################################
# IMPORTANT
# Enter your personal information here, between the quotation marks:
name <- "Luka Dordevic"
github.name <- "LukaDordevic"
email.address <- "luckyluke@berkeley.edu"

#################################################################
# The exam has a total of 100 points and for each task 
# the point is given in square brackets.
# The exam has 6 parts and the total number of points by part is:
# 15 pts : Part I, general R commands
# 20 pts : Part II, plotting
# 15 pts : Part III, apply and by statements
# 20 pts : Part IV, functions 
# 10 pts : Part V, simulations
# 20 pts : Part VI, string manipulation and regular expressions
#100 pts in Total

# If you spend about 30 min or so reading instructions and the like
# this gives you 1.5 min per point -- the exam is LONG ON PURPOSE!
# Please just work your way through it the best you can, prioritizing
# things you can easily do and remember everyone else is also
# racing the clock and will be graded on a curve.

#################################################################
# DO NOT manually set the path anywhere in this script, instead:

# RIGHT NOW!!   In the RStudio menu select:
# Session > Set Working Directory > To Source File Location

# NOTE!
# All "load" statements are included in the file already,
# DO NOT change them in any way or add a path to them,
# it will prevent us from being able to run your script.

# NOTE!
# There are a few calls to the function set.seed()
# DO NOT change these in any way.

#################################################################

#### PART I : General R commands [15 pts]

# [1 pt]
# Create [x], a numeric vector of length 1000 with 
# entries: 5, 10, 15, etc.
x=seq(5, 5000, by=5)

# > class(x)
# [1] "numeric"

  # [1 pt]
  # Create [y], a logical vector of length 1000 
  # with y[i]=T if x[i] is divisible by 10, otherwise F
  
  y <- !(as.logical(sapply(1:length(x), function(i){
    x[i]%%10
  })))
  
  
  # [1 pt]
  # Create [z], a numeric vector of length 111 with entries
  # that are drawn from a standard normal distribution (hint: rnorm)
  # *and* stored in increasing order
  set.seed(42)
z <- rnorm(111)
  
  
  # [1 pt]
  # Create [v], a numeric vector with :
  # a random permutation of the even numbers from 2 to 222
  set.seed(31415)
v <- sample(seq(2,222,by=2), 111 ,replace=F)
  
  
  # [1 pt]
  # Create [w], a random permutation of the numeric values of a deck of cards
  # (i.e. just the numbers 1 through 13 each repeated 4 times)
  set.seed(2718)
w <- sample(rep(1:13, 4), 52, replace=F)
  
  
  # [1 pt]
  # Create [m], a matrix of size 10x10 with entries that are 
  # Exponential random variables (hint: rexp) with rate 3
  # (arrange the values by column, as per default)
  set.seed(344)
m <- matrix(rexp(100, 3), ncol=10, nrow=10)
  
  
  # [1 pt]
  # Create [l], a list with 12 elements, each a vector of length 100.
  # Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
  set.seed(71)
l <- list(rpois(100, 5), rpois(100, 5), rpois(100, 5), rpois(100, 5), rpois(100, 5),
            rpois(100, 5), rpois(100, 5), rpois(100, 5), rpois(100, 5), rpois(100, 5),
            rpois(100, 5), rpois(100, 5))
  
  
  # For the next few tasks you will use the data frame family (size 14x5)
  # LEAVE AS IS:
  load("family.rda")

# [1 pt]
# Create [f1] a subset of family with only women age 50 or over
f1 <- family[family$gender == 'f' & family$age >= 50,]
  
  
  # [1 pt]
  # Create [f2] a subset of family with only men 6 foot tall or more
  f2 <- family[family$gender == 'm' & family$height > 72,]
  
  
  # [1 pt]
  # Create [f3] a subset of family of people whose name starts with T
  ##f3 <- family[gsufamily$name == '^T'] 

f3=family[family$name %in% unlist(sapply(1:length(family$name), function(x){
  grep("^T", levels(family$name)[x], value=T)})),]
  
  # [1 pt]
  # Create [f4] a subset of family with just the youngest individual (so just one row)
  f4 <- family[family$age == min(family$age),]
  
  
  # for the next two tasks you will use the data frame infants (size 1236x15)
  # LEAVE AS IS:
  load("KaiserBabies.rda") 

# [2 pt]
# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers:
t <- table(infants[infants$parity == 1 & infants$marital == "Married",'ed'])
  
  # [2 pt]
  # Calculate [mw], the average birthweight ($bwt) of all babies whose were full term, 
  # i.e. gestation equal or more than 259 days.
  mw <- mean(infants[infants$gestation >= 259, 'bwt'], na.rm=T)
  
  
  #################################################################
#### PART II : Plotting [20 pts]

##### Flowers [8 pts total, 2+3+3]
# We will now use the dataset "iris" which is included in the R package.
# To look at the dataframe you can just type "iris" at the prompt
# It is a data frame of size 150x5 with measurements of 4 attributes
# for 150 flowers, 50 each of 3 different species of irises.

# [2 pts]
# Make a box plot of Sepal Length by Species (so 3 boxplots in one plot)
boxplot(iris$Sepal.Length ~ iris$Species)


# [3 pts]
# Make a scatterplot of petal width (y-axis) versus petal length (x-axis)
# The axes labels should be "Petal Length" and "Petal Width",
# Color the plotting symbol by Species (any 3 colors)
plot( iris$Petal.Width ~ iris$Petal.Length, xlab="Petal Length",
      ylab="Petal Width", col=c('red','blue', 'green')) #color
#legend('topright', iris$Species, pch='o', col=c('red','blue', 'green'))
# [3 pt]
# Make a scatterplot of ( sepal length / petal length) as a function of index (order)
# Color the plotting symbol by Species (any 3 colors)
scatter.smooth(iris$Sepal.Length/iris$Petal.Length , iris$Species, col=c('red','blue', 'green'))

##### We will now use the infant birth data again (data frame infants)

# [6 pts]
# Make a scatterplot of infant birthweight in ounces (bwt) vs. gestation time in days (gestation)
# The plotting symbol should be a red star (*)
# Put on custom made x-axis and y-axis labels that fully describe the variables
# Add a vertical line at gestation=259 (full length pregnancy)
plot(infants$bwt ~ infants$gestation, pch='*', col='red',
     ylab="Birthweight of infants", xlab="Gestation of infants", main="Gestation vs Birthweight")
abline( v=259, untf = T)



# [6 pts]
# Make a histogram of mother's age (age) and superimpose on it a _blue_ density plot (same variable)
# Note that the y-axis of the histogram and the density have to be the same...
# Add x-axis labels
hist(infants$age, freq=F, xlab="Age of a mother") 
plot(density(infants$parity), col='blue')
#plot(density(table(infants$age, infants$parity)))


#################################################################
##### PART III : apply and by statements [15 pts]

# For the next few tasks you will use the list Cache500 
# (list of length 500, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("Cache500.rda")

# [3 pts]
# Create [first.cache], a vector where each entry is the _first_ element of the
# corresponding vector in the list Cache500

first.cache <- unlist(lapply(Cache500, function(x){(x[1])}))
  
  # [3 pts]
  # Create [mean.cache], a vector of length 500 where each entry is the mean 
  # of the corresponding element of the list Cache500
  
  mean.cache <- unlist(lapply(Cache500, function(x){mean(x)})) #do we need to delete NAs?
  
  # [2 pts]
  # Create [sd.cache], a vector of length 500 where each entry is the sd
  # of the corresponding element of the list Cache500
  
  sd.cache <- unlist(lapply(Cache500, function(x){sd(x)}))
  
  # [4 pts]
  # Create [mean.long.cache], a vector where 
  # mean.long.cache[i] is:
  # the mean of Cache500[[i]] IF it has 50 or more entries.
  # NA IF Cache500[[i]] has less than 50 entries.
  
  mean.long.cache <- sapply(1:length(mean.cache), function(x){
    ifelse(length(Cache500[[x]]) >= 50, mean.cache[x], NA)
  })
  
  # Consider again the iris dataset
  # [3 pts]
  # Create a variable [max.petal.width] _a numeric vector of length 3_
  # that has the maximum petal length for each iris species.
  
  max.petal.width <- as.numeric(tapply(iris$Petal.Length, iris$Species, max))
  
  #################################################################
##### PART IV : functions [20 pts]

# [6 pts]
# Write a function [firstColToNames] that takes a matrix or a data frame
# and converts the first column to row names.
# Input:  a matrix or a data frame
# Output : a matrix of data frame that is like the input except 
# -- the first column has been removed
# -- what was the first column is now row names.

# Example, make a small 2x4 matrix, test:
# test <- matrix(c(1, 5, 3, 8, 2, 5, 7, 9), ncol=4, byrow=T)
# > test
# [,1] [,2] [,3] [,4]
# [1,]    1    5    3    8
# [2,]    2    5    7    9
# The output from firstColToNames(test) should be a 2x3 matrix with row names
# > firstColToNames(test)
#   [,1] [,2] [,3]
# 1    5    3    8
# 2    5    7    9

firstColToNames <- function(m){
  temp <- m[,1]
  rownames(m) <- temp
  m=m[,-1]
  return(m)
    
}


# [6 pts]
# Write a function [longerRange()] with
# Input [m1 and m2] : two numeric matrices or data frames, don't have to have the same dimension 
# Output : 1 if the range of m1 is larger than or equal to the range of m2, 
#        : 2 otherwise 
# The range is the distance between the maximum and minimum values
# The function should ignore NA values (i.e. if a matrix has an entry that is NA)

longerRange <- function(m1, m2){
  max1=max(sapply(1:dim(m1)[2], function(x){max(m1[,x])}))
  min1=min(sapply(1:dim(m1)[2], function(x){min(m1[,x])}))
  max2=max(sapply(1:dim(m2)[2], function(x){max(m2[,x])}))
  min2=min(sapply(1:dim(m2)[2], function(x){min(m2[,x])}))
  range1=max1-min1
  range2=max2-min2
  if(range1 >= range2) return(c(1))
  else return(c(2))
}

# [8 pts]
# Write a function [TempConv()] that takes
# Inputs
# [t] : a temperature (numeric value)
# [scale] : a character "F" or "C" depending on whether the 
#           input temperature is in Fahrenheit or Celcius
# Output 
# [t.new] : the temperature converted to the other scale.

# The conversion formulas are: 
# F = C * 9/5 + 32
# C = (F - 32) * 5/9
# so e.g. 30 F=-1.11 C and 30 C = 86 F

TempConv <- function(t, scale){
  if(scale == 'F'){
    temp = (t - 32) * 5/9
    return(temp)
  }
  else{
    temp= t * 9/5 + 32
    return(temp)
  }
}
  
    

#################################################################
##### PART V : simulations [10 pts]

# leave this here:
set.seed(123456)

# [5 pts]
# Write a function, [dice_sum()], that takes as input:
# [k] : the number of dice rolled
# [B] : the number of rolls
# and returns:
# [dsum] : a vector of length B where each element is the sum of a roll 
#          k dice 

# So if k=1 pick a number between 1 and 6 at random B times and return them,
# if k=2 then in each roll you pick twice a number between 1 and 6 at random,
# calculate their sum, do this B times and return
# and so on.

dice_sum <- function(k, B){
  dsum=sapply(1:B, function(x){
    sample(1:6, k, replace=T)
})
apply(dsum, 2, sum)
}

# [5 pts]
# Lets run four simulations:
# Fix k=2 for all simulations
# Use B=20, 100, 1000, 5000 (in this order)

# For each value of B we will:

# Calculate the mean and sd of the output and store in 
# [ave.diceRoll] : a vector of length 4 where each entry is the mean of the simulation output
# [sd.diceRoll] : a vector of length 4 where each entry is the sd of the simulation output

# Then plot four histograms of the output from dice_sum()
# On each histogram the x-axis label should be "sum of dice roll" and the title should be
# "Histogram for B=[correct number]"

# CAREFUL : just run 4 simulations and save the mean and sd each time and make the histogram
# DO NOT : run 4 simulations to get the mean, then another 4 simulations to make the plots.

# NOTE : if for some reason you can not write the function [dice_sum] create four vectors
# of length 20, 100, 1000 and 5000 and complete the tasks below.

#----------------------
# To get all four histograms on one plot we include a command that splits the plotting
# screen into four panels, 2 in each row.  When a plotting function is called the plot
# goes into the first panel (upper left), when you call a plotting function again it goes
# into the second panel (upper right), etc.

# DO NOT DELETE THIS:
par(mfrow=c(2,2))
#----------------------
simulations <- data.frame(dice_sum(2,20), dice_sum(2,100), dice_sum(2,1000), dice_sum(2,5000))
ave.diceRoll <- sapply(1:dim(simulations)[2], function(x) mean(simulations[,x]))
  sd.diceRoll <- sapply(1:dim(simulations)[2], function(x) sd(simulations[,x]))
  Bvec <- c(20, 100, 1000, 5000)

hist(simulations[,1], xlab="Sum of dice roll", main="Histogram for B=20")
hist(simulations[,2], xlab="Sum of dice roll", main="Histogram for B=100")
hist(simulations[,3], xlab="Sum of dice roll", main="Histogram for B=1000")
hist(simulations[,4], xlab="Sum of dice roll", main="Histogram for B=5000")
  
  # STOP: did you remember to put in the title and axis label on the histograms?
  
  #################################################################
##### PART VI : string manipulation and regular expressions [20 pts]

phrases <- c("coat", "cat", "ct", "mat", "Sat!", "Now?", "match", "How much? $10", "7 cats", "ratatatcat", "atatatatatatatatat")

# [2 pts]
# Create a vector [text1] that lists the elements in phrases that have 
# a match to "at", anywhere 
text1 <- grep("(at)", phrases, value=T)
  
  # [2 pts]
  # Create a vector [text2] that lists the elements in phrases that have 
  # a match to "at", _at the end of the phrase_ 
  text2 <- grep("(at)$", phrases, value=T)
  
  # [4 pts]
  # Create a vector [text3] that lists the elements in phrases that have 
  # a match to any multiple of "at", _two or more times_ (atat" or "atatat" etc.)
  # and anything before or after that match
  text3 <- grep("(at){2,}", phrases, value=T)
  
  # [3 pts]
  # Create a vector [tests] that is of length 200 and has the entries
  # "test1", "test2", ..., "test200"
  
  tests <- sapply(1:200, function(x){
    paste0('test', x)
  })
  
  # [3 pts]
  # Take the vector [tests] from above and create a character string
  # [tests.all] (so a vector of length 1)
  # that stores the entries of [tests] as one long string
  # i.e. tests.all should be "test1 test2 test3 ... test200"
  tests.all <- paste0(tests, collapse=" ")
    
  
  
  # [6 pts]
  # Start with [minchin] which is a character string, create 
  # a _vector_ (not list) [minchin.split] which 
  # stores the words of [minchin] each as a separate element.
  # Also, convert all upper case letters to lower case.
  # You can leave punctuation marks in.
  
  minchin <- "And try as hard as I like, A small crack appears In my diplomacy-dike. By definition, I begin, Alternative Medicine, I continue Has either not been proved to work, Or been proved not to work. You know what they call alternative medicine That's been proved to work? Medicine."

minchin.split <- unlist(strsplit(gsub("([[:alnum:]])|([[:punct:]])", "\\1", tolower(minchin)), " ")) ##deleted punct.
  
  
  #################################################################

# OK! done
# Plug ethernet back in SAVE the file final.R and email it to
# ingileif@gmail.com

