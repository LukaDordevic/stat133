# Quiz 3
# Number of Problems: 6
# The quiz is out of 22 points.
here=T
# Function 1 (3 points)
# Write a function called numAtElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly possibly
#     with the "@" symbol
#
# and return the following
#   <num.at>: an integer indicating how many elements of <chvec> contain the "@"
#     symbol. For example: numAtElements(c('karl', 'k@rl', '@@@')) should return 2
numAtElements <- function(chvec){
  x=length(which(( grepl("@", chvec))))
  return(x)
  
}

# Function 2 (3 points)
# Write a function called unexclaim. Your function should take the following
# arguments
#   <chstring>: a character vector of length 1 (contains only one string).
#
# and return the following
#   <newstring>: a character vector of length 1 where all ! symbols have been
#     replaced by . symbols
unexclaim <- function(chstring) {
  x=gsub("\\!", "\\.", chstring)
  return(x)
}

# Function 3 (3 points)
# Write a function called updateDate. Your function should take the following
# arguments
#   <dates>: a character vector of dates of the form "month, year" (e.g. "May, 2001")
#   <old.yr>: a string indicating the year for which elements will be updated
#     (e.g. "2002")
#
# and return the following
#   <updated.dates>: a character vector of dates where <old.yr> has been replaced by
#     '2015'. This vector should only contain the elements whose date has been
#     updated. For example updateDate(c('May, 2010', 'June, 2011'), '2010') should
#     return 'May, 2015'.
updateDate <- function(dates, old.yr) {
  updated.dates=gsub(old.yr, "2015", dates)
  updated.dates=updated.dates[which(updated.dates != dates)]
  return(updated.dates)
}

# Function 4 (4 points)
# Write a function called countcatdog that counts the number of instances of
# "cat" (case-insensitive) and the number of instances of "dog"
# (case-insensitive) in a string. Your function should take the following
# arguments 
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <counts>: An integer vector of length 2 with names "cat" and "dog".
#             For example, countcatdog("doGCATcat abcAt") returns:
#                    cat dog
#                     3   1
countcatdog<- function(chvec){
  cat=length(unlist(gregexpr("cat", tolower(chvec))))
  dog=length(unlist(gregexpr("dog", tolower(chvec))))
  total=c(cat,dog)
  names(total)=c("cat", "dog")
  return(total)
}

# Function 5 (3 points)
# Write a function called sumDigits that compute the sum of all the digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the sum of all the digits in chvec)
sumDigits <- function(chvec){
  x=length(grep("[0-9]", strsplit(chvec, "")[[1]]))
  return(x)
}

# Some test cases:
# all.equal(sumDigits("1z3p ! 21"), 7)
# all.equal(sumDigits("abcdefg"), 0)


# Function 6 (6 points)
# DNA.vec is a character vector of strings of DNA. It contains at least two
# strings of DNA. For simplicity, each string has only 10 characters.  Note that
# a DNA is always made up of A, T, C, G's. Write a function called dnaTransform
# that performs the following:

# Step 1: Find the first two DNA strings in DNA.vec that contains the sequence
# "ATTA"; call them DNA1 and DNA2. If there are less than two DNA strings that
# contains the sequence "ATTA", the function ends immediately and returns the
# first two elements in DNA.vec
split.1= function(DNA.vec){
DNA1 = DNA.vec[grep("ATTA", DNA.vec)[1]]

DNA2 = DNA.vec[grep("ATTA", DNA.vec)[2]]
if(is.na(DNA1) | is.na(DNA2)){
  DNA1=DNA.vec[1]
  DNA2=DNA.vec[2]
}
return(unlist(list(DNA1, DNA2)))
}


# Step 2: Split DNA1 into two halves (i.e. strings of length 5 each).
# (For example, if DNA1 is "ATTATAGCCA", then we have "ATTAT" as the first half
# and "AGCCA" as the second half)
split.2 = function(DNA.vec){
  DNA1 = split.1(DNA.vec)[1]
  DNA2 = split.1(DNA.vec)[2]
DNA1=c(substr(DNA1, start=1, stop=5), substr(DNA1, start=6, stop=10))
# Step 3: Split DNA2 into two halves (as in Step 2).
DNA2=c(substr(DNA2, start=1, stop=5), substr(DNA2, start=6, stop=10))
return(list(DNA1, DNA2))
}
# Step 4: Return a character vector of two strings:
# --first string: the first half of DNA1 combined with the second half of DNA2
# --second string: the first half of DNA2 combined with the second half of DNA1

# Input:
#   <DNA_vec>: A character vector of DNAs
# Output:
#   <DNA_final>: A character vector of two DNAs

dnaTransform <- function(DNA.vec){
    DNA1 = split.2(DNA.vec)[[1]]
    DNA2 = split.2(DNA.vec)[[2]]
    a=paste(DNA1[1], DNA2[2], sep="")
    b=paste(DNA2[1], DNA1[2], sep="")
    if(all(split.1(DNA.vec) == c(DNA.vec[1], DNA.vec[2])))
      return(unlist(list(DNA.vec[1], DNA.vec[2])))
    else
    return(as.character(c(a, b)))
  
}

# Some test cases:
# all.equal(dnaTransform(c("AAAAAAAAAA", "ATTAGATACT", "ATACATTACG")), c("ATTAGTTACG", "ATACAATACT"))
# all.equal(dnaTransform(c("ATCGATCGAT", "TCGATCGATT", "ATTTTTTTTT")), c("ATCGATCGAT", "TCGATCGATT"))
#nice
# End of Quiz 3
