install.packages("data.table")
install.packages('bit64')
install.packages("dplyr")## dplyr provides the join functions
library(data.table)
library(dplyr) 

setwd("G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin")

#Read the data from database citations
citations<- fread("SAScitations.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclass.csv")

#We transform possiblenclass and citations into a smaller subset, 
#given that the memory requirements are too high
#Creating small data tables

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")

smallcitations<-citations[.(1:30)]
#I'm deleting the variables I'll not going to use, to make the data sets smaller.
smallpossiblenclass<-possiblenclass[.(1:30)]
smallpossiblenclass<-smallpossiblenclass[,c(1:50)]
#smallpossiblenclass[,  c("x_control")  := NULL]

smallcitations=as.data.frame(smallcitations)#To use the same objects as Frank's code. 
smallpossiblenclass=as.data.frame(smallpossiblenclass)

#Delete the NAs created in smallmatching2 and smallpossiblenclass by the partitioning of the datasets
#smallcitations<-smallcitations[-which(is.na(smallcitations$cited)),]
#smallpossiblenclass<-smallpossiblenclass[-which(is.na(smallpossiblenclass$control)),]


# Create matrix of citations in big for use below
big_citations <- as.matrix(smallpossiblenclass[, -c(1:9)])

# Function that returns vector of all rows from big that match 'mypatent,' a list
# with named elements corresponding to small. (I.e. mypatent will be a row from 
# small)
get_matching_rows <- function(mypatent) {
  
  # Vector of row indices for all patents in big that cite mypatent
  citing_rows<- which((big_citations == mypatent$cited) & (big_citations == mypatent$patent) , arr.ind = TRUE)[,1]
  
  # Vector of row indices for big that satisfy:
  # (1) same class as mypatent
  # (2) within one year of mypatent
comparable_rows <- which((smallpossiblenclass$nclass == mypatent$nclass) &
                             (abs(mypatent$sasdate - smallpossiblenclass$c_sasdate) < 183) &
                             (abs(smallpossiblenclass$c_invnum - mypatent$invnum)>0) &
                             ((smallpossiblenclass$c_appyear - mypatent$o_gyear)>=0) &
                             (abs(smallpossiblenclass$c_pdpass - mypatent$o_pdpass)>0) &
                             (abs(smallpossiblenclass$c_pdpass - mypatent$pdpass)>0) &
                             (abs(smallpossiblenclass$c_invnum - mypatent$o_invnum)>0) &
                             (abs(smallpossiblenclass$control - mypatent$patent)>0) &
                             (abs(smallpossiblenclass$control - mypatent$cited)>0))  

  
  # A match is a row index in comparable_rows that does *not* cite mypatent 
  matching_rows <- setdiff(comparable_rows, citing_rows)
  return(matching_rows)
} 

# Now find the full set of matching indices for every patent in small
system.time(
  list_of_matches <- lapply(1:nrow(smallcitations), 
                            function(row) get_matching_rows(smallcitations[row,])))

# Use the list of matches to generate random matches (note that this is a 
# with replacement matching strategy, since we may re-use the same patent
# in big as a match for different patents in small.)

#either of the following two ways will work to deal with the lists of length zero; we will want
#to test to see which is faster
#list_of_matches[sapply(list_of_matches, function(x) length(x)==0)] <- NA
 # random_matches <- sapply(list_of_matches, function(x) sample(x,1))

random_matches <- sapply(list_of_matches, function(x) ifelse(length(x)==0,NA,sample(x,1)))


# Now you can cbind the matches:
merged_patents <- cbind(smallcitations, smallpossiblenclass[random_matches,])
# And delete any rows that don't have controls (also deletes rows created)
# spuriously when cutting the data down
controls <- subset(merged_patents, (!is.na(merged_patents[,'control'])))