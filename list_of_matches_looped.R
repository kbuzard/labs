install.packages("data.table")
install.packages("dplyr")## dplyr provides the join functions
install.packages("parallel")
install.packages("tidyr")
library(data.table)
library(dplyr) 
library(parallel)
library(tidyr)

setwd("G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin/Patents/RSUE")

# Use the detectCores() function to find the number of cores in system
num_cores <- detectCores()-1

#----------------for Northeast Baseline--------------------------------
#Read the data from database citations
citations<- fread("SAScitations.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclass.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_NEbaseline <- unlist(lom,recursive=F)

saveRDS(list_of_matches_NEbaseline,file = "list_of_matches_NEbaseline2")



#----------------for California Baseline--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsCA.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclass.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_CAbaseline <- unlist(lom,recursive=F)

saveRDS(list_of_matches_CAbaseline,file = "list_of_matches_CAbaseline")



#----------------for MSA Baseline--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsMSA.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclass.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_MSA <- unlist(lom,recursive=F)

saveRDS(list_of_matches_MSA,file = "list_of_matches_MSA")


#----------------for Subclasses NE--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsNEsub.csv")
citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassSub.csv")
possiblenclass <- separate(possiblenclass, nclass, c("nclass","subclass","subclass2"), convert = TRUE)

setkeyv(citations,c("nclass","subclass"))
setkeyv(possiblenclass, c("nclass","subclass"))
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkeyv(citations,c("nclass","subclass"))
  setkeyv(possiblenclass, c("nclass","subclass"))
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:783)])
  
  # Create matrix of citations in big for use below
  big_citations <- as.matrix(smallpossiblenclass[, -c(1:11)])
  
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
                                (mypatent$subclass == smallpossiblenclass$subclass) &
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_NEsub <- unlist(lom,recursive=F)

saveRDS(list_of_matches_NEsub,file = "list_of_matches_NEsub")


#----------------for Subclasses CA--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsCAsub.csv")
citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassSub.csv")
possiblenclass <- separate(possiblenclass, nclass, c("nclass","subclass","subclass2"), convert = TRUE)

setkeyv(citations,c("nclass","subclass"))
setkeyv(possiblenclass, c("nclass","subclass"))
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkeyv(citations,c("nclass","subclass"))
  setkeyv(possiblenclass, c("nclass","subclass"))
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:783)])
  
  # Create matrix of citations in big for use below
  big_citations <- as.matrix(smallpossiblenclass[, -c(1:11)])
  
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
                               (mypatent$subclass == smallpossiblenclass$subclass) &
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_CAsub <- unlist(lom,recursive=F)

saveRDS(list_of_matches_CAsub,file = "list_of_matches_CAsub")


#----------------for Northeast STEM--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsNEstem.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclass.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_NEstem <- unlist(lom,recursive=F)

saveRDS(list_of_matches_NEstem,file = "list_of_matches_NEstem")


#----------------for California STEM--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsCAstem.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclass.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_CAstem <- unlist(lom,recursive=F)

saveRDS(list_of_matches_CAstem,file = "list_of_matches_CAstem")


#----------------for Northeast No Examiner--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsNENOexam.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassNOexam.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_NENOexam <- unlist(lom,recursive=F)

saveRDS(list_of_matches_NENOexam,file = "list_of_matches_NENOexam")


#----------------for California No Examiner--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsCANOexam.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassNOexam.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_CANOexam <- unlist(lom,recursive=F)

saveRDS(list_of_matches_CANOexam,file = "list_of_matches_CANOexam")


#----------------for California All 2001 Onward--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsCA2001.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassNOexam.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_CA2001 <- unlist(lom,recursive=F)

saveRDS(list_of_matches_CA2001,file = "list_of_matches_CA2001")


#----------------for California Examiner-Added Only----------------------------
#Read the data from database citations
citations <- fread("SAScitationsCAONLYexam.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassNOexam.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_CAONLYexam <- unlist(lom,recursive=F)

saveRDS(list_of_matches_CAONLYexam,file = "list_of_matches_CAONLYexam")


#----------------for Northeast Examiner-Added Only----------------------------
#Read the data from database citations
citations <- fread("SAScitationsNEONLYexam.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassNOexam.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_NEONLYexam <- unlist(lom,recursive=F)

saveRDS(list_of_matches_NEONLYexam,file = "list_of_matches_NEONLYexam")


#----------------for California All 2001 Onward--------------------------------
#Read the data from database citations
citations<- fread("SAScitationsNE2001.csv")

#Read the data from database possiblenclass
possiblenclass <- fread("SASpossiblenclassNOexam.csv")

setkey(citations,"nclass")
setkey(possiblenclass, "nclass")
t <- citations[, .N ,by = nclass]
beg <- t[,nclass[2:.N]]
end <- beg

list_of_matches <- vector("list", length(beg))

for(i in 1:length(beg)) {
  setkey(citations,"nclass")
  setkey(possiblenclass, "nclass")
  
  smallcitations<-citations[.(beg[i]:end[i])]
  smallcitations <- as.data.frame(subset(smallcitations[(!is.na(smallcitations[,cited]))]))
  smallpossiblenclass<-possiblenclass[.(beg[i]:end[i])]
  smallpossiblenclass<-as.data.frame(smallpossiblenclass[,c(1:781)])
  
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
  
  # Setup cluster
  clust <- makeCluster(num_cores) #This line will take time
  
  clusterEvalQ(clust, {
    library(data.table)
    library(dplyr)
  })
  
  clusterExport(clust, c('get_matching_rows','big_citations','smallcitations','smallpossiblenclass'))
  
  #The parallel version of lapply() is parLapply() and needs an additional cluster argument.
  list_of_matches[[i]]<-parLapply(clust,1:nrow(smallcitations),function(row) get_matching_rows(smallcitations[row,]))
  stopCluster(clust)
  
  print(i)
  print(Sys.time())
  flush.console()
}

nclass.first <- match(unique(beg), possiblenclass[,nclass])
lom <- list_of_matches

for(j in 1:length(list_of_matches)) {
  for(i in 1:lengths(list_of_matches)[j]) {
    lom[[j]][[i]] <- list_of_matches[[j]][[i]] + nclass.first[[j]] - 1
  }
}

list_of_matches_NE2001 <- unlist(lom,recursive=F)

saveRDS(list_of_matches_NE2001,file = "list_of_matches_NE2001")