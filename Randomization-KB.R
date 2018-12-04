install.packages("data.table")
install.packages("dplyr")## dplyr provides the join functions
library(data.table)
library(dplyr) 
library(stringr)
library(tidyr)

setwd("G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin/Patents/RSUE")


#---------Find originating patents whose cites / controls are in same cluster----------
matching_clusters <- function(xnames5,xnames10,xnames20) {

  xnames <- c(xnames5,xnames10,xnames20)
  
  #Here I'm receiving list_of_matches.
  random_matches <- sapply(list_of_matches, function(x) ifelse(length(x)==0,NA,sample(x,1)))

  # Now you can cbind the matches:
  merged_patents <- as.data.table(cbind(citations,possiblenclass[random_matches,]))
  # And delete any rows that don't have controls (also deletes rows created)
  # spuriously when cutting the data down
  merged_patents <- subset(merged_patents[,.(cited,patent,control)], (!is.na(merged_patents[,control])))


  #Third Program begin
  clustpatents <- select(clustpatents,one_of("NE_Plots_2",xnames))
  aux_clustpatents<-rename(clustpatents, cited = NE_Plots_2)

  #Merged both datatables 
  matching_1<-as.data.table(left_join(merged_patents,aux_clustpatents, by="cited"))

  setorder(matching_1, "patent") #Sort the dataset by patent

  #Match citing data (from Matching dataset) with geographic information from clustpatents
  clustpatents_cit<-setnames(aux_clustpatents,xnames,str_c("c",xnames))
  rm(aux_clustpatents)

  aux2_clustpatents<-rename(clustpatents_cit, patent = cited)

  #Merge both datatables 
  matching_2<-as.data.table(left_join(matching_1,aux2_clustpatents, by="patent"))
  rm(aux2_clustpatents)

  #Sort the dataset by control
  setorder(matching_2, "control")
  aux3_clustpatents<-rename(clustpatents, control = NE_Plots_2)

  #Match control data (from Matching dataset) with geographic information from clustpatents
  clustpatents_cont<-setnames(aux3_clustpatents,xnames,str_c("cc",xnames))
  rm(aux3_clustpatents)

  #Merged the datatables again
  matching_3<-as.data.table(left_join(matching_2,clustpatents_cont, by="control"))

  #Transforming NA into 0 to use a binary analysis
  for(j in seq_along(matching_3)){
    set(matching_3, i=which(is.na(matching_3[[j]])), j=j, value=0)
  }

  #Create Matching variables for clusters(c=citing, cc=control)
  for(i in 1:length(xnames)) {
    xname <- xnames[i]
    yname <- paste0('c', xname)
    yname2 <- paste0('cc', xname)
    outname <- paste0('c_same', xname)
    outname2 <- paste0('cc_same', xname)
    x <- matching_3[[xname]]
    y <- matching_3[[yname]]
    y2 <- matching_3[[yname2]]
    z <- 1 * (x == 1 & y == 1)
    z2 <- 1 * (x == 1 & y2 == 1)
    matching_3[[outname]] <- z
    matching_3[[outname2]] <- z2
    out[[xname]] <- sum(x)
    out[[outname]] <- sum(z)
    out[[outname2]] <- sum(z2)
  }

  return(transpose(out))  
  
}  



#-------Randomize Data, Format both randomized and non-randomized data into columns----
randomize <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n) {

  xnames <- c(xnames5,xnames10,xnames20)
  
  replications <- as.data.table(replicate(n, matching_clusters(xnames5,xnames10,xnames20)))
}


colsEFI <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n) {
  clusterMeans <- as.data.table(rowSums(replications)/n)
  xnames <- c(xnames5,xnames10,xnames20)
  
  locdiff <- list()
  for(i in 1:length(xnames)) {
    locdiff[[i]] <- replications[2+3*(i-1),]/replications[3+3*(i-1),]
  }
  locdiff <- as.data.table(apply(do.call("rbind",locdiff), 1, function(x) mean(x[!is.infinite(x)])))
  
  #Format Randomized Data into Columns
  columnE_5 <- round(clusterMeans[seq(1, by=3, length.out = length(xnames5)),])
  columnF_5 <- round(clusterMeans[seq(2, by=3, length.out = length(xnames5)),])
  columnI_5 <- round(clusterMeans[seq(3, by=3, length.out = length(xnames5)),])
  columnL_5 <- locdiff[1:length(xnames5)]
  columnL_5 <- round(rbind(columnL_5,list(0)), digits = 1)

  columnE_10 <- round(clusterMeans[seq(length(xnames5)*3+1, by=3, length.out = length(xnames10)),])
  columnF_10 <- round(clusterMeans[seq(length(xnames5)*3+2, by=3, length.out = length(xnames10)),])
  columnI_10 <- round(clusterMeans[seq(length(xnames5)*3+3, by=3, length.out = length(xnames10)),])
  columnL_10 <- locdiff[length(xnames5)+1:length(xnames10)]
  columnL_10 <- round(rbind(columnL_10,list(0)), digits = 1)
  
  
  columnE_20 <- round(clusterMeans[seq((length(xnames5)+length(xnames10))*3+1, by=3, length.out = length(xnames20)),])
  columnF_20 <- round(clusterMeans[seq((length(xnames5)+length(xnames10))*3+2, by=3, length.out = length(xnames20)),])
  columnI_20 <- round(clusterMeans[seq((length(xnames5)+length(xnames10))*3+3, by=3, length.out = length(xnames20)),])
  
  cols <- list(columnE_5,columnF_5,columnI_5,columnE_10,columnF_10,columnI_10,columnE_20,columnF_20,columnI_20,columnL_5,columnL_10)
}


#--------Read in data and cluster names-----------------------

#---------Northeast baseline-----------
citations <- fread("SAScitations.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_NEbaseline2")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

n=999

ne_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
          "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
x <- ne_base
y <- "NEbaseline"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#-------------California baseline-----------------
citations <- fread("SAScitationsCA.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_CAbaseline")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

n=999

ca_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ca_base
y <- "CAbaseline"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#-----------MSA baseline--------------
citations <- fread("SAScitationsMSA.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingMSA.csv")
clustpatents <- fread("SASclustpatentsMSA.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_MSA")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("BOS","LA","NY","PHL","SD","SF","DC")
xnames10 <- NULL
xnames20 <- NULL

clnames5 <- c("Boston","Los Angeles","New York","Philadelphia","San Diego","San Francisco","Washington, DC")
clnames10 <- NULL
clnames20 <- NULL

n=999

msa_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- msa_base
y <- "MSA"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#---------Northeast subclass-------------
citations <- fread("SAScitationsNEsub.csv")
possiblenclass <- fread("SASpossiblenclassSub.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

possiblenclass <- separate(possiblenclass, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
setkeyv(possiblenclass, c("nclass","subclass"))
possiblenclass <- possiblenclass[,-c(7:8)]

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
setkeyv(citations,c("nclass","subclass"))
citations <- citations[,-c(12:13)]
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_NEsub")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

n=999

ne_sub <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
x <- ne_sub
y <- "NEsub"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)

#-------------#California subclass---------------
citations <- fread("SAScitationsCAsub.csv")
possiblenclass <- fread("SASpossiblenclassSub.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

possiblenclass <- separate(possiblenclass, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
setkeyv(possiblenclass, c("nclass","subclass"))
possiblenclass <- possiblenclass[,-c(7:8)]

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
setkeyv(citations,c("nclass","subclass"))
citations <- citations[,-c(12:13)]
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_CAsub")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

n=999

ca_sub <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ca_sub
y <- "CAsub"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)

#---------NE Stem-------------
citations <- fread("SAScitationsNEstem.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingNEstem.csv")
clustpatents <- fread("SASclustpatentsNEstem.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_NEstem")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("DC5A","DC5B","Balt5","Wilm5","Philly5A","Philly5B","NY5A","NY5B","CT5A","CT5B","CT5C","CT5D","Boston5A","Boston5B","Boston5C","Bing5","Syracuse5","Buffalo5","Pitt5A","Pitt5B")
xnames10 <- c("Richmond10","DC10A","DC10B","Philly10A","Philly10B","Pitt10","Bing10","Syracuse10","Rochester10","Buffalo10","Boston10","NY10")
xnames20 <- NULL

clnames5 <- c("Bethesda-Rockville, MD-Vienna, VA","Columbia-Laurel, MD","Phoenix-Cockeysville, MD","Wilmington, DE","King of Prussia, PA","Philadephia, PA","Princeton, NJ-New York, NY","Long Island, NY","Danbury, CT","Stratford, CT","North Haven,CT","Hartford, CT","Hudson-Westborough, MA","Boston-Cambridge, MA","Nashua, NH","Binghamton, NY","Syrcuse, NY","Buffalo, NY","Pittsburgh, PA","Pittsburgh-Verona, PA")
clnames10 <- c("Richmond, VA","Washington, DC-Baltimore, MD","Hagerstown, MD","Lancaster,PA","Philadelphia,PA-Wilmington,DC-Cherry Hill, NJ","Pittsburgh, PA","Binghamton, NY","Syracuse, NY","Rochester,NY","Buffalo, NY","Boston, MA","New York, NY-Northern NJ-CT")
clnames20 <- NULL

n=999

ne_stem <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                 "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ne_stem
y <- "NEstem"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)

#--------CA Stem----------------
citations <- fread("SAScitationsCAstem.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingCAstem.csv")
clustpatents <- fread("SASclustpatentsCAstem.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_CAstem")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5A","SD5B","OC5","MAL5","SB5","SF5A","SF5B","SF5C")
xnames10 <- c("SD10","OC10","MAL10","SB10","SF10","SF10B")
xnames20 <- NULL

clnames5 <- c("San Diego-La Jolla","Carslbad","Irvine","Camarillo","Santa Barbara","San Jose-Santa Clara","Pleasanton","Santa Rosa")
clnames10 <- c("San Diego","Anaheim-Irving","Oxnard-Camarillo","Santa Barbara","San Francisco-Palo Alto-San Jose","Santa Rosa")
clnames20 <- NULL

#n=10

ca_stem <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ca_stem
y <- "CAstem"


replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#---------Northeast No Examiner-added Patents-----------
citations<- fread("SAScitationsNENOexam.csv")
possiblenclass <- fread("SASpossiblenclassNOexam.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_NENOexam")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

n=999

ne_NOexam <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
x <- ne_NOexam
y <- "NENOexam"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#---------California No Examiner-added Patents-----------
citations<- fread("SAScitationsCANOexam.csv")
possiblenclass <- fread("SASpossiblenclassNOexam.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_CANOexam")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

ca_NOexam <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                  "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
x <- ca_NOexam
y <- "CANOexam"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#---------Northeast ONLY Examiner-added Patents-----------
citations<- fread("SAScitationsNEONLYexam.csv")
possiblenclass <- fread("SASpossiblenclassNOexam.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_NEONLYexam")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

ne_ONLYexam <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                  "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
x <- ne_ONLYexam
y <- "NEONLYexam"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#---------California ONLY Examiner-added Patents-----------
citations<- fread("SAScitationsCAONLYexam.csv")
possiblenclass <- fread("SASpossiblenclassNOexam.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_CAONLYexam")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

x <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                    "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
y <- "CAONLYexam"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#---------Northeast All Patents in Examiner Era (2001 On)-----------
citations<- fread("SAScitationsNE2001.csv")
possiblenclass <- fread("SASpossiblenclassNOexam.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_NE2001")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

x <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                    "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
y <- "NE2001"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)


#---------California All Patents in Examiner Era (2001 On)-----------
citations<- fread("SAScitationsCA2001.csv")
possiblenclass <- fread("SASpossiblenclassNOexam.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

citations <- setkey(citations,"nclass")
possiblenclass <- setkey(possiblenclass,"nclass")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_CA2001")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

x <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
          "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
y <- "CA2001"

replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)
