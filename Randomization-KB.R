install.packages('knitr', dependencies = TRUE)
install.packages("data.table")
install.packages("dplyr")## dplyr provides the join functions
install.packages("formattable") #Give format to the tables
install.packages("janitor") #Get the total sum of each column
install.packages("flextable") #Give format to the tables
library(knitr)
library(flextable)
library(formattable)
library(data.table)
library(dplyr) 
library(janitor) 
library(officer)
library(stringr)


setwd("G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin/Patents/RSUE")


#Read the data from database citations
citations<- fread("SAScitations.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
clustpatents<-fread("SASclustpatents.csv")
originating<-fread("SASoriginating.csv")


clustpatents<-as.data.table(lapply(clustpatents, as.numeric))
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))


list_of_matches <- readRDS("list_of_matches")

#c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B","Boston10","DC10","NY10","Philly10","Boston20","DC20","NY20")

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")
xnames <- c(xnames5,xnames10,xnames20)

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

test2 <- data.frame(matrix(0, nrow = 1, ncol = 0))

do_once <- function(xnames) {

  #Here I'm receiving list_of_matches.
  random_matches <- sapply(list_of_matches, function(x) ifelse(length(x)==0,NA,sample(x,1)))

  # Now you can cbind the matches:
  merged_patents <- as.data.table(cbind(citations,possiblenclass[random_matches,]))
  # And delete any rows that don't have controls (also deletes rows created)
  # spuriously when cutting the data down
  merged_patents <- subset(merged_patents[,.(cited,patent,control)], (!is.na(merged_patents[,control])))


  #Third Program begin
  aux_clustpatents<-rename(clustpatents, cited = NE_Plots_2)

  #Merged both datatables 
  matching_1<-as.data.table(left_join(merged_patents,aux_clustpatents, by="cited"))

  setorder(matching_1, "patent") #Sort the dataset by patent

  #Match citing data (from Matching dataset) with geographic information from clustpatents
  clustpatents_cit<-setnames(aux_clustpatents,xnames,str_c("c",xnames))
  rm(aux_clustpatents)

  aux2_clustpatents<-rename(clustpatents_cit, patent = cited)

  #Merged both datatables 
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
    test2[[xname]] <- sum(x)
    test2[[outname]] <- sum(z)
    test2[[outname2]] <- sum(z2)
  }

  return(transpose(test2))  
  
}  

n=2
system.time(test <-as.data.table(replicate(n, do_once(xnames))))
test3 <- as.data.table(rowSums(test)/n)

#test2<-colSums(subset(matching_3[,.(Boston5A,Boston5B,DC5,NY5A,NY5B,NY5C,NY5D,Philly5A,Philly5B,
#                                    c_sameBoston5A, c_sameBoston5B, c_sameDC5, c_sameNY5A, c_sameNY5B, c_sameNY5C, 
#                                    c_sameNY5D, c_samePhilly5A,c_samePhilly5B,
#                                    cc_sameBoston5A, cc_sameBoston5B, cc_sameDC5, cc_sameNY5A, cc_sameNY5B, cc_sameNY5C, 
#                                    cc_sameNY5D, cc_samePhilly5A, cc_samePhilly5B,
#                                    Boston10,DC10,NY10,Philly10,c_sameBoston10,c_sameDC10,c_sameNY10,c_samePhilly10,
#                                    cc_sameBoston10,cc_sameDC10,cc_sameNY10,cc_samePhilly10,
#                                    DC20,Boston20,NY20,
#                                    c_sameDC20,c_sameBoston20,c_sameNY20,
#                                    cc_sameDC20,cc_sameBoston20,cc_sameNY20)]))


columnE_5 <- round(test3[seq(1, by=3, length.out = length(xnames5)),])
columnF_5 <- round(test3[seq(2, by=3, length.out = length(xnames5)),])
columnI_5 <- round(test3[seq(3, by=3, length.out = length(xnames5)),])

columnE_10 <- round(test3[seq(length(xnames5)*3+1, by=3, length.out = length(xnames10)),])
columnF_10 <- round(test3[seq(length(xnames5)*3+2, by=3, length.out = length(xnames10)),])
columnI_10 <- round(test3[seq(length(xnames5)*3+3, by=3, length.out = length(xnames10)),])

columnE_20 <- round(test3[seq((length(xnames5)+length(xnames10))*3+1, by=3, length.out = length(xnames20)),])
columnF_20 <- round(test3[seq((length(xnames5)+length(xnames10))*3+2, by=3, length.out = length(xnames20)),])
columnI_20 <- round(test3[seq((length(xnames5)+length(xnames10))*3+3, by=3, length.out = length(xnames20)),])


final_5_group2 <- as.data.table(cbind(columnE_5, columnF_5))

#Calculate the totals
names(final_5_group2)[1] <- "Matched_Citing_Patents"
names(final_5_group2)[2] <- "From_Same_Cluster"

Cluster2 <- clnames5
final_5_group2 <- cbind(Cluster2, final_5_group2)

final_5_group2 <- adorn_totals(final_5_group2, "row")

final_5_group2[, "columnG"] <- (final_5_group2[, 3] *100/ final_5_group2[, 2])

#Columns H, I, J,K
final_5_group3 <- as.data.table(cbind(columnE_5, columnI_5))

#Calculate the totals
names(final_5_group3)[1] <- "Control_Patents"
names(final_5_group3)[2] <- "From_Same_Cluster"

Cluster3 <- clnames5
final_5_group3<-cbind(Cluster3, final_5_group3)

final_5_group3<-adorn_totals(final_5_group3, "row")

final_5_group3[, "columnJ"] <- (final_5_group3[, 3] *100/ final_5_group3[, 2])


rand_table_5 <- as.data.table(cbind(final_5_group2,final_5_group3))
rand_table_5[,  c("Cluster3")  := NULL]
rand_table_5[, "columnK"] <- (rand_table_5[, 4] / rand_table_5[, 7])



rm(final_5_group2, final_5_group3,Cluster2,Cluster3)


#10 Mile CLuster
final_10_group2 <- as.data.table(cbind(columnE_10, columnF_10))

#This part calcualte the totals
names(final_10_group2)[1] <- "Matched_Citing_Patents"
names(final_10_group2)[2] <- "From_Same_Cluster"

Cluster2 <- clnames10
final_10_group2 <- cbind(Cluster2, final_10_group2)

final_10_group2 <- adorn_totals(final_10_group2, "row")

final_10_group2[, "columnG"] <- (final_10_group2[, 3] *100/ final_10_group2[, 2])


#Columns H, I, J,K
final_10_group3<-as.data.table(cbind(columnE_10, columnI_10))

#This part calcualte the totals
names(final_10_group3)[1] <- "Control_Patents"
names(final_10_group3)[2] <- "From_Same_Cluster"

Cluster3 <- clnames10
final_10_group3<-cbind(Cluster3, final_10_group3)

final_10_group3<-adorn_totals(final_10_group3, "row")


final_10_group3[, "columnJ"] <- (final_10_group3[, 3] *100/ final_10_group3[, 2])


rand_table_10 <- as.data.table(cbind(final_10_group2,final_10_group3))
rand_table_10[,  c("Cluster3")  := NULL]
rand_table_10[, "columnK"] <- (rand_table_10[, 4] / rand_table_10[, 7])

rm(final_10_group2, final_10_group3, Cluster2, Cluster3)


#20 Mile CLuster 

final_20_group2 <- as.data.table(cbind(columnE_20, columnF_20))

#This part calcualte the totals
names(final_20_group2)[1] <- "Matched_Citing_Patents"
names(final_20_group2)[2] <- "From_Same_Cluster"

Cluster2 <- clnames20
final_20_group2 <- cbind(Cluster2, final_20_group2)

final_20_group2 <- adorn_totals(final_20_group2, "row")

final_20_group2[, "columnG"] <- (final_20_group2[, 3]*100 / final_20_group2[, 2])

#Columns H, I, J
final_20_group3 <- as.data.table(cbind(columnE_20, columnI_20))

#This part calcualte the totals
names(final_20_group3)[1] <- "Control_Patents"
names(final_20_group3)[2] <- "From_Same_Cluster"

Cluster3 <- clnames20
final_20_group3 <- cbind(Cluster3, final_20_group3)

final_20_group3 <- adorn_totals(final_20_group3, "row")

final_20_group3[, "columnJ"] <- (final_20_group3[, 3] *100/ final_20_group3[, 2])

rand_table_20<-as.data.table(cbind(final_20_group2,final_20_group3))
rand_table_20[,  c("Cluster3")  := NULL]
rand_table_20[, "columnK"] <- (rand_table_20[, 4] / rand_table_20[, 7])

rm(final_20_group2, final_20_group3, Cluster2, Cluster3)



#Tables without randomization. Columns A, B, C, D

#Column A (Originating Patents)*/  
#First Merge originating with clustpatents*/
clustpatents<-subset(clustpatents[,.(NE_Plots_2,Boston5A,Boston5B,Boston10,DC5,DC10,NY5A,NY5B,NY5C,NY5D,NY10,
                                     Philly5A, Philly5B, Philly10,DC20, Boston20, NY20)])

aux_clustpatents<-rename(clustpatents, cited = NE_Plots_2)

originating<-as.data.table(left_join(originating, aux_clustpatents, by="cited"))

#Match citing data (from Matching dataset) with geographic information from clustpatents

#For Column B (Citations)
#Sort the dataset by cited
setorder(citations, "cited")

citations<-rename(citations, citing = patent)
clustpatents<-rename(clustpatents,  cited = NE_Plots_2)

citations_colB<-as.data.table(left_join(citations,clustpatents, by="cited"))

#Column C (Citations from same cluster as originating)

#Sort the dataset by citing
setorder(citations_colB, "citing")
clustpatents_cit<-setnames(aux_clustpatents,c("Boston5A","Boston5B","Boston10","DC5","DC10","NY5A","NY5B","NY5C","NY5D","NY10", 
                                              "Philly5A","Philly5B","Philly10","DC20","Boston20","NY20"),
                           c("cBoston5A","cBoston5B","cBoston10","cDC5","cDC10","cNY5A","cNY5B","cNY5C","cNY5D","cNY10", 
                             "cPhilly5A","cPhilly5B","cPhilly10","cDC20","cBoston20","cNY20"))
clustpatents_cit<-rename(clustpatents_cit, citing = cited)

citations_colc<-as.data.table(left_join(citations_colB,clustpatents_cit, by="citing"))

#Transforming NA into 0 to use a binary analysis
for(j in seq_along(citations_colc)){
  set(citations_colc, i=which(is.na(citations_colc[[j]])), j=j, value=0)
}


columnc<-citations_colc[, sameBoston5A := ifelse(Boston5A == 1 & cBoston5A == 1, 1 , 0)]  
columnc<-citations_colc[, sameBoston5B := ifelse(Boston5B == 1 & cBoston5B == 1, 1 , 0)]
columnc<-citations_colc[, sameBoston10 := ifelse(Boston10 == 1 & cBoston10 == 1, 1 , 0)]
columnc<-citations_colc[, sameDC5 := ifelse(DC5 == 1 & cDC5 == 1, 1 , 0)]
columnc<-citations_colc[, sameDC10 := ifelse(DC10 == 1 & cDC10 == 1, 1 , 0)]
columnc<-citations_colc[, sameNY5A := ifelse(NY5A == 1 & cNY5A == 1, 1 , 0)]
columnc<-citations_colc[, sameNY5B := ifelse(NY5B == 1 & cNY5B == 1, 1 , 0)]
columnc<-citations_colc[, sameNY5C := ifelse(NY5C == 1 & cNY5C == 1, 1 , 0)]
columnc<-citations_colc[, sameNY5D := ifelse(NY5D == 1 & cNY5D == 1, 1 , 0)]
columnc<-citations_colc[, sameNY10 := ifelse(NY10 == 1 & cNY10 == 1, 1 , 0)]
columnc<-citations_colc[, samePhilly5A := ifelse(Philly5A == 1 & cPhilly5A == 1, 1 , 0)]
columnc<-citations_colc[, samePhilly5B := ifelse(Philly5B == 1 & cPhilly5B == 1, 1 , 0)]
columnc<-citations_colc[, samePhilly10 := ifelse(Philly10 == 1 & cPhilly10 == 1, 1 , 0)]
columnc<-citations_colc[, sameDC20 := ifelse(DC20 == 1 & cDC20 == 1, 1 , 0)]
columnc<-citations_colc[, sameBoston20 := ifelse(Boston20 == 1 & cBoston20 == 1, 1 , 0)]
columnc<-citations_colc[, sameNY20 := ifelse(NY20 == 1 & cNY20 == 1, 1 , 0)]


#Column A (Originating Patents)
originating_5_colA<-as.data.table(colSums(subset(originating[,.(Boston5A,Boston5B,DC5,NY5A,NY5B,NY5C,NY5D,Philly5A,Philly5B)])))

#Column B (Citations)
citations_5_colB<-as.data.table(colSums(subset(citations_colB[,.(Boston5A,Boston5B,DC5,NY5A,NY5B,NY5C,NY5D,Philly5A,Philly5B)])))

#Column C (Citations from same cluster as originating)
citations_columnc_5 <- as.data.table(colSums(subset(columnc[,.(sameBoston5A,sameBoston5B,sameDC5,sameNY5A,sameNY5B,sameNY5C,sameNY5D,samePhilly5A,samePhilly5B)])))


#Putting the table together
final_5_group<-cbind(originating_5_colA, citations_5_colB, citations_columnc_5)
names(final_5_group)[1] <- "Originating_Patents"
names(final_5_group)[2] <- "Citing_Patents"
names(final_5_group)[3] <- "From_Same_Cluster"

Cluster <- clnames5
final_5_group<-cbind(Cluster, final_5_group)

final_5_group<-adorn_totals(final_5_group, "row")


#Columns A to D
final_5_group[, "columnD"] <- (final_5_group[, 4] *100/ final_5_group[, 3])


#10 Mile
#Column A (Originating Patents)
originating_10_colA<-as.data.table(colSums(subset(originating[,.(Boston10,DC10,NY10,Philly10)])))

#Column B (Citations)
citations_10_colB<-as.data.table(colSums(subset(citations_colB[,.(Boston10,DC10,NY10,Philly10)])))

#Column C (Citations from same cluster as originating)
citations_columnc_10 <- as.data.table(colSums(subset(columnc[,.(sameBoston10,sameDC10,sameNY10,samePhilly10)])))

#Putting the table together
final_10_group<-cbind(originating_10_colA, citations_10_colB, citations_columnc_10)
names(final_10_group)[1] <- "Originating_Patents"
names(final_10_group)[2] <- "Citing_Patents"
names(final_10_group)[3] <- "From_Same_Cluster"
#Naming the rows
Cluster<-c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
final_10_group<-cbind(Cluster, final_10_group)
final_10_group<-adorn_totals(final_10_group, "row")

#Columns A to D
final_10_group[, "columnD"] <- final_10_group[, 4] *100/ final_10_group[, 3]


#20 Mile
#Column A (Originating Patents)
originating_20_colA<-as.data.table(colSums(subset(originating[,.(DC20,Boston20,NY20)])))

#Column B (Citations)
citations_20_colB<-as.data.table(colSums(subset(citations_colB[,.(DC20,Boston20,NY20)])))

#Column C (Citations from same cluster as originating)
citations_columnc_20 <- as.data.table(colSums(subset(columnc[,.(sameDC20,sameBoston20,sameNY20)])))

#Putting the table together
final_20_group<-cbind(originating_20_colA, citations_20_colB, citations_columnc_20)
names(final_20_group)[1] <- "Originating_Patents"
names(final_20_group)[2] <- "Citing_Patents"
names(final_20_group)[3] <- "From_Same_Cluster"
#Naming the rows
Cluster2<-c("Washington, DC","Boston, MA", "New York, NY")
final_20_group<-cbind(Cluster2, final_20_group)
final_20_group<-adorn_totals(final_20_group, "row")

#Columns A to D
final_20_group[, "columnD"] <- final_20_group[, 4] *100/ final_20_group[, 3]


#Table 1a 5 mile Cluster 
Table1a_5<-cbind(final_5_group,rand_table_5)
Table1a_5[,  c("Cluster2")  := NULL]
names(Table1a_5)[2] <- "Originating Patents"
names(Table1a_5)[3] <- "Citing Patents"
names(Table1a_5)[4] <- "From Same Cluster"
names(Table1a_5)[5] <- "Percent (C/B)"
names(Table1a_5)[6] <- "Matched Citing Patents*"
names(Table1a_5)[7] <- "From Same Cluster*"
names(Table1a_5)[8] <- "Percent (F/E)"
names(Table1a_5)[9] <- "Control Patents**"
names(Table1a_5)[10] <- "From Same Cluster**"
names(Table1a_5)[11] <- "Percent (I/H)"
names(Table1a_5)[12] <- "Location Differential (G/J)"

#Formating the tables

Table1a_5_1<-regulartable(Table1a_5, col_keys= c("Cluster","Originating Patents","Citing Patents", "From Same Cluster","Percent (C/B)", "col_1", 
                                                 "Matched Citing Patents*", "From Same Cluster*", "Percent (F/E)", "col_2",
                                                 "Control Patents**","From Same Cluster**", "Percent (I/H)","col_3","Location Differential (G/J)") )
zero_format <- function(x){
  sprintf("%.0f",x)
}
one_format <- function(x){
  sprintf("%.1f",x)
}
percent_format<- function(x){
  sprintf("%.2f %%",x)
}
Table1a_5_1<-set_formatter(Table1a_5_1,"Originating Patents" = zero_format,"Citing Patents" = zero_format, "From Same Cluster" = zero_format,
                           "Percent (C/B)" = percent_format, "Matched Citing Patents*" = zero_format, "From Same Cluster*"= zero_format, "Percent (F/E)"= percent_format,
                           "Control Patents**"=zero_format,"From Same Cluster**"= zero_format, "Percent (I/H)"=percent_format,"Location Differential (G/J)"=one_format)

Table1a_5_1 <- add_header(Table1a_5_1,"Cluster"= "Column", "Originating Patents" = "A","Citing Patents" = "B", "From Same Cluster" = "C",
                          "Percent (C/B)" = "D", "Matched Citing Patents*" = "E", "From Same Cluster*"= "F", "Percent (F/E)"= "G",
                          "Control Patents**"="H","From Same Cluster**"="I", "Percent (I/H)"="J","Location Differential (G/J)"="K", top= TRUE)
Table1a_5_1<-theme_box(Table1a_5_1)
Table1a_5_1 <- add_header(Table1a_5_1, "Cluster"=" ","Originating Patents" = " ", "Citing Patents"=" ","From Same Cluster"=" ","Percent (C/B)"=" ",
                          "Matched Citing Patents*"="Treatment Group", "From Same Cluster*"="Treatment Group","Percent (F/E)"="Treatment Group",
                          "Control Patents**"="Control Group", "From Same Cluster**"="Control Group", "Percent (I/H)"="Control Group", 
                          "Location Differential (G/J)" = " ", top = TRUE )

Table1a_5_1 <- merge_h(Table1a_5_1, part = "header")
Table1a_5_1 <- width(Table1a_5_1, j = ~ col_1, width = 0.1)
Table1a_5_1 <- width(Table1a_5_1, j = ~ col_2, width = 0.1)
Table1a_5_1 <- width(Table1a_5_1, j = ~ col_3, width = 0.1)

Table1a_5_1


doc <- read_docx()
doc <- body_add_flextable(doc, value = Table1a_5_1)
print(doc, target = "G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin/Table1atest.docx")




#Table 1b 10 mile Cluster 
Table1b_10<-cbind(final_10_group,rand_table_10)
Table1b_10[,  c("Cluster2")  := NULL]
names(Table1b_10)[2] <- "Originating Patents"
names(Table1b_10)[3] <- "Citing Patents"
names(Table1b_10)[4] <- "From Same Cluster"
names(Table1b_10)[5] <- "Percent (C/B)"
names(Table1b_10)[6] <- "Matched Citing Patents*"
names(Table1b_10)[7] <- "From Same Cluster*"
names(Table1b_10)[8] <- "Percent (F/E)"
names(Table1b_10)[9] <- "Control Patents**"
names(Table1b_10)[10] <- "From Same Cluster**"
names(Table1b_10)[11] <- "Percent (I/H)"
names(Table1b_10)[12] <- "Location Differential (G/J)"


#Formating the tables
Table1b_10_1<-regulartable(Table1b_10, col_keys= c("Cluster","Originating Patents","Citing Patents", "From Same Cluster","Percent (C/B)", "col_1", 
                                                   "Matched Citing Patents*", "From Same Cluster*", "Percent (F/E)", "col_2",
                                                   "Control Patents**","From Same Cluster**", "Percent (I/H)","col_3","Location Differential (G/J)") )

Table1b_10_1<-set_formatter(Table1b_10_1,"Originating Patents" = zero_format,"Citing Patents" = zero_format, "From Same Cluster" = zero_format,
                            "Percent (C/B)" = percent_format, "Matched Citing Patents*" = zero_format, "From Same Cluster*"= zero_format, "Percent (F/E)"= percent_format,
                            "Control Patents**"=zero_format,"From Same Cluster**"= zero_format, "Percent (I/H)"=percent_format,"Location Differential (G/J)"=one_format)

Table1b_10_1 <- add_header(Table1b_10_1,"Cluster"= "Column", "Originating Patents" = "A","Citing Patents" = "B", "From Same Cluster" = "C",
                           "Percent (C/B)" = "D", "Matched Citing Patents*" = "E", "From Same Cluster*"= "F", "Percent (F/E)"= "G",
                           "Control Patents**"="H","From Same Cluster**"="I", "Percent (I/H)"="J","Location Differential (G/J)"="K", top= TRUE)
Table1b_10_1<-theme_box(Table1b_10_1)
Table1b_10_1 <- add_header(Table1b_10_1,  "Cluster"=" ","Originating Patents" = " ", "Citing Patents"=" ","From Same Cluster"=" ","Percent (C/B)"=" ",
                           "Matched Citing Patents*"="Treatment Group", "From Same Cluster*"="Treatment Group","Percent (F/E)"="Treatment Group",
                           "Control Patents**"="Control Group", "From Same Cluster**"="Control Group", "Percent (I/H)"="Control Group", 
                           "Location Differential (G/J)" = " ", top = TRUE )
Table1b_10_1 <- merge_h(Table1b_10_1, part = "header")
Table1b_10_1 <- width(Table1b_10_1, j = ~ col_1, width = 0.1)
Table1b_10_1 <- width(Table1b_10_1, j = ~ col_2, width = 0.1)
Table1b_10_1 <- width(Table1b_10_1, j = ~ col_3, width = 0.1)

Table1b_10_1

doc <- read_docx()
doc <- body_add_flextable(doc, value = Table1b_10_1)
print(doc, target = "G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin/Table1b.docx")


#20 mile Cluster 
Table1b_20<-cbind(final_20_group,rand_table_20)
Table1b_20[,  c("Cluster2")  := NULL]
names(Table1b_20)[2] <- "Originating Patents"
names(Table1b_20)[3] <- "Citing Patents"
names(Table1b_20)[4] <- "From Same Cluster"
names(Table1b_20)[5] <- "Percent (C/B)"
names(Table1b_20)[6] <- "Matched Citing Patents*"
names(Table1b_20)[7] <- "From Same Cluster*"
names(Table1b_20)[8] <- "Percent (F/E)"
names(Table1b_20)[9] <- "Control Patents**"
names(Table1b_20)[10] <- "From Same Cluster**"
names(Table1b_20)[11] <- "Percent (I/H)"
names(Table1b_20)[12] <- "Location Differential (G/J)"

#Table 3a: Citation Location Differentials and Spatial Scale  

row1<-Table1a_5[10]
row2<-Table1b_10[5]
row3<-Table1b_20[4]

Table_3a <- rbind(row1, row2, row3, fill=TRUE)
Table_3a[,  c("Cluster","From Same Cluster","Percent (C/B)", "Matched Citing Patents*", "Control Patents**","From Same Cluster*","From Same Cluster**")  := NULL]

Cluster_size<-c("5-Mile","10-Mile","20-Mile") 
N_Cluster<-c(9,4,3)

Table_3a_1a<-cbind(Cluster_size,N_Cluster)

Table_3a<-cbind(Table_3a_1a,Table_3a)
names(Table_3a)[1] <- "Cluster Size"
names(Table_3a)[2] <- "# of Clusters"
names(Table_3a)[5] <- "Treatment Proportion"
names(Table_3a)[6] <- "Control Proportion"
names(Table_3a)[7] <- "Location Differential"


#Formating the tables
Table3a<-regulartable(Table_3a, col_keys= c("Cluster Size", "# of Clusters","Originating Patents","Citing Patents", "col_1", 
                                            "Treatment Proportion", "Control Proportion", "col_2", "Location Differential") )

Table3a<-set_formatter(Table3a,"Originating Patents" = zero_format,"Citing Patents" = zero_format, "Treatment Proportion"= percent_format,
                            "Control Proportion"=percent_format,"Location Differential"=one_format)
Table3a<-theme_box(Table3a)

Table3a <- width(Table3a, j = ~ col_1, width = 0.1)
Table3a <- width(Table3a, j = ~ col_2, width = 0.1)
Table3a


doc <- read_docx()
doc %>% #add_text(title="title") %>% 
  add_flextable(Table3a, landscape = TRUE) %>% 
  print(target="G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin/Table3a.docx")

