install.packages('knitr', dependencies = TRUE)
install.packages("data.table")
install.packages("dplyr")## dplyr provides the join functions
install.packages("formattable") #Give format to the tables
install.packages("janitor") #Get the total sum of each column
install.packages("flextable") #Give format to the tables
install.packages("tidyr") #For function "separate"
library(knitr)
library(flextable) #def need
library(formattable)
library(data.table) #def need
library(dplyr) #def need
library(janitor) #def need
library(officer) #def need
library(stringr) #def need
library(tidyr) #def need

setwd("G:/MAX-Filer/Collab/Labs-kbuzard-S18/Admin/Patents/RSUE")

zero_format <- function(x){
  sprintf("%.0f",x)
}
one_format <- function(x){
  sprintf("%.1f",x)
}
percent_format<- function(x){
  sprintf("%.2f %%",x)
}


#---------Tables without randomization. Columns A, B, C, D-------------------

#Column A (Originating Patents)*/  
colA <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n) {
  #First Merge originating with clustpatents*/
  clustpatents <- select(clustpatents,one_of("NE_Plots_2",xnames5,xnames10,xnames20))
  
  aux_clustpatents<-rename(clustpatents, cited = NE_Plots_2)
  
  originating<-as.data.table(left_join(originating, aux_clustpatents, by="cited"))
}


#Match citing data (from Matching dataset) with geographic information from clustpatents

#For Column B (Citations)
colB <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n) {
  setorder(citations, "cited")
  
  citations <- rename(citations, citing = patent)
  clustpatents <- rename(clustpatents,  cited = NE_Plots_2)
  
  citations_colB <- as.data.table(left_join(citations,clustpatents, by = "cited"))
}


#Column C (Citations from same cluster as originating)
colC <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n) {
  
  xnames <- c(xnames5,xnames10,xnames20)
  clustpatents <- select(clustpatents,one_of("NE_Plots_2",xnames))
  aux_clustpatents<-rename(clustpatents, cited = NE_Plots_2)
  clustpatents_cit <- setnames(aux_clustpatents,xnames,str_c("c",xnames))
  clustpatents_cit <- rename(clustpatents_cit, citing = cited)
  
  setorder(citations_colB, "cited")
  citations_colc <- as.data.table(left_join(citations_colB,clustpatents_cit, by="citing"))
  
  #Transforming NA into 0 to use a binary analysis
  for(j in seq_along(citations_colc)){
    set(citations_colc, i=which(is.na(citations_colc[[j]])), j=j, value=0)
  }
  
  for(i in 1:length(xnames)) {
    xname <- xnames[i]
    yname <- paste0('c', xname)
    outname <- paste0('same', xname)
    x <- citations_colc[[xname]]
    y <- citations_colc[[yname]]
    z <- 1 * (x == 1 & y == 1)
    citations_colc[[outname]] <- z
  }
  
  return(citations_colc)
}



#------------5 Mile Table------------------------------------
table5m <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n) {
  final_5_group2 <- as.data.table(cbind(as.data.table(columnsEFI[1]), as.data.table(columnsEFI[2])))
  
  names(final_5_group2)[1] <- "Matched_Citing_Patents"
  names(final_5_group2)[2] <- "From_Same_Cluster"
  
  Cluster2 <- clnames5
  final_5_group2 <- cbind(Cluster2, final_5_group2)
  
  final_5_group2 <- adorn_totals(final_5_group2, "row")
  
  final_5_group2[, "columnG"] <- (final_5_group2[, 3] *100/ final_5_group2[, 2])
  
  #Columns H, I, J,K
  final_5_group3 <- as.data.table(cbind(as.data.table(columnsEFI[1]), as.data.table(columnsEFI[3])))
  
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
  rand_table_5[, "columnL"] <- (as.data.table(columnsEFI[10]))
  
  #Column A (Originating Patents)
  originating_5_colA <- as.data.table(colSums(select(originating,one_of(xnames5))))
  
  #Column B (Citations)
  citations_5_colB <- as.data.table(colSums(select(citations_colB,one_of(xnames5))))
  
  #Column C (Citations from same cluster as originating)
  citations_columnc_5 <- as.data.table(colSums(select(citations_colc,one_of(str_c("same",xnames5)))))
  
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
  
  
  #Format table
  Table1a_5<-as.data.table(cbind(final_5_group,rand_table_5))
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
  names(Table1a_5)[13] <- "Location Differential (Ave. of Ratios)"
  
  Table1a_5_1<-regulartable(Table1a_5, col_keys= c("Cluster","Originating Patents","Citing Patents", "From Same Cluster","Percent (C/B)", "col_1", 
                                                   "Matched Citing Patents*", "From Same Cluster*", "Percent (F/E)", "col_2",
                                                   "Control Patents**","From Same Cluster**", "Percent (I/H)","col_3","Location Differential (G/J)","Location Differential (Ave. of Ratios)") )
  
  Table1a_5_1 <- set_formatter(Table1a_5_1,"Originating Patents" = zero_format,"Citing Patents" = zero_format, "From Same Cluster" = zero_format,
                               "Percent (C/B)" = percent_format, "Matched Citing Patents*" = zero_format, "From Same Cluster*"= zero_format, "Percent (F/E)"= percent_format,
                               "Control Patents**"=zero_format,"From Same Cluster**"= zero_format, "Percent (I/H)"=percent_format,"Location Differential (G/J)"=one_format,"Location Differential (Ave. of Ratios)"=one_format)
  
  Table1a_5_1 <- add_header(Table1a_5_1,"Cluster"= "Column", "Originating Patents" = "A","Citing Patents" = "B", "From Same Cluster" = "C",
                            "Percent (C/B)" = "D", "Matched Citing Patents*" = "E", "From Same Cluster*"= "F", "Percent (F/E)"= "G",
                            "Control Patents**"="H","From Same Cluster**"="I", "Percent (I/H)"="J","Location Differential (G/J)"="K", "Location Differential (Ave. of Ratios)"="L", top= TRUE)
  Table1a_5_1 <- theme_box(Table1a_5_1)
  Table1a_5_1 <- add_header(Table1a_5_1, "Cluster"=" ","Originating Patents" = " ", "Citing Patents"=" ","From Same Cluster"=" ","Percent (C/B)"=" ",
                            "Matched Citing Patents*"="Treatment Group", "From Same Cluster*"="Treatment Group","Percent (F/E)"="Treatment Group",
                            "Control Patents**"="Control Group", "From Same Cluster**"="Control Group", "Percent (I/H)"="Control Group", 
                            "Location Differential (G/J)" = " ", top = TRUE )
  
  Table1a_5_1 <- merge_h(Table1a_5_1, part = "header")
  Table1a_5_1 <- fontsize(Table1a_5_1, part = "all", size = 7)
  Table1a_5_1 <- font(Table1a_5_1, part = "all", fontname = "Times")
  Table1a_5_1 <- width(Table1a_5_1, j = c("Cluster"), width = 2.4)
  Table1a_5_1 <- width(Table1a_5_1, j = c("Originating Patents","Citing Patents", "From Same Cluster", "Matched Citing Patents*", 
                                          "From Same Cluster*", "Control Patents**","From Same Cluster**", 
                                          "Location Differential (G/J)", "Location Differential (Ave. of Ratios)"), width = 0.65)
  Table1a_5_1 <- width(Table1a_5_1, j = c("Percent (C/B)", "Percent (F/E)", "Percent (I/H)"), width = 0.55)
  Table1a_5_1 <- width(Table1a_5_1, j = ~ col_1, width = 0.1)
  Table1a_5_1 <- width(Table1a_5_1, j = ~ col_2, width = 0.1)
  Table1a_5_1 <- width(Table1a_5_1, j = ~ col_3, width = 0.1)
  Table1a_5_1 <- height(Table1a_5_1, height = 0.15, part = "body")
  Table1a_5_1 <- height(Table1a_5_1, i = 2, height = 0.15, part = "header")
  Table1a_5_1 <- height(Table1a_5_1, i = 1, height = 0.15, part = "header")
  Table1a_5_1 <- padding(Table1a_5_1, padding.top = 0, padding.bottom = 0, part = "body")
  #Table1a_5_1 <- border_remove(Table1a_5_1)
  #Table1a_5_1 <- border_outer(Table1a_5_1, border = std_border, part = "body")
  
    rm(final_5_group2, final_5_group3,Cluster2,Cluster3)
  
  return(Table1a_5_1)
}




#-------------------10 Mile Table----------------------------
table10m <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n){  
  final_10_group2 <- as.data.table(cbind(as.data.table(columnsEFI[4]), as.data.table(columnsEFI[5])))
  
  #This part calcualte the totals
  names(final_10_group2)[1] <- "Matched_Citing_Patents"
  names(final_10_group2)[2] <- "From_Same_Cluster"
  
  Cluster2 <- clnames10
  final_10_group2 <- cbind(Cluster2, final_10_group2)
  
  final_10_group2 <- adorn_totals(final_10_group2, "row")
  
  final_10_group2[, "columnG"] <- (final_10_group2[, 3] *100/ final_10_group2[, 2])
  
  
  #Columns H, I, J,K
  final_10_group3<-as.data.table(cbind(as.data.table(columnsEFI[4]), as.data.table(columnsEFI[6])))
  
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
  rand_table_10[, "columnL"] <- (as.data.table(columnsEFI[11]))
  
  rm(final_10_group2, final_10_group3, Cluster2, Cluster3)
  
  
  #Column A (Originating Patents)
  originating_10_colA <- as.data.table(colSums(select(originating,one_of(xnames10))))
  
  #Column B (Citations)
  citations_10_colB <- as.data.table(colSums(select(citations_colB,one_of(xnames10))))
  
  #Column C (Citations from same cluster as originating)
  citations_columnc_10 <- as.data.table(colSums(select(citations_colc,one_of(str_c("same",xnames10)))))
  
  #Putting the table together
  final_10_group<-cbind(originating_10_colA, citations_10_colB, citations_columnc_10)
  names(final_10_group)[1] <- "Originating_Patents"
  names(final_10_group)[2] <- "Citing_Patents"
  names(final_10_group)[3] <- "From_Same_Cluster"
  #Naming the rows
  Cluster<-clnames10
  final_10_group<-cbind(Cluster, final_10_group)
  final_10_group<-adorn_totals(final_10_group, "row")
  
  #Columns A to D
  final_10_group[, "columnD"] <- final_10_group[, 4] *100/ final_10_group[, 3]
  
  #-------Format 10 mile Table----------------------------------------------------
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
  names(Table1b_10)[13] <- "Location Differential (Ave. of Ratios)"
  
  #Formating the tables
  Table1b_10_1<-regulartable(Table1b_10, col_keys= c("Cluster","Originating Patents","Citing Patents", "From Same Cluster","Percent (C/B)", "col_1", 
                                                     "Matched Citing Patents*", "From Same Cluster*", "Percent (F/E)", "col_2",
                                                     "Control Patents**","From Same Cluster**", "Percent (I/H)","col_3","Location Differential (G/J)","Location Differential (Ave. of Ratios)") )
  Table1b_10_1 <- fontsize(Table1b_10_1, part = "all", size = 7)
  Table1b_10_1 <- font(Table1b_10_1, part = "all", fontname = "Times")
  Table1b_10_1 <- height(Table1b_10_1, height = 0.15, part = "body")
  Table1b_10_1<-set_formatter(Table1b_10_1,"Originating Patents" = zero_format,"Citing Patents" = zero_format, "From Same Cluster" = zero_format,
                              "Percent (C/B)" = percent_format, "Matched Citing Patents*" = zero_format, "From Same Cluster*"= zero_format, "Percent (F/E)"= percent_format,
                              "Control Patents**"=zero_format,"From Same Cluster**"= zero_format, "Percent (I/H)"=percent_format,"Location Differential (G/J)"=one_format,"Location Differential (Ave. of Ratios)"=one_format)
  
  Table1b_10_1 <- add_header(Table1b_10_1,"Cluster"= "Column", "Originating Patents" = "A","Citing Patents" = "B", "From Same Cluster" = "C",
                             "Percent (C/B)" = "D", "Matched Citing Patents*" = "E", "From Same Cluster*"= "F", "Percent (F/E)"= "G",
                             "Control Patents**"="H","From Same Cluster**"="I", "Percent (I/H)"="J","Location Differential (G/J)"="K", "Location Differential (Ave. of Ratios)"="L", top= TRUE)
  Table1b_10_1<-theme_box(Table1b_10_1)
  Table1b_10_1 <- add_header(Table1b_10_1,  "Cluster"=" ","Originating Patents" = " ", "Citing Patents"=" ","From Same Cluster"=" ","Percent (C/B)"=" ",
                             "Matched Citing Patents*"="Treatment Group", "From Same Cluster*"="Treatment Group","Percent (F/E)"="Treatment Group",
                             "Control Patents**"="Control Group", "From Same Cluster**"="Control Group", "Percent (I/H)"="Control Group", 
                             "Location Differential (G/J)" = " ", top = TRUE )
  Table1b_10_1 <- height(Table1b_10_1, i = 2, height = 0.15, part = "header")
  Table1b_10_1 <- height(Table1b_10_1, i = 1, height = 0.15, part = "header")
  Table1b_10_1 <- merge_h(Table1b_10_1, part = "header")
  Table1b_10_1 <- width(Table1b_10_1, j = c("Cluster"), width = 2.4)
  Table1b_10_1 <- width(Table1b_10_1, j = c("Originating Patents","Citing Patents", "From Same Cluster", 
                                            "Matched Citing Patents*", "From Same Cluster*", 
                                            "Control Patents**","From Same Cluster**", "Location Differential (G/J)", "Location Differential (Ave. of Ratios)"), width = 0.65)
  Table1b_10_1 <- width(Table1b_10_1, j = c("Percent (C/B)", "Percent (F/E)", "Percent (I/H)"), width = 0.55)
  Table1b_10_1 <- width(Table1b_10_1, j = ~ col_1, width = 0.1)
  Table1b_10_1 <- width(Table1b_10_1, j = ~ col_2, width = 0.1)
  Table1b_10_1 <- width(Table1b_10_1, j = ~ col_3, width = 0.1)
  Table1b_10_1 <- padding(Table1b_10_1, padding.top = 0, padding.bottom = 0, part = "body")
  
  return(Table1b_10_1)
}

#-------Table 3a: 5,10,20 mile Clusters-------------------------------------------
table3 <- function(xnames5,xnames10,xnames20,clnames5,clnames10,clnames20,n){
  final_20_group2 <- as.data.table(cbind(as.data.table(columnsEFI[7]), as.data.table(columnsEFI[8])))
  
  #This part calcualte the totals
  names(final_20_group2)[1] <- "Matched_Citing_Patents"
  names(final_20_group2)[2] <- "From_Same_Cluster"
  
  Cluster2 <- clnames20
  final_20_group2 <- cbind(Cluster2, final_20_group2)
  
  final_20_group2 <- adorn_totals(final_20_group2, "row")
  
  final_20_group2[, "columnG"] <- (final_20_group2[, 3]*100 / final_20_group2[, 2])
  
  #Columns H, I, J
  final_20_group3 <- as.data.table(cbind(as.data.table(columnsEFI[7]), as.data.table(columnsEFI[9])))
  
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
  
  
  #Column A (Originating Patents)
  originating_20_colA <- as.data.table(colSums(select(originating,one_of(xnames20))))
  
  #Column B (Citations)
  citations_20_colB <- as.data.table(colSums(select(citations_colB,one_of(xnames20))))
  
  #Column C (Citations from same cluster as originating)
  citations_columnc_20 <- as.data.table(colSums(select(citations_colc,one_of(str_c("same",xnames20)))))
  
  #Putting the table together
  final_20_group<-cbind(originating_20_colA, citations_20_colB, citations_columnc_20)
  names(final_20_group)[1] <- "Originating_Patents"
  names(final_20_group)[2] <- "Citing_Patents"
  names(final_20_group)[3] <- "From_Same_Cluster"
  #Naming the rows
  Cluster2<-clnames20
  final_20_group<-cbind(Cluster2, final_20_group)
  final_20_group<-adorn_totals(final_20_group, "row")
  
  #Columns A to D
  final_20_group[, "columnD"] <- final_20_group[, 4] *100/ final_20_group[, 3]
  
  
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
  
  row1<-Table5m$body$dataset[length(xnames5)+1,]
  row2<-Table10m$body$dataset[length(xnames10)+1,]
  row3<-Table1b_20[length(xnames20)+1,]
  
  Table_3a <- rbind(row1, row2, row3, fill=TRUE)
  Table_3a[,  c("Cluster","From Same Cluster","Percent (C/B)", "Matched Citing Patents*", "Control Patents**","From Same Cluster*","From Same Cluster**")  := NULL]
  
  Cluster_size<-c("5-Mile","10-Mile","20-Mile") 
  N_Cluster<-c(length(xnames5),length(xnames10),length(xnames20))
  
  Table_3a_1a<-cbind(Cluster_size,N_Cluster)
  
  Table_3a<-cbind(Table_3a_1a,Table_3a)
  names(Table_3a)[1] <- "Cluster Size"
  names(Table_3a)[2] <- "# of Clusters"
  names(Table_3a)[5] <- "Treatment Proportion"
  names(Table_3a)[6] <- "Control Proportion"
  names(Table_3a)[7] <- "Location Differential"
  
  
  #Formating the tables
  Table3a <- regulartable(Table_3a, col_keys= c("Cluster Size", "# of Clusters","Originating Patents","Citing Patents", "col_1", 
                                                "Treatment Proportion", "Control Proportion", "col_2", "Location Differential") )
  
  Table3a <- set_formatter(Table3a,"Originating Patents" = zero_format,"Citing Patents" = zero_format, "Treatment Proportion"= percent_format,
                           "Control Proportion"=percent_format,"Location Differential"=one_format)
  Table3a <- set_header_labels(Table3a, "col_1" = " ", "col_2" = " ")
  Table3a <- font(Table3a, part = "all", fontname = "Times")
  Table3a <- theme_box(Table3a)
  Table3a <- merge_h(Table3a, part = "header")
  Table3a <- width(Table3a, j = c("Cluster Size"), width = 1.5)
  Table3a <- width(Table3a, j = c("# of Clusters","Originating Patents","Citing Patents", 
                                  "Treatment Proportion", "Control Proportion", "Location Differential"), width = 1.2)  
  Table3a <- width(Table3a, j = ~ col_1, width = 0.1)
  Table3a <- width(Table3a, j = ~ col_2, width = 0.1)
  
  return(Table3a)
  
}


#--------Read in data and cluster names-----------------------
n = 999

#Northeast baseline
columnsEFI <- readRDS("columnsEFI_NEbaseline")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")
citations <- fread("SAScitations.csv")
clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))


xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

ne_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

originating <- do.call(colA,ne_base)
citations_colB <- do.call(colB,ne_base)
citations_colc <- do.call(colC,ne_base)

Table5m <- do.call(table5m,ne_base)
Table10m <- do.call(table10m,ne_base)  
Table3a <- do.call(table3,ne_base)  
Table5m_ne_base <- Table5m
Table10m_ne_base <- Table10m


#California baseline
citations <- fread("SAScitationsCA.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

columnsEFI <- readRDS("columnsEFI_CAbaseline")

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

ca_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

originating <- do.call(colA,ca_base)
citations_colB <- do.call(colB,ca_base)
citations_colc <- do.call(colC,ca_base)

Table5m <- do.call(table5m,ca_base)
Table10m <- do.call(table10m,ca_base)  
Table3b <- do.call(table3,ca_base)  
Table5m_ca_base <- Table5m
Table10m_ca_base <- Table10m


#NE Stem
citations <- fread("SAScitationsNEstem.csv")
originating <- fread("SASoriginatingNEstem.csv")
clustpatents <- fread("SASclustpatentsNEstem.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

columnsEFI <- readRDS("columnsEFI_NEstem")

xnames5 <- c("DC5A","DC5B","Balt5","Wilm5","Philly5A","Philly5B","NY5A","NY5B","CT5A","CT5B","CT5C","CT5D","Boston5A","Boston5B","Boston5C","Bing5","Syracuse5","Buffalo5","Pitt5A","Pitt5B")
xnames10 <- c("Richmond10","DC10A","DC10B","Philly10A","Philly10B","Pitt10","Bing10","Syracuse10","Rochester10","Buffalo10","Boston10","NY10")
xnames20 <- NULL

clnames5 <- c("Bethesda-Rockville, MD-Vienna, VA","Columbia-Laurel, MD","Phoenix-Cockeysville, MD","Wilmington, DE","King of Prussia, PA","Philadephia, PA","Princeton, NJ-New York, NY","Long Island, NY","Danbury, CT","Stratford, CT","North Haven,CT","Hartford, CT","Hudson-Westborough, MA","Boston-Cambridge, MA","Nashua, NH","Binghamton, NY","Syrcuse, NY","Buffalo, NY","Pittsburgh, PA","Pittsburgh-Verona, PA")
clnames10 <- c("Richmond, VA","Washington, DC-Baltimore, MD","Hagerstown, MD","Lancaster,PA","Philadelphia,PA-Wilmington,DC-Cherry Hill, NJ","Pittsburgh, PA","Binghamton, NY","Syracuse, NY","Rochester,NY","Buffalo, NY","Boston, MA","New York, NY-Northern NJ-CT")
clnames20 <- NULL

ne_stem <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

originating <- do.call(colA,ne_stem)
citations_colB <- do.call(colB,ne_stem)
citations_colc <- do.call(colC,ne_stem)

Table5m_ne_stem <- do.call(table5m,ne_stem)
Table10m_ne_stem <- do.call(table10m,ne_stem)  


#CA Stem
citations <- fread("SAScitationsCAstem.csv")
originating <- fread("SASoriginatingCAstem.csv")
clustpatents <- fread("SASclustpatentsCAstem.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

columnsEFI <- readRDS("columnsEFI_CAstem")

xnames5 <- c("SD5A","SD5B","OC5","MAL5","SB5","SF5A","SF5B","SF5C")
xnames10 <- c("SD10","OC10","MAL10","SB10","SF10","SF10B")
xnames20 <- NULL

clnames5 <- c("San Diego-La Jolla","Carslbad","Irvine","Camarillo","Santa Barbara","San Jose-Santa Clara","Pleasanton","Santa Rosa")
clnames10 <- c("San Diego","Anaheim-Irving","Oxnard-Camarillo","Santa Barbara","San Francisco-Palo Alto-San Jose","Santa Rosa")
clnames20 <- NULL

ca_stem <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

originating <- do.call(colA,ca_stem)
citations_colB <- do.call(colB,ca_stem)
citations_colc <- do.call(colC,ca_stem)

Table5m_ca_stem <- do.call(table5m,ca_stem)
Table10m_ca_stem <- do.call(table10m,ca_stem)  


#Northeast subclass
citations <- fread("SAScitationsNEsub.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

columnsEFI <- readRDS("columnsEFI_NEsub")

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

ne_sub <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

originating <- do.call(colA,ne_sub)
citations_colB <- do.call(colB,ne_sub)
citations_colc <- do.call(colC,ne_sub)

Table5m_ne_sub <- do.call(table5m,ne_sub)
Table10m_ne_sub <- do.call(table10m,ne_sub)  


#California subclass
citations <- fread("SAScitationsCAsub.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

columnsEFI <- readRDS("columnsEFI_CAsub")

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

ca_sub <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

originating <- do.call(colA,ca_sub)
citations_colB <- do.call(colB,ca_sub)
citations_colc <- do.call(colC,ca_sub)

Table5m_ca_sub <- do.call(table5m,ca_sub)
Table10m_ca_sub <- do.call(table10m,ca_sub)  


#---------------------Unfinished-----------------------

#MSA baseline
columnsEFI <- readRDS("columnsEFI_MSA")

xnames5 <- c("BOS","LA","NY","PHL","SD","SF","DC")
xnames10 <- NULL
xnames20 <- NULL

clnames5 <- c("Boston","Los Angeles","New York","Philadelphia","San Diego","San Francisco","Washington, DC")
clnames10 <- NULL
clnames20 <- NULL

msa_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                 "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- msa_base
y <- "MSA"


#--------Print to Word Document---------------------------------------------------
fn1 <- "Sources: NBER Patent Data Project and authors' calculations."  
fn2 <- "*The subset of citing patents for which we obtained a similar control patent. See text for details."
fn3 <- "**Control Patents are chosen to have the same three-digit technology classification as the citing 
patent, and their application date must be within a one-year window of the citing patent's application date.
These control patents are chosen with replacement sampling. We eliminate self-citations and do not allow
controls to be drawn from patents assigned to the same firm to which the originating patent is assigned."
fn4 <- "Control Patents are chosen to have the same three-digit technology classification as the citing 
patent, and their application date must be within a one-year window of the citing patent's application date.
These control patents are chosen with replacement sampling. We eliminate self-citations and do not allow
controls to be drawn from patents assigned to the same firm to which the originating patent is assigned."
fn5 <- "The clusters identified in the above table are based on STEM workers as the backcloth. Note that the
cluster definitions change because the backcloth changed to STEM workers instead of manufacturing workers
as used in Tables 1 and 2."
fn6 <- "**Control Patents are chosen to have the same six-digit technology classification as the citing 
patent, and their application date must be within a one-year window of the citing patent's application date.
These control patents are chosen with replacement sampling. We eliminate self-citations and do not allow
controls to be drawn from patents assigned to the same firm to which the originating patent is assigned."



fpt = fp_text(font.size = 8, font.family = "Times")
pad <- fp_par(padding.top = 2)

titles = fp_text(font.size = 10, font.family = "Times")
title_pad <- fp_par(padding.bottom = 2, text.align = "center")

print_tables <- function() {
 doc <- read_docx() %>%
  body_add_par(value = "Tables", style = "centered") %>%
  body_end_section_continuous() %>%
  body_add_fpar(fpar(ftext("Table 1a: Five-Mile Clusters in the Northeast corridor, Baseline Results", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table5m_ne_base) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("Table 1b: Ten-Mile Clusters in the Northeast corridor, Baseline Results", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table10m_ne_base) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn2, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn3, prop = fpt), fp_p = pad )) %>%
  body_add_break() %>% 
  body_add_fpar(fpar(ftext("Table 2a: Five-Mile Clusters in California, Baseline Results", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table5m_ca_base) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("Table 2b: 10-Mile Clusters in California, Baseline Results", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table10m_ca_base) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn2, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn3, prop = fpt), fp_p = pad )) %>%
  body_add_break() %>% 
  body_add_fpar(fpar(ftext("Table 3a: Citation Location Differentials and Spatial Scale (Northeast corridor)", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table3a) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext("", prop = fpt))) %>%
  body_add_fpar(fpar(ftext("Table 3b: Citation Location Differentials and Spatial Scale (California)", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table3b) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn4, prop = fpt), fp_p = pad )) %>%
  body_add_break() %>% 
  body_add_fpar(fpar(ftext("Table 5a: Five-Mile Clusters in the Northeast Corridor, STEM Worker Clusters", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table5m_ne_stem) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("Table 5b: 10-Mile Clusters in the Northeast Corridor, STEM Worker Clusters", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table10m_ne_stem) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn2, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn3, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn5, prop = fpt), fp_p = pad )) %>%
  body_add_break() %>% 
  body_add_fpar(fpar(ftext("Table 6a: Five-Mile Clusters in California, STEM Worker Clusters", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table5m_ca_stem) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("Table 6b: 10-Mile Clusters in California, STEM Worker Clusters", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table10m_ca_stem) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn2, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn3, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn5, prop = fpt), fp_p = pad )) %>%
  body_add_break() %>% 
  body_add_fpar(fpar(ftext("Table 7a: Five-Mile Clusters in the Northeast Corridor, Disaggregated Subclasses", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table5m_ne_sub) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("Table 7b: 10-Mile Clusters in the Northeast Corridor, Disaggregated Subclasses", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table10m_ne_sub) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn2, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn6, prop = fpt), fp_p = pad )) %>%
  body_add_break() %>% 
  body_add_fpar(fpar(ftext("Table 8a: Five-Mile Clusters in California, Disaggregated Subclasses", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table5m_ca_sub) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("Table 8b: 10-Mile Clusters in California, Disaggregated Subclasses", prop = titles), fp_p = title_pad )) %>% 
  body_add_flextable(value = Table10m_ca_sub) %>%
  body_add_fpar(fpar(ftext(fn1, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn2, prop = fpt), fp_p = pad )) %>%
  body_add_fpar(fpar(ftext(fn6, prop = fpt), fp_p = pad )) %>%
  body_end_section_landscape() %>%
  print(target = "tables.docx")  
}

print_tables()
