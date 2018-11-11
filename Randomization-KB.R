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

zero_format <- function(x){
  sprintf("%.0f",x)
}
one_format <- function(x){
  sprintf("%.1f",x)
}
percent_format<- function(x){
  sprintf("%.2f %%",x)
}


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
  columnL_10 <- round(locdiff[length(xnames5)+1:length(xnames10)], digits = 1)

  columnE_20 <- round(clusterMeans[seq((length(xnames5)+length(xnames10))*3+1, by=3, length.out = length(xnames20)),])
  columnF_20 <- round(clusterMeans[seq((length(xnames5)+length(xnames10))*3+2, by=3, length.out = length(xnames20)),])
  columnI_20 <- round(clusterMeans[seq((length(xnames5)+length(xnames10))*3+3, by=3, length.out = length(xnames20)),])
  columnL_20 <- round(locdiff[length(xnames5)+length(xnames10)+1:length(xnames20)], digits = 1)
  
  cols <- list(columnE_5,columnF_5,columnI_5,columnE_10,columnF_10,columnI_10,columnE_20,columnF_20,columnI_20,columnL_5,columnL_10,columnL_20)
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
  
  
  #Formating the tables
  Table1b_10_1<-regulartable(Table1b_10, col_keys= c("Cluster","Originating Patents","Citing Patents", "From Same Cluster","Percent (C/B)", "col_1", 
                                                     "Matched Citing Patents*", "From Same Cluster*", "Percent (F/E)", "col_2",
                                                     "Control Patents**","From Same Cluster**", "Percent (I/H)","col_3","Location Differential (G/J)") )
  Table1b_10_1 <- fontsize(Table1b_10_1, part = "all", size = 7)
  Table1b_10_1 <- height(Table1b_10_1, height = 0.15, part = "body")
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
  Table1b_10_1 <- height(Table1b_10_1, i = 2, height = 0.15, part = "header")
  Table1b_10_1 <- height(Table1b_10_1, i = 1, height = 0.15, part = "header")
  Table1b_10_1 <- merge_h(Table1b_10_1, part = "header")
  Table1b_10_1 <- width(Table1b_10_1, j = c("Cluster"), width = 2.4)
  Table1b_10_1 <- width(Table1b_10_1, j = c("Originating Patents","Citing Patents", "From Same Cluster", 
                                            "Matched Citing Patents*", "From Same Cluster*", 
                                            "Control Patents**","From Same Cluster**", "Location Differential (G/J)"), width = 0.65)
  Table1b_10_1 <- width(Table1b_10_1, j = c("Percent (C/B)", "Percent (F/E)", "Percent (I/H)"), width = 0.55)
  Table1b_10_1 <- width(Table1b_10_1, j = ~ col_1, width = 0.1)
  Table1b_10_1 <- width(Table1b_10_1, j = ~ col_2, width = 0.1)
  Table1b_10_1 <- width(Table1b_10_1, j = ~ col_3, width = 0.1)
  
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
  Table3a <- theme_box(Table3a)
  Table3a <- merge_h(Table3a, part = "header")
  Table3a <- width(Table3a, j = c("Cluster Size"), width = 1.5)
  Table3a <- width(Table3a, j = c("# of Clusters","Originating Patents","Citing Patents", 
                                  "Treatment Proportion", "Control Proportion", "Location Differential"), width = 1.2)  
  Table3a <- width(Table3a, j = ~ col_1, width = 0.1)
  Table3a <- width(Table3a, j = ~ col_2, width = 0.1)

  return(Table3a)

}

#--------Print to Word Document---------------------------------------------------
fn1 <- "Sources: NBER Patent Data Project and authors’ calculations."  
fn2 <- "*The subset of citing patents for which we obtained a similar control patent. See text for details."
fn3 <- "†Control Patents are chosen to have the same three-digit technology classification as the citing 
patent, and their application date must be within a one-year window of the citing patent’s application date.
These control patents are chosen with replacement sampling. We eliminate self-citations and do not allow 
controls to be drawn from patents assigned to the same firm to which the originating patent is assigned."

fpt = fp_text(font.size = 7, font.family = "Arial")

print_tables <- function() {
  doc <- read_docx() %>%
    body_add_par(value = "Tables", style = "centered") %>%
    body_end_section_continuous() %>%
    body_add_par(value = "Table 1a: Five-Mile Clusters in the Northeast corridor, Baseline Results", style = "centered") %>% 
    body_add_flextable(value = Table5m) %>%
    body_add_par("", style = "Normal") %>%
    body_add_par(value = "Table 1b: 10-Mile Clusters in the Northeast corridor, Baseline Results", style = "centered") %>% 
    body_add_flextable(value = Table10m) %>%
    body_add_fpar(fpar(ftext(fn1, prop = fpt))) %>%
    body_add_fpar(fpar(ftext(fn2, prop = fpt))) %>%
    body_add_fpar(fpar(ftext(fn3, prop = fpt))) %>%
    body_add_break() %>% 
    body_add_par(value = "Table 3a: Citation Location Differentials and Spatial Scale (Northeast corridor)", style = "centered") %>% 
    body_add_flextable(value = Table3a) %>%
    body_add_fpar(fpar(ftext(fn1, prop = fpt))) %>%
    body_add_fpar(fpar(ftext(fn3, prop = fpt))) %>%
    body_end_section_landscape() %>%
    print(target = "test.docx")  
}

print_one_table <- function() {
  doc <- read_docx() %>%
    body_add_par(value = "Tables", style = "centered") %>%
    body_end_section_continuous() %>%
    body_add_par(value = "Table 1a: Five-Mile Clusters in the Northeast corridor, Baseline Results", style = "centered") %>% 
    body_add_flextable(value = Table5m) %>%
    body_add_par("", style = "Normal") %>%
    body_add_fpar(fpar(ftext(fn1, prop = fpt))) %>%
    body_add_fpar(fpar(ftext(fn2, prop = fpt))) %>%
    body_add_fpar(fpar(ftext(fn3, prop = fpt))) %>%
    body_end_section_landscape() %>%
    print(target = "test.docx")  
}

#--------Read in data and cluster names-----------------------

#Northeast baseline
citations <- fread("SAScitations.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_NEbaseline")
list_of_matches <- unlist(list_of_matches,recursive = F)

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("Boston5A","Boston5B","DC5","NY5A","NY5B","NY5C","NY5D","Philly5A","Philly5B")
xnames10 <- c("Boston10","DC10","NY10","Philly10")
xnames20 <- c("Boston20","DC20","NY20")

clnames5 <- c("Framingham-Marlborough-Westborough, MA","Boston-Cambridge-Waltham-Woburn, MA", "Silver Spring-Bethesda, MD-McLean, VA",
              "Trenton-Princeton, NJ","Parsippany-Morristown-Union, NJ", "Greenwich-Stamford, CT-Scarsdale, NY","Stratford-Milford-CT",
              "Conshohocken-King of Prussia-West Chester, PA", "Wilmington-New Castle, DE")
clnames10 <- c("Boston, MA","Washington, DC", "New York, NY", "Philadelphia, PA")
clnames20 <- c("Washington, DC","Boston, MA", "New York, NY")

n=3

ne_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
          "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
x <- ne_base
y <- "ne_base"

#California baseline
citations <- fread("SAScitationsCA.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matchesCA <- readRDS("list_of_matches_CAbaseline")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

n=3

ca_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ca_base


#MSA baseline
citations <- fread("SAScitationsMSA.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingMSA.csv")
clustpatents <- fread("SASclustpatentsMSA.csv")

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

n=2

msa_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- msa_base
y <- "msa"

#Northeast subclass
citations <- fread("SAScitationsNEsub.csv")
possiblenclass <- fread("SASpossiblenclassSub.csv")
originating <- fread("SASoriginating.csv")
clustpatents <- fread("SASclustpatents.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))

citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
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

n=3

ne_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)
x <- ne_base
y <- "NEsub"

#California subclass
citations <- fread("SAScitationsCAsub.csv")
possiblenclass <- fread("SASpossiblenclassSub.csv")
originating <- fread("SASoriginatingCA.csv")
clustpatents <- fread("SASclustpatentsCA.csv")

clustpatents <- as.data.table(lapply(clustpatents, as.numeric))
clustpatents <- rename(clustpatents,  NE_Plots_2 = 1)
citations <- separate(citations, nclass, c("nclass","subclass","subclass2"), convert = TRUE)
citations <- as.data.frame(subset(citations[(!is.na(citations[,nclass]))]))

list_of_matches <- readRDS("list_of_matches_CAsub")

out <- data.frame(matrix(0, nrow = 1, ncol = 0))

xnames5 <- c("SD5_ALT","LA5_ALT","SF5A_ALT","SF5B_ALT")
xnames10 <- c("SD10_ALT","LA10_ALT","SF10_ALT")
xnames20 <- c("SD20_ALT","SF20_ALT")

clnames5 <- c("San Diego","Los Angeles","Palo Alto-San Jose","Dublin-Pleasonton")
clnames10 <- c("San Diego","Los Angeles", "San Francisco")
clnames20 <- c("San Diego","San Francisco")

n=10

ca_base <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ca_base
y <- "CAsub"


#NE Stem
citations <- fread("SAScitationsNEstem.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingNEstem.csv")
clustpatents <- fread("SASclustpatentsNEstem.csv")

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

n=10

ne_stem <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                 "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ne_stem
y <- "NEstem"



#CA Stem
citations <- fread("SAScitationsCAstem.csv")
possiblenclass <- fread("SASpossiblenclass.csv")
originating <- fread("SASoriginatingCAstem.csv")
clustpatents <- fread("SASclustpatentsCAstem.csv")

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

n=10

ca_stem <- list("xnames5" = xnames5, "xnames10" = xnames10, "xnames20" = xnames20,
                "clnames5" = clnames5, "clnames10" = clnames10, "clnames20" = clnames20, "n" = n)

x <- ca_stem
y <- "CAstem"



#--------Call all functions-----------------------
replications <- do.call(randomize,x)
z <- paste0("replications_",y,".csv")
write.csv(replications, z)
columnsEFI <- do.call(colsEFI,x)
z2 <- paste0("columnsEFI_",y)
saveRDS(columnsEFI,file = z2)
originating <- do.call(colA,x)
citations_colB <- do.call(colB,x)
citations_colc <- do.call(colC,x)

Table5m <- do.call(table5m,x)
Table10m <- do.call(table10m,x)  
Table3a <- do.call(table3,x)  

print_tables()
print_one_table()
