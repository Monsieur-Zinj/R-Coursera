path="~/Documents/R-Coursera/R-Coursera/Data Science: Foundations using R/C2 - R programming/specdata/"
library("eList")

variable<-"nitrate"
ex <- paste(c(1,10,20,320)) # convert to string


pollutantmean <- function(directory, polluant, id=1:332){
  
  # return sum of variables and number of no na
  mean_and_pond <- function(file){
    var_sum <- sum(file[,polluant], na.rm=TRUE)
    no_na_nb <- sum(!is.na(file[,polluant]))
    c(var_sum, no_na_nb)
  }

  id<-nchar(paste(id))
  print(id)
  zeros<-..[for (i in id) paste(rep("0",3-i),collapse = "")] # calculate how much zeros need to be padded before file name 
  file_list <- paste0(directory,zeros,id,".csv") #concatenate path,zeros, number and .csv
  myfiles = lapply(file_list, read.csv) # read all file and put data in a list
  
  mean_and_pond_res<-lapply(myfiles,mean_and_pond)
  mean_and_pond_res<-rowSums(matrix(unlist(mean_and_pond_res),nrow = 2)) # add all sum and no na nb
  mean_res<-mean_and_pond_res[1]/mean_and_pond_res[2]
  mean_res
  
  
}






