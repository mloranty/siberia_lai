#set working directory to folder containing all hemi-photo outputs
setwd("/Volumes/data/data_repo/field_data/siberia_lai/hemisfer_outputs/all_sites/")

#list the names of the files in the folder for use in for loop
files <- list.files()

#create a blank dataframe with named columns - for loop will dump rows into this dataframe
all_sites <- data.frame(matrix(nrow = 0,ncol = 26))
colnames(all_sites) <- c("File","Date.Time","Mill","LiCor","Lang","G","NC","T","Mill-S","LiCor-S","Lang-S","G-S","NC-S","T-S","Mill-C","LiCor-C","Lang-C","G-C","NC-C","T-C","Mill-SC","LiCor-SC","Lang-SC","G-SC","NC-SC","T-SC")

#runs each file in the folder through
for (i in files) {
  
  #extracts table of lai values
  d <- read.delim(file = i,                    
                  skip = 30, 
                  nrows = 6, 
                  sep = "\t",header = F, 
                  na.strings = "-")
  
  #extracts file name and date & time
  d2 <- read.delim(file = i,                   
                   skip = 0, 
                   nrows = 2, 
                   sep = "\t",header = F, 
                   na.strings = "-")
  
  #assigns columns to vectors to rearrange
  file.datetime <- d2$V2                       
  no.corr <- d$V2
  S.corr <- d$V4
  C.corr <- d$V6
  SC.corr <- d$V8
  
  #combines vectors to make one long row
  row <- c(file.datetime,no.corr,S.corr,C.corr,SC.corr)
  #inserts row into blank dataframe
  all_sites[nrow(all_sites) + 1,] <- row
}

#converts all LAI values from characters back to numeric
all_sites[,3:26] <- as.numeric(unlist(all_sites[,3:26]))

#writes all results to a csv
setwd("/Volumes/data/data_repo/field_data/siberia_lai/hemisfer_outputs/")
write.csv(all_sites,file = "all_sites_LAI_output.csv")
