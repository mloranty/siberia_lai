## This script extracts file information from a set of hemispherical photographs

#list files to be read in
files <- file.info(
  list.files(path = "/Volumes/data/projects/siberia_lai/hemi_photos_DG/2014_jpg/",
             pattern = ".JPG$", 
             full.names = TRUE))

#extact the file name from the file path
paths <- data.frame(rownames(files))
colnames(paths) <- c("File.Path")
paths$File.Path <- sub("DAV","DavH",paths$File.Path)
paths$File.Name <- sub(".*//", "", paths$File.Path)

#extract site name
paths$Site <- ifelse(nchar(paths$File.Name) < 10,
                        substr(paths$File.Name,1,3),
                        substr(paths$File.Name,1,4))

#extract picture number
paths$Photo.No <- ifelse(nchar(paths$File.Name) < 10,
                     substr(paths$File.Name,5,5),
                     substr(paths$File.Name,6,6))

#add column to indicate density
paths$Density <- ifelse(grepl("H",paths$Site),paste("HIGH"),
                            ifelse(grepl("M",paths$Site),paste("MED"),
                                   paste("LOW")))

paths$Slope <- ifelse(grepl("S",paths$Site),paste("SLOPED"),paste("FLAT"))

paths <- paths[,c(3,4,2,5,6,1)]
