## Download and unzip the data file.
if (!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/projectdata.zip")
unzip("./data/projectdata.zip", exdir = "./data")
