if (Be.Chatty == TRUE){
  cat(
    "Only if you have your matrix of gene expression in csv format (rows : GeneName, column : SampleName)\n\n")
  
  Sys.sleep(message.delay.time)
}

path.countsData <- file.choose()
countsData <- read.csv(path.countsData, row.names = 1,
                       check.names = T)

sample_id <- colnames(countsData)
print(sample_id)

source("../Functions/1.1.1.check_sample_name.R")
source("../Functions/1.1.2.choose_condition.R")