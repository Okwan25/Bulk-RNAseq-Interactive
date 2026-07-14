# start up for download packages

# Welcome and check R status
if (Be.Chatty == TRUE){
  cat( 
"Welcome to GENEration!\n
This simplified version of the DEGs analysis\n
pipeline will try to take care of as much as possible.\n
\n"
  )
  Sys.sleep(message.delay.time)
  cat(
"For best results, make sure your R and RStudio are up-to-date.\n
\n"
  )
  
  Sys.sleep(message.delay.time)
  
  # check packages, give warning, install missing packages--------------
  
  cat(
"Now we're going to try to install any of the required packages\n
that you don't already have installed.\n
\n"
  )
  Sys.sleep(message.delay.time)
}

required.packages <- c(
 "tximport", "edgeR", "limma", "Glimma",
 "fgsea", "GSEABase", "GSVA", "EDASeq", "DESeq2", "coda"
)

cran.packages <- c(
  "rhdf5", "readr", "xml2", "data.table", "stringr", "gtools", "ggplot2", 
  "gplots", "RColorBrewer", "tidyr", "gsubfn", "dplyr", "tidyverse", 
  "ggfortify", "ggrepel", "plotly", "forcats"
)

if ( length(setdiff(required.packages, rownames(installed.packages()))) !=0 ){
  cat( "We need to install some packages before we get started.\n
This might take a few minutes.\n
If you're prompted to update, please do so.\n")
  
  install.packages(setdiff(cran.packages, rownames(installed.packages())))
}

if (length(setdiff(required.packages, rownames(installed.packages()))) !=0 ){
  cat("\n")
  cat("Installation appears to have failed for one or more packages.\n
You'll need to get help before proceeding.\n
      \n")
} else {
  cat("\n")
  cat("That's all done. Now, on to the analysis!\n
      \n")
}

stopifnot("Not all packages were installed" = 
            length(setdiff(required.packages, rownames(installed.packages()))) == 0 )
