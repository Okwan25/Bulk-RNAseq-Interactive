# start packages
cat(
  "\n
  Loading packages...\n
  \n"
)

lapply( required.packages, require, character.only = TRUE )
lapply( cran.packages, library, character.only = TRUE )


# source parameters and functions---------------

if (Be.Chatty == TRUE){
  cat(
"\n
If there are no error messages, then the packages are loaded and source code has been located.\n
Warnings about packages being built under a slight different R version are usually not a problem.\n
Proceed to the next step.\n
\n"
  )
} else {
  cat(
"Move to the next code chunk"
  )
}

