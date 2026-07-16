




max.arg <- max(lengths(strsplit(sample_id, "\\.")))

possible_sep <- c("_", "-", ".", "/", "|")

detected_sep <- possible_sep[sapply(possible_sep, function(sep) {
  any(grepl(sep, sample_id, fixed = TRUE))
})]

if(length(detected_sep) == 0){
  stop("None separator detected in sample name")
}
 
cat("\nSeparators detected :\n")
print(detected_sep)

# Choose the good separator 
if(length(detected_sep) == 1){
  
  sep_choice <- detected_sep
  cat("\nOnly one separator separator detected :", sep_choice, "\n")
  
} else {
  
  sep_choice <- readline(
    paste0(
      "\nWhich separator need to use ? ",
      paste(detected_sep, collapse = ", "),
      " : "
    )
  )
  
  if(!sep_choice %in% detected_sep){
    stop("Invalid separator")
  }
}


# Regex separator expression
sep_regex <- switch(
  sep_choice,
  "." = "\\.",
  "|" = "\\|",
  sep_choice
)


# -------------------------------------------------
# 2. Split the name of sample
# -------------------------------------------------

split_names <- strsplit(sample_id, sep_regex)


# Number of column 
n_fields <- lengths(split_names)

print(n_fields)


# Search problematic samples
max_fields <- max(n_fields)

if(any(n_fields != max_fields)){
  
  bad_samples <- sample_id[n_fields != max_fields]
  
  cat(
    "\nTheses samples are problematic :\n",
    paste("-", bad_samples, collapse = "\n"),
    "\n\n"
  )
  
  cat(
    "What would you like to do? ?\n",
    "1 : Fill in the missing fields with “NA”\n",
    "2 : Delete theses samples\n",
    "3 : Stop the script\n"
  )
  
  choice <- readline("Your choose (1/2/3) : ")
  
  if(choice == "1"){
    
    split_names <- lapply(split_names, function(x){
      length(x) <- max_fields
      x
    })
    
  } else if(choice == "2"){
    
    keep <- n_fields == max_fields
    
    sample_id   <- sample_id[keep]
    countsData  <- countsData[, keep, drop = FALSE]
    split_names <- split_names[keep]
    n_fields    <- n_fields[keep]
    
    cat(sum(!keep), "samples deleted.\n")
    
  } else if(choice == "3"){
    
    stop("Stop the script.")
    
  } else {
    
    stop("Invalid choose.")
    
  }
  
} else {
  
  split_names <- lapply(split_names, function(x){
    length(x) <- max_fields
    x
  })
  
}


# Creation of metadata
annotation_col <- as.data.frame(
  do.call(rbind, split_names),
  stringsAsFactors = FALSE
)


colnames(annotation_col) <- paste0("part_", seq_len(max_fields))


# -------------------------------------------------
# 3. Name columns
# -------------------------------------------------

print(annotation_col)


new_names <- character(ncol(annotation_col))


for(i in seq_len(ncol(annotation_col))){
  
  cat("\nColonne :", colnames(annotation_col)[i], "\n")
  print(unique(annotation_col[,i]))
  
  new_names[i] <- readline(
    "Name of variable (ex: condition, patient, timepoint, sample_id) : "
  )
}


colnames(annotation_col) <- new_names


# -------------------------------------------------
# 4. Final check
# -------------------------------------------------

cat("\nFinal version of the metadata :\n")
print(annotation_col)

rownames(annotation_col) <- sample_id

annotation_col