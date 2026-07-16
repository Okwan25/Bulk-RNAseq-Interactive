if (Be.Chatty == TRUE){
  cat( 
    "Now, we will grep only sample match with your group.\n")
  Sys.sleep(message.delay.time)
}

choose_elements <- function(id) {
  
  elements <- strsplit(id, "-")[[1]]
  
  for (i in seq_along(elements)) {
    cat(i, ":", elements[i], "\n")
  }
  
  choose_ID <- as.numeric(readline(
    prompt = "Which one is your SAMPLE ID: "
  ))
  
  choose_condition <- as.numeric(readline(
    prompt = "Which one is your CONDITION : "
  ))
  
  if (
    !is.na(choose_ID) &&
    !is.na(choose_condition) &&
    choose_ID %in% seq_along(elements) &&
    choose_condition %in% seq_along(elements) &&
    choose_ID == choose_condition
  ) {
    cat("\nInvalid X \n")
    return(NULL)
  }
    
    resultat <- list(
      ID = choose_ID,
      condition = choose_condition
    )
    
    return(resultat)
}

id <- sample_id[1]

result <- choose_elements(id)

message("ID :", result$ID, "\n")
message("Condition :", result$condition, "\n")

message("\nLet show your raw data: \n")

selection <- sample_id[grepl(paste0("(", Condition1, "|", Condition2, ")$"), sample_id)]

find_incomplete_pairs <- function(IDs, Condition1, Condition2) {
  sel <- IDs[grepl(paste0("-(", Condition1, "|", Condition2, ")$"), IDs, perl = TRUE)]
  if (length(sel) == 0) return(character(0))
  ID <- sapply(strsplit(sel, "-"), function(x) x[length(x)-1])
  suffix  <- sub(".*-", "", sel)  
  present <- split(suffix, ID)
  incomplete_ID <- names(present)[
    !vapply(present, function(x) all(c(Condition1, Condition2) %in% unique(x)), logical(1))
  ]
  sel[ID %in% incomplete_ID]
}

if (paired) {
  message("Paired mode : activated")
  message("Number of files : ", length(selection))
  outliers <- find_incomplete_pairs(selection, Condition1, Condition2)
  if (length(outliers) > 0) {
    message("Samples unique find: ", paste(outliers, collapse = ", "))
  } else {
    message("All sample have ", Condition1, " et ", Condition2)
  }
  selection <- selection[!selection %in% outliers]
} else {
  message("Paired mode : deactivated")
}

message("\nLet sort your file ...\n")

metadata <- data.frame(
  filename = sample_id,
  ID = sapply(strsplit(sample_id, "-"), `[`, 2),
  condition = sapply(strsplit(sample_id, "-"), `[`, 3),
  stringsAsFactors = FALSE
)

metadata <- metadata[order(metadata$condition, metadata$ID), ]
print(table(metadata$condition))

message("\nGo next part\n")


