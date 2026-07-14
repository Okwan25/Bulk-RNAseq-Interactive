if (Be.Chatty == TRUE){
  cat( 
    "Now, we will grep only sample match with your group.\n")
  Sys.sleep(message.delay.time)
}

choisir_elements <- function(id) {
  
  elements <- strsplit(id, "-")[[1]]
  
  for (i in seq_along(elements)) {
    cat(i, ":", elements[i], "\n")
  }
  
  choix_patient <- as.numeric(readline(
    prompt = "Which one is your PATIENT ID: "
  ))
  
  choix_condition <- as.numeric(readline(
    prompt = "Which one is your CONDITION : "
  ))
  
  if (
    !is.na(choix_patient) &&
    !is.na(choix_condition) &&
    choix_patient %in% seq_along(elements) &&
    choix_condition %in% seq_along(elements) &&
    choix_patient == choix_condition
  ) {
    cat("\nInvalid X \n")
    return(NULL)
  }
    
    resultat <- list(
      patient = choix_patient,
      condition = choix_condition
    )
    
    return(resultat)
}

id <- sample_id[1]

result <- choisir_elements(id)

message("Patient :", result$patient, "\n")
message("Condition :", result$condition, "\n")

message("\nLet show your raw data: \n")

Condition1 <- dge.condition[1]
Condition2 <- dge.condition[2]

selection <- sample_id[grepl(paste0("(", Condition1, "|", Condition2, ")$"), sample_id)]

find_incomplete_pairs <- function(samples, Condition1, Condition2) {
  sel <- samples[grepl(paste0("-(", Condition1, "|", Condition2, ")$"), samples, perl = TRUE)]
  if (length(sel) == 0) return(character(0))
  patient <- sapply(strsplit(sel, "-"), function(x) x[length(x)-1])
  suffix  <- sub(".*-", "", sel)  
  present <- split(suffix, patient)
  incomplete_patients <- names(present)[
    !vapply(present, function(x) all(c(Condition1, Condition2) %in% unique(x)), logical(1))
  ]
  sel[patient %in% incomplete_patients]
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
  patient = sapply(strsplit(sample_id, "-"), `[`, 2),
  condition = sapply(strsplit(sample_id, "-"), `[`, 3),
  stringsAsFactors = FALSE
)

metadata <- metadata[order(metadata$condition, metadata$patient), ]
print(table(metadata$condition))

message("\nGo next part\n")


