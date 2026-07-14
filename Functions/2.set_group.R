if (Be.Chatty == TRUE){
  cat(
    "This part of the workflow will automatically detect experimental groups
from your file names.

You will then select which conditions to compare and optionally rename them
for visualization.\n\n")
  
  Sys.sleep(message.delay.time)
}

base_dir <- abundance.dir
sample_id <- dir(base_dir)

cat("\nExample of your filename: ", sample_id[1], "\n")

# -------------------------
# 🔹 1. PARSE SAMPLE NAMES
# -------------------------
parse_samples <- function(samples) {
  split_names <- strsplit(samples, "[-_]")
  
  data.frame(
    sample = samples,
    patient = sapply(split_names, function(x) {
      if (length(x) < 2) return(NA)
      x[length(x)-1]
    }),
    condition = sapply(split_names, function(x) {
      x[length(x)]
    }),
    stringsAsFactors = FALSE
  )
}

meta <- parse_samples(sample_id)

# -------------------------
# 🔹 2. AUTO-DETECT GROUPS
# -------------------------
conditions <- unique(meta$condition)

cat("\nDetected conditions:\n")
print(conditions)

if (length(conditions) < 2) {
  stop("Not enough distinct conditions detected in filenames.")
}

# -------------------------
# 🔹 3. USER SELECTS 2 GROUPS
# -------------------------
idx1 <- menu(conditions, title = "Select FIRST condition")
idx2 <- menu(conditions, title = "Select SECOND condition")

if (idx1 == 0 || idx2 == 0) {
  stop("Selection cancelled.")
}

if (idx1 == idx2) {
  stop("You must select two different conditions.")
}

dge.condition <- conditions[c(idx1, idx2)]

cat("\nSelected comparison: ", dge.condition[1], " vs ", dge.condition[2], "\n")

# -------------------------
# 🔹 4. OPTIONAL RENAMING
# -------------------------
want.to.rename.groups <- menu(
  c("Yes", "No"),
  title = "Do you want to rename the groups for plots?"
)

dge.group.names <- list(
  orig.name = dge.condition,
  new.name  = dge.condition
)

if (want.to.rename.groups == 1) {
  
  new.labels <- character(length(dge.condition))
  
  for (i in seq_along(dge.condition)) {
    new.labels[i] <- readline(
      paste("Enter label for", dge.condition[i], ": ")
    )
  }
  
  dge.condition.label <- new.labels
  names(dge.condition.label) <- dge.condition
  
} else {
  
  dge.condition.label <- dge.condition
  names(dge.condition.label) <- dge.condition
}

cat("\nFinal group labels:\n")
print(dge.condition.label)
cat("\n")

Sys.sleep(message.delay.time)

# -------------------------
# 🔹 5. FILTER DATA
# -------------------------
meta <- meta[meta$condition %in% dge.condition, ]

if (nrow(meta) == 0) {
  stop("No samples found for selected conditions.")
}

# -------------------------
# 🔹 6. ORDER SAMPLES
# -------------------------

# convertir patient en numérique (IMPORTANT)
meta$patient_num <- as.numeric(meta$patient)

# garder ordre des conditions choisi
meta$condition <- factor(meta$condition, levels = dge.condition)

# tri
meta <- meta[order(meta$condition, meta$patient_num), ]

# update
sample_id <- meta$sample
rm(meta)

cat("\nOrdered samples:\n")
print(sample_id)