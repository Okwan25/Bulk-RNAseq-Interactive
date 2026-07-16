if (Be.Chatty == TRUE){
  cat(
    "This part of the workflow will automatically detect experimental groups
from your file names.

You will then select which conditions to compare and optionally rename them
for visualization.\n\n")
  
  Sys.sleep(message.delay.time)
}

base_dir <- rstudioapi::selectDirectory()
sample_id <- dir(base_dir)

cat("\nExample of your filename: ", sample_id[1], "\n")

# -------------------------
# 🔹 1. PARSE SAMPLE NAMES
# -------------------------
parse_samples <- function(samples) {
  split_names <- strsplit(samples, "[-_]")
  
  data.frame(
    sample = samples,
    ID = sapply(split_names, function(x) {
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
# 🔹 3. PAIRED ?
# -------------------------

paired.or.not <- menu(
  c("Yes", "No"),
  title = "Are your samples paired? (That is, for any given sample, it appears in both conditions)"
)

if (paired.or.not == 1) {
  paired = TRUE
} else {
  paired = FALSE
}


# -------------------------
# 🔹 4. USER SELECTS 2 GROUPS
# -------------------------
idx1 <- menu(conditions, title = "Select REFERENCE condition")
idx2 <- menu(conditions, title = "Select TEST condition")

if (idx1 == 0 || idx2 == 0) {
  stop("Selection cancelled.")
}

if (idx1 == idx2) {
  stop("You must select two different conditions.")
}

dge.condition <- conditions[c(idx1, idx2)]

Condition1 <- dge.condition[1]
Condition2 <- dge.condition[2]

cat("\nSelected comparison: ", Condition2, " vs ", Condition1, "\n")

# -------------------------
# 🔹 5. OPTIONAL RENAMING
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
# 🔹 6. FILTER DATA
# -------------------------
meta <- meta[meta$condition %in% dge.condition, ]

if (nrow(meta) == 0) {
  stop("No samples found for selected conditions.")
}

# -------------------------
# 🔹 7. ORDER SAMPLES
# -------------------------

meta$ID_num <- as.numeric(meta$ID)

meta$condition <- factor(meta$condition, levels = dge.condition)

meta <- meta[order(meta$condition, meta$ID_num), ]

sample_id <- meta$sample
rm(meta)

cat("\nOrdered samples:\n")
print(sample_id)