# -------------------------
# AUTO-DETECT GROUPS
# -------------------------

meta <- annotation_col

conditions <- unique(meta$condition)

cat("\nDetected conditions:\n")
print(conditions)

if (length(conditions) < 2) {
  stop("Not enough distinct conditions detected.")
}

# -------------------------
# PAIRED ?
# -------------------------

paired.or.not <- menu(
  c("Yes", "No"),
  title = "Are your samples paired? (same sample in both conditions)"
)

if (paired.or.not == 1) {
  paired <- TRUE
} else {
  paired <- FALSE
}

# -------------------------
# USER SELECTS 2 CONDITIONS
# -------------------------

idx1 <- menu(
  conditions,
  title = "Select REFERENCE condition"
)

idx2 <- menu(
  conditions,
  title = "Select TEST condition"
)

if (idx1 == 0 || idx2 == 0) {
  stop("Selection cancelled.")
}

if (idx1 == idx2) {
  stop("You must select two different conditions.")
}

dge.condition <- conditions[c(idx1, idx2)]

Condition1 <- dge.condition[1]
Condition2 <- dge.condition[2]

cat(
  "\nSelected comparison: ",
  Condition2,
  " vs ",
  Condition1,
  "\n"
)


# -------------------------
# OPTIONAL ADDITIONAL FILTER
# -------------------------

possible.variables <- setdiff(
  colnames(meta),
  "condition"
)

possible.variables <- possible.variables[
  sapply(
    meta[possible.variables],
    function(x) length(unique(x)) > 1
  )
]

if (length(possible.variables) > 0) {
  add.variable <- menu(
    c("Yes", "No"),
    title = "Do you want to filter using another variable (e.g. Day)?"
  )
  
  if (add.variable == 1) {
    selected.variable <- select.list(
      possible.variables,
      multiple = FALSE,
      title = "Select variable to filter"
    )
    
    if (selected.variable != "") {
      selected.levels <- list()
      for (cond in dge.condition) {
        available.levels <- unique(
          meta[
            meta$condition == cond,
            selected.variable
          ]
        )
        selected.levels[[cond]] <- select.list(
          available.levels,
          multiple = FALSE,
          title = paste(
            "Select",
            selected.variable,
            "for condition",
            cond
          )
        )
        if (selected.levels[[cond]] == "") {
          stop("Selection cancelled.")
        }
      }
      
      cat("\nSelected filtering:\n")
      print(selected.levels)
      keep <- rep(FALSE, nrow(meta))

      for (cond in names(selected.levels)) {
        keep <- keep |
          (
            meta$condition == cond &
              meta[[selected.variable]] == selected.levels[[cond]]
          )
      }
      meta <- meta[keep, ]
      cat(
        "\nSamples remaining after filtering:",
        nrow(meta),
        "\n"
      )
    }
  }
}

# -------------------------
# OPTIONAL RENAMING
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
      paste(
        "Enter label for",
        dge.condition[i],
        ": "
      )
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
Sys.sleep(message.delay.time)

# -------------------------
# FINAL FILTER CONDITIONS
# -------------------------

meta <- meta[
  meta$condition %in% dge.condition,
]

if (nrow(meta) == 0) {
  stop("No samples found for selected conditions.")
}

# -------------------------
# ORDER SAMPLES
# -------------------------

meta$ID_num <- as.numeric(meta$ID)

cat("\nFinal metadata used:\n")
print(meta)
