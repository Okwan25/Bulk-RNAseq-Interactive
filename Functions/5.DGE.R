if (Be.Chatty == TRUE){
  cat( 
    "Let to apply the DGE analysis.\n")
  
  Sys.sleep(message.delay.time)
}

patient <- factor(metadata$patient); condition <- factor(metadata$condition)

dge.method <- menu( c("DESEq2", "limmaVoom"),
                           title = "Please choose your DEG approach.")

if (dge.method == 1){
  dds$condition <- relevel(dds$condition, ref = dge.condition[1])
  dds <- DESeq(dds) 
  lenres <- length(resultsNames(dds))
  resname <- resultsNames(dds)[lenres]
  rezzy <- results(dds, name = resname, addMLE = F)
  rezzy_2 <- as.data.frame(rezzy@listData)
  rezzy_2$gene <- rezzy@rownames
  rezzy_2$resname <- resname
  rezzy_2 <- rezzy_2[order(rezzy_2$padj), ]
} else {
  if (paired == TRUE){
    design <- model.matrix(~0+patient+condition)
  } else {
    design <- model.matrix(~0+condition)
  }
  DGE <- DGEList(counts = countsData, group = condition)
  DGE <- calcNormFactors(DGE)
  filtered = DGE$counts[keep,]
  v = voom(filtered, design, plot = T)
  fit = lmFit(v, design)
  fit2 <- eBayes(fit, robust = T)
  res = topTable(fit2, n = Inf, coef=ncol(design), sort.by = "p")
  rezzy_2 = cbind(data.frame(ID = row.names(res), stringsAsFactors = F), res)
}

print( head(rezzy_2) )


