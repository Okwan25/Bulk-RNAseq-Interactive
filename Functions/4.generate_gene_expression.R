if (Be.Chatty == TRUE){
  cat( 
    "Let to generate gene expresssion from kallisto output.\n")
  
  Sys.sleep(message.delay.time)
}

files <- file.path(base_dir, sample_id, "abundance.tsv")
names(files) <- paste0(sample_id)
tx2gene <- read.delim("./GeneSet/ENST_HGNC_protein_coding_noUnattributed.txt")
txi <- tximport(files, type = "kallisto", tx2gene = tx2gene, ignoreTxVersion = T)
countsData <- txi$counts


metadata$patient <- factor(metadata$patient); metadata$condition <- factor(metadata$condition)

if (paired == TRUE){
  design = ~ patient+condition
} else {
  design = ~ condition
}
  
ddsTxi <- DESeqDataSetFromTximport(txi = txi,
                                     colData = metadata,
                                     design = design)

threshold.gene.expression <- readline( "What is the threshold of gene expression (ex: 32)? ")
threshold.gene.expression <- as.numeric(threshold.gene.expression)

threshold.sample <- readline( "This threshold in how many sample (ex: 2)? ")
threshold.sample <- as.numeric(threshold.sample)



keep <- rowSums(counts(ddsTxi) >= threshold.gene.expression) >= threshold.sample
print(table(keep))
dds <- ddsTxi[keep,]

par(mfrow=c(1,2))
nsamples <- ncol(countsData)
col <- brewer.pal(nsamples, "Paired")
raw = cpm(countsData, log=T)
plot(density(raw[,1]), col=col[1] ,lwd=2, ylim=c(0,0.26), las=2, main="", xlab="")
for (i in 2:nsamples){
  den <- density(raw[,i])
  lines(den$x, den$y, col=col[i], lwd=2)
}
title(main="A. Raw data", xlab="Log-cpm")

filtered = cpm(counts(dds), log=T)
plot(density(filtered[,1]), col=col[1], lwd=2, ylim=c(0,0.26), las=2, main="", xlab="")
for (i in 2:nsamples){
  den <- density(filtered[,i])
  lines(den$x, den$y, col=col[i], lwd=2)
}
title(main="B. Filtered data", xlab="Log-cpm")


par(mfrow=c(1,1))
dds <- estimateSizeFactors(dds)
norm_counts <- counts(dds, normalized=TRUE)
seqSet <- newSeqExpressionSet(as.matrix(norm_counts),
                              phenoData = AnnotatedDataFrame(as.data.frame(colData(dds))))
plotRLE(seqSet, outline = FALSE, col = as.numeric(seqSet$condition), xaxt = "n")
abline(h = 0.6, col = "blue", lty = 2, lwd = 2)







