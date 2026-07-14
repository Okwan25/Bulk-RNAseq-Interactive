
ranks <- rezzy_2$stat
names(ranks) <- rezzy_2$gene
ranks <- sort(ranks, decreasing = T)


geneList <- file.choose()

# Vérification (optionnel)
if (!grepl("\\.gmt$", geneList)) {
  stop("Please choose a .gmt file")
}

# Afficher le chemin choisi
print(basename(geneList))
cat("\n")
pathway <- gmtPathways(geneList)
set.seed(12345)
fgseaRes <- fgsea(pathways=pathway, stats=ranks, nperm=1000)
fgseaRes <- fgseaRes[order(padj, -abs(NES)), ]
plop <- as.data.frame(cbind(fgseaRes$pathway, fgseaRes$pval, fgseaRes$padj, 
                            fgseaRes$ES, fgseaRes$NES))
cols <- c("pathway", "pval", "padj", "ES", "NES")
colnames(plop) <- cols
plop2 = plop[order(plop$padj, plop$NES),]

if (basename(geneList) == "tmod_final.gmt")
{
  LI.subset <- subset(plop2, grepl("^LI\\.", pathway))
  LI.subset <- LI.subset[order(LI.subset$NES, decreasing=T), ]
  LI.subset <- LI.subset[!grepl("TBA", LI.subset$pathway),]
  plop2 <- LI.subset
}
print( head(plop2) )


plop2$NES <- as.numeric(plop2$NES)

fgseaResTidy <- plop2 %>%
  as_tibble() %>%
  arrange(desc(NES)) 

# show top/bot 15st
dt = rbind(head(fgseaResTidy,15), tail(fgseaResTidy,15))
dt$NES <- as.numeric(dt$NES)
dt$padj <- as.numeric(dt$padj)

thr.padj <- readline( "What is the threshold of padj ? ")
thr.padj <- as.numeric(thr.padj)

thr.NES <- readline( "What is the threshold of NES ? ")
thr.NES <- as.numeric(thr.NES)

dt_sign <- dt %>%
  filter(padj <= thr.padj) %>%
  filter(abs(NES) >= thr.NES)

print( plot_barplot(dt_sign) )








