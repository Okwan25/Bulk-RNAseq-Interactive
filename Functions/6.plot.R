#Function plot to see the expression of the top gene 
plot_top_gene_expression <- function(dds, DGE_RESULT, thr.gene){
  df <- plotCounts(dds,
                   gene=head(DGE_RESULT,thr.gene)$gene,
                   intgroup = "condition",
                   returnData = TRUE)
  
  # transformation log2 (avec pseudo-count pour éviter log(0))
  df$log_count <- log2(df$count + 1)
  
  p <- ggplot(df, aes(x = condition, y = log_count)) +
    geom_point(position = position_jitter(width = 0.1)) +
    theme_minimal() +
    ylab("log2(normalized counts + 1)")
  print(p)
}

#Function plot histogramm of FC and pval
plot_histogram <- function(DGE_RESULT, parameter, #log2FoldChange or pvalue 
                           thr.breaks){ 
  hist(DGE_RESULT[[parameter]], 
       breaks=thr.breaks, col="seagreen", xlab=parameter,
       main="Distribution of differential expression values")
  abline(v=c(-1,1), col="black", lwd=2, lty=2)
}

#Function plot heatmap
plot_heatmap <- function(top_genes, scale, #"row" or "col"
                         thr.fontsize_row, thr.fontsize_col,
                         bool.cluster_cols, bool.cluster_rows){
  pheatmap::pheatmap(log2(counts(dds) + 1)[top_genes,],
                     scale=scale,
                     fontsize_row = thr.fontsize_row, 
                     fontsize_col = thr.fontsize_col,
                     cluster_cols = bool.cluster_cols, 
                     cluster_rows = bool.cluster_rows,
                     main = "Heatmap of gene expression")
}

#Function plot volcano-plot
plot_volcano <- function(df, thr.logFC, thr.pvalue){
  df$diffexpress <- "NO"
  df$diffexpressed <- "NO"
  df$diffexpressed[df$log2FoldChange  > thr.logFC & df$pvalue < thr.pvalue] <- "UP"
  df$diffexpressed[df$log2FoldChange  < -thr.logFC & df$pvalue < thr.pvalue] <- "DOWN"
  
  rownames(df) <- df$ID
  df$delabel <- NA
  df$gene_symbol <- df$gene
  df$delabel[df$diffexpressed != "NO"] <- df$gene_symbol[df$diffexpressed != "NO"]
  
  g <- ggplot(data=df, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
    geom_point(size = 1) + 
    theme_minimal() +
    geom_text_repel(size = 2) +
    scale_color_manual(values=c("blue", "grey", "red")) +
    geom_vline(xintercept=c(-thr.logFC, thr.logFC), col="red") +
    geom_hline(yintercept=-log10(thr.pvalue), col="red") + 
    theme( legend.text = element_text(size = 6.5),
           legend.title = element_text(size = 7)) 
    #xlim(-3.5,3.5) + 
    #ylim(0, 25)   
  
  return(list(plot = g, data = df))
}

#Function plot barplot
plot_barplot <- function(dt_sign){
  ggplot(data = dt_sign, 
         aes(x = fct_reorder(pathway,NES), y = NES ,fill = padj)) +
    geom_col()+
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      fill = "padj",
      x = "Pathways",
      y = "NES"
    ) +
    theme_minimal()
}

##
plot_top_gene_expression(dds = dds, DGE_RESULT = rezzy_2, 1)
##

plot_histogram(DGE_RESULT = rezzy_2, "log2FoldChange", thr.breaks = 50)
plot_histogram(DGE_RESULT = rezzy_2, "pvalue", thr.breaks = 50)
##

##
rezzy_Condition2 <- rezzy_2 %>% arrange(desc(log2FoldChange))
top_genes <- head(rezzy_Condition2$gene, 50)
plot_heatmap(top_genes = top_genes, scale = "row",
             thr.fontsize_row = 5.5,
             thr.fontsize_col = 4,
             bool.cluster_cols = F,
             bool.cluster_rows = F)

rezzy_Condition1 <- rezzy_2 %>% arrange(log2FoldChange)
top_genes <- head(rezzy_Condition1$gene, 50)
plot_heatmap(top_genes = top_genes, scale = "row",
             thr.fontsize_row = 5.5,
             thr.fontsize_col = 4,
             bool.cluster_cols = F,
             bool.cluster_rows = F)
##

##
thr.pvalue <- readline( "(Volcano-plot) What is the threshold of pval ? ")
thr.pvalue <- as.numeric(thr.pvalue)

thr.FC <- readline( "(Volcano-plot) What is the threshold of FC ? ")
thr.FC <- as.numeric(thr.FC)

volcano <- plot_volcano(df = rezzy_2, thr.logFC = log10(thr.FC), thr.pvalue = thr.pvalue)

plot <- volcano$plot
print(plot)

df <- volcano$data
up = df$delabel[which(df$diffexpressed=="UP")]
down = df$delabel[which(df$diffexpressed=="DOWN")]
print( table(df$diffexpressed) )
##



