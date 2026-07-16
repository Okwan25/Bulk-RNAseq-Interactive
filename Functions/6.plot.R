#Function plot to see the expression of the top gene 
plot_top_gene_expression <- function(dds, DGE_RESULT, thr.gene){
  gene = head(rezzy_2,1)$gene
  df <- plotCounts(dds,
                   gene=head(DGE_RESULT,thr.gene)$gene,
                   intgroup = "condition",
                   returnData = TRUE)
    p <- ggplot(df, aes(x=condition, y=count, color=condition, fill=condition)) +
    geom_boxplot(alpha=0.4, outlier.shape = NA) +  
    geom_jitter(width=0.1, size=2, alpha=0.8) +   
    theme_classic() +
    scale_color_brewer(palette="Set1") +
    scale_fill_brewer(palette="Set1") +
    scale_y_log10()+
    labs(
      x = "condition",
      y = "log10(norm count)",
      title = paste0("Expression of ", gene)
    )
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
plot_complex_heatmap <- function(heatmap_input, top_genes,
                                 cluster_columns, cluster_rows,
                                 row_names_gp, column_names_gp,
                                 title){
  
  mat <- log2(heatmap_input + 1)[top_genes, ]
  mat_scaled <- t(scale(t(mat)))
  
  annotation_col <- data.frame(
    Condition = colData(dds)$condition
  )
  rownames(annotation_col) <- colnames(mat_scaled)
  ha <- HeatmapAnnotation(
    df = annotation_col,
    col = list(
      Condition = setNames(
        c("#377EB8", "#E41A1C"),
        c(Condition1, Condition2)
      )
    )
  )
  
  plot_heatmap <- Heatmap(
    mat_scaled,
    name = "Z-score",
    top_annotation = ha,
    cluster_columns = cluster_columns,
    cluster_rows = cluster_rows,
    row_names_gp = gpar(fontsize = row_names_gp),
    column_names_gp = gpar(fontsize = column_names_gp),
    show_column_names = TRUE,
    show_row_names = TRUE,
    column_title = title
  )
  return(plot_heatmap)
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
heatmap_input = counts(dds)

heatmap.condition.test <- plot_complex_heatmap(heatmap_input = heatmap_input,
                                               top_genes = top_genes,
                                               cluster_columns = FALSE,
                                               cluster_rows = FALSE,
                                               row_names_gp = 5.5,
                                               column_names_gp = 4.5,
                                               title = "Top on TEST condition")


rezzy_Condition1 <- rezzy_2 %>% arrange(log2FoldChange)
top_genes <- head(rezzy_Condition1$gene, 50)

heatmap.condition.ref <- plot_complex_heatmap(heatmap_input = heatmap_input,
                                              top_genes = top_genes,
                                              cluster_columns = FALSE,
                                              cluster_rows = FALSE,
                                              row_names_gp = 5.5,
                                              column_names_gp = 4.5,
                                              title = "Top on REFERENCE condition")


print(heatmap.condition.test)
print(heatmap.condition.ref)
##

##
thr.pvalue <- readline( "(Volcano-plot) What is the threshold of pval ? (ex: 0.05): ")
while (is.na(as.numeric(thr.pvalue))) {
  thr.pvalue <- readline("Invalid value. Please enter a numeric p-value (ex: 0.05): ")
}
thr.pvalue <- as.numeric(thr.pvalue)

thr.FC <- readline( "(Volcano-plot) What is the threshold of FC ? (ex: 1.5): ")
while (is.na(as.numeric(thr.FC))) {
  thr.FC <- readline("Invalid value. Please enter a numeric p-value (ex: 1.5): ")
}
thr.FC <- as.numeric(thr.FC)

volcano <- plot_volcano(df = rezzy_2, thr.logFC = log10(thr.FC), thr.pvalue = thr.pvalue)

plot <- volcano$plot
print(plot)

df <- volcano$data
up = df$delabel[which(df$diffexpressed=="UP")]
down = df$delabel[which(df$diffexpressed=="DOWN")]
print( table(df$diffexpressed) )
##



