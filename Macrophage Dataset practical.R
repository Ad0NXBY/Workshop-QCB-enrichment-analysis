#using the DE analysis results from Session 2 to perform GO Biological Process ORA for M1 and M2 marker genes
#prepare a figure to visualize your results complete with a legend and description/interpretation of the findings
load("Session4_practical(1).Rdata")
gene.df <- bitr(DE.blocked.treat$gene_name,
                fromType = "SYMBOL",
                toType = c("ENTREZID"),
                OrgDb = org.Hs.eg.db)
gene.df$gene_name <- gene.df$SYMBOL

DE.blocked.treat <- left_join(DE.blocked.treat, gene.df, by = "gene_name")

DE.blocked.treat$DE.regulation <-
  ifelse(DE.blocked.treat$logFC > 0 & DE.blocked.treat$FDR < 0.01, "M2",
         ifelse(DE.blocked.treat$logFC < 0 & DE.blocked.treat$FDR < 0.01, "M1", NA))

Volplot <- DE.blocked.treat %>%
  ggplot(aes(x = logFC, y = -log10(FDR), color = DE.regulation)) + 
  geom_point() +
  theme_pubr() +
  theme(legend.title = element_blank(), legend.position = "right", legend.key.size = unit(5, "pt")) + 
  scale_color_discrete()
print(Volplot)
