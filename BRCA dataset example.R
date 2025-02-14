# Name required packages
CRAN_packages <- c("tidyverse", "cli", "BiocManager", "viridis",
                   "pheatmap", "cowplot", "factoextra", "ggnewscale", "ggpubr", "ggrepel")
Bioc_packages <- c("edgeR", "DESeq2", "EnhancedVolcano", "apeglm", "vsn", "clusterProfiler",
                   "org.Hs.eg.db", "HPO.db", "enrichplot", "GOSemSim", "pathview")

# Install missing packages
installed_CRAN_packages <- CRAN_packages %in% rownames(installed.packages())
installed_Bioc_packages <- Bioc_packages %in% rownames(installed.packages())
current_packages <- c(installed_Bioc_packages, installed_CRAN_packages)

if (any(current_packages == FALSE)) {
  install.packages(CRAN_packages[!installed_CRAN_packages])
  BiocManager::install(Bioc_packages[!installed_Bioc_packages])
}

# Load required packages
invisible(lapply(c(CRAN_packages, Bioc_packages), library, character.only = TRUE))

#Download and explore BRCA dataset
data(geneList, package="DOSE")
str(geneList)

gene.df <- bitr(names(geneList), fromType = "ENTREZID",
                toType = c("ENSEMBL", "SYMBOL"),
                OrgDb = org.Hs.eg.db)

head(gene.df)
#Over Representation Analysis (ORA) ===========================================
UR_genes <- names(geneList)[geneList > 2]
head(UR_genes)
#or to look at the equivalent gene Symbols, which are typically easier to interpret
gene.df$SYMBOL[gene.df$ENTREZID %in%  head(UR_genes)]

#Gene Ontology term ORA
ego <- enrichGO(gene          = UR_genes,
                universe      = names(geneList),
                OrgDb         = org.Hs.eg.db,
                ont           = "BP",
                pAdjustMethod = "bonferroni",
                readable      = TRUE)
names(ego@result)

#GO ORA results Visualization
dotplot(ego, showCategory=10, label_format = 30) +
  ggtitle("GO-BP ORA") + 
  theme(axis.text.y = element_text(size = 7))

ego <- pairwise_termsim(ego)
emapplot(ego, showCategory = 20, cex.params = list(category_label = 0.5, cex_line = 0.1))

cnetplot(ego, categorySize="pvalue",
         color.params = list(foldChange = geneList),
         cex.params = list(gene_label = 0.5, category_label = 0.7),
         layout = "kk")

#KEGG Pathway ORA=======================================================
x <- enrichKEGG(gene=UR_genes, universe = names(geneList),
                organism     = 'hsa',
                pvalueCutoff = 0.05)
head(x@result)

dotplot(x, showCategory=10, label_format = 30) +
  ggtitle("Kegg Pathway ORA") + 
  theme(axis.text.y = element_text(size = 7))

hsa04110 <- pathview(gene.data  = geneList,
                     pathway.id = "hsa04110",
                     species    = "hsa",
                     limit      = list(gene=max(abs(geneList)), cpd=1))

#Comparing Gene lists=======================================================
DR_genes <- names(geneList)[geneList < -2]

ck <- compareCluster(geneCluster = list(UR = UR_genes, DR = DR_genes), fun = "enrichKEGG",
                     organism = "hsa", universe = names(geneList))
ck <- setReadable(ck, OrgDb = org.Hs.eg.db, keyType="ENTREZID")

dotplot(ck)

ck <- pairwise_termsim(ck)
emapplot(ck, showCategory = 10, 
         cex.params = list(category_label = 1, cex_line = 0.2, pie2axis = 0.1),
         pie.params = list(legend_n = 2))

cnetplot(ck, 
         cex.params = list(gene_label = 0.75, group_label = 1),
         pie.params = list(legend_n = 2)) + #legend_loc_x and legend_n edit parameters associated with the node size scatterpie legend.
  theme_void(base_size = 7) 

#GSEA
plot(geneList)
y <- gseGO(geneList, OrgDb = org.Hs.eg.db,
           pvalueCutoff = 0.2,
           pAdjustMethod = "BH", 
           eps = 1e-100,
           verbose = FALSE)
y <- setReadable(y, 'org.Hs.eg.db', 'ENTREZID')

dotplot(y, showCategory=10, label_format = 30) +
  ggtitle("dotplot for ORA") + 
  theme(axis.text.y = element_text(size = 7))
#Plot doesnt utlize GSEA specific statistics, create plot to visualize the NES and significance
y@result %>%
  mutate(label = ifelse(rank(qvalue) <= 5, Description, "")) %>%
  ggplot(aes(x = NES, y = -log10(qvalue), label = label)) +
  geom_point(aes(size = setSize, colour = p.adjust < 0.01)) +
  scale_size(range = c(0.5,2)) +
  scale_color_manual(values = c("grey70", "red")) +
  ggrepel::geom_text_repel(size = 2.5, min.segment.length = 0, force = 10) +
  theme_classic() 
head(y@result)

#Chromosome segregation
gs2p <- 1
gseaplot2(y, geneSetID = gs2p, title = y$Description[gs2p],
          pvalue_table = T)
#Instead of using default p_value, it can be informative to add alternative statistics from the GSEA to the plot
gs2p <- 1
plot <- gseaplot2(y, geneSetID = gs2p,
                  title = paste(y$Description[gs2p]))
plot[[1]] <- plot[[1]] + labs(subtitle = paste0("(NES=", signif(y@result$NES[gs2p],3), 
                                                "; FDR q=", signif(y@result$qvalue[gs2p],3), ")"))
plot

