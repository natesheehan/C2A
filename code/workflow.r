## ---------------------------
##
## Script name: workflow.r
##
## Purpose of script: Run full workflow for paper
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-09-20
##
##
## ---------------------------
##
## Notes: To run this workflow, `build.r` needs to be run prior in order to install packages, load functions and data.
##
##
## ---------------------------

## PUBLICATIONS and Venn Digagram - Figure 1
p1= plot_total_publications(gisaid,ena,ddbj,ncbi)
p2 = plot_average_altmetric(gisaid,ena,ddbj,ncbi)
p3 = plot_average_citation(gisaid,ena,ddbj,ncbi)
p4 = plot_total_citations(gisaid,ena,ddbj,ncbi)
ggarrange(p1,p2,p3,p4, common.legend = TRUE)
ggsave(
  paste0("imgs/publications-plot.png"),
  dpi = 320,
  width = 12,
  height = 12,
  limitsize = FALSE
)
rm(p1,p2,p3,p4)
gc()
x = list(GISAID = gisaid$Publication.ID, ENA = ena$Publication.ID,
         NCBI = ncbi$Publication.ID, DDBJ = ddbj$Publication.ID)
#4D Venn diagram
lx=ggVennDiagram(x) + scale_fill_gradient(low="#F4FAFE",high="seagreen4") + custom_theme_oss_no_axis()
ggsave(
  paste0("imgs/venn-plot.png"),
  dpi = 320,
  width = 12,
  height = 12,
  limitsize = FALSE
)
## ACCESS - Figure 2
plot_access("GISAID","ENA","ddbj","NCBI")
ggsave(
  paste0("imgs/access-plot.png"),
  dpi = 320,
  width = 12,
  height = 12,
  limitsize = FALSE
)
gc()
## PUBLISHERS - Figure 3
plot_publisher("GISAID", "ENA", "ddbj", "NCBI")
ggsave(
  paste0("imgs/publishers-plot.png"),
  dpi = 320,
  width = 12,
  height = 12,
  limitsize = FALSE
)
gc()
# VARIANTS - Figure 5
j9 = plot_variants(ena)
j10 = plot_variants(gisaid)
j11 = plot_variants(ncbi)
j12 = plot_variants(ddbj)
ggarrange(j10,j11,j9,j12)
ggsave(
  paste0("imgs/variants-plot.png"),
  dpi = 320,
  width = 10,
  height = 10,
  limitsize = FALSE
)
rm(j9,j10,j11,j12)
gc()
## KEYWORDS AND THEMATIC MAPPING - Figure 5
a = plot_keyword_graph(gisaid)
b = plot_keyword_graph(ncbi)
c = plot_keyword_graph(ena)
d = plot_keyword_graph(ddbj)
ggarrange(a,b,c,d)
ggsave(
  paste0("imgs/keywords-plot.png"),
  dpi = 320,
  width = 20,
  height = 14,
  limitsize = FALSE
)
rm(a,b,c,d)
gc()
### SCP vs MCP - Figure 6
j = plot_scp_mcp(gisaid)
k = plot_scp_mcp(ncbi)
l = plot_scp_mcp(ddbj)
m = plot_scp_mcp(ena)
ggarrange(j,k,m,l,common.legend = TRUE)
ggsave(
  paste0("imgs/region-colab-plot.png"),
  dpi = 320,
  width = 12,
  height = 12,
  limitsize = FALSE
)
### INCOME COLLABORATOIN MATRIX - table 2
calculate_percentage_collaboration(j)
gc()
### INCOME COLAB - Figure 7

j1 = plot_income_colab(gisaid)
k1 = plot_income_colab(ncbi)
l1 = plot_income_colab(ddbj)
m1 = plot_income_colab(ena)
ggarrange(j1[[2]],k1[[2]],m1[[2]],l1[[2]], common.legend = TRUE)
ggsave(
  paste0("imgs/income-colab-plot.png"),
  dpi = 320,
  width = 12,
  height = 18,
  limitsize = FALSE
)
## Income Stats - sup material 1
head(as.data.frame(m1[1])|>arrange(desc(Articles)),40)
head(as.data.frame(m1[1])|>arrange(desc(-Percent)),40)
gc()
### CCOUNTRY COLLABORATION - Figure 8
ddbjcountry = plot_country_collab(ddbj)
gisaidcountry = plot_country_collab(gisaid)
enacountry = plot_country_collab(ena)
ncbicountry = plot_country_collab(ncbi)
ggarrange(ddbjcountry,gisaidcountry,enacountry,ncbicountry)
gc()
ddbjcm=centrality_measures(ddbj,"Country.of.standardized.research.organization")
ddbjcm$rep = "ddbj"
enacm = centrality_measures(ena, "Country.of.standardized.research.organization")
enacm$rep = "ena"
giscm = centrality_measures(gisaid, "Country.of.standardized.research.organization")
giscm$rep = "gisaid"
ncbicm = centrality_measures(ncbi,"Country.of.standardized.research.organization")
ncbicm$rep = "ncbi"

cm = rbind(ddbjcm,enacm,giscm,ncbicm)
gc()
# NETWORK STATS - Table 3
stats_collab_network(gisaid,"Country.of.standardized.research.organization")
stats_collab_network(gisaid,"Research.Organizations...standardized")
stats_collab_network(gisaid,"Funder.Group")
stats_author_collab_network(gisaid,"Authors")
stats_collab_network(ncbi,"Country.of.standardized.research.organization")
stats_collab_network(ncbi,"Research.Organizations...standardized")
stats_collab_network(ncbi,"Funder.Group")
stats_author_collab_network(ncbi,"Authors")
stats_collab_network(ena,"Country.of.standardized.research.organization")
stats_collab_network(ena,"Research.Organizations...standardized")
stats_collab_network(ena,"Funder.Group")
stats_author_collab_network(ena,"Authors")
stats_collab_network(ddbj,"Country.of.standardized.research.organization")
stats_collab_network(ddbj,"Research.Organizations...standardized")
stats_collab_network(ddbj,"Funder.Group")
stats_collab_network(ddbj,"Authors")
gc()
### FUNDERS - figure 9
i1=plot_collab_network(gisaid,"Funder.Group")
i2=plot_collab_network(ncbi,"Funder.Group")
i3=plot_collab_network(ena,"Funder.Group")
i4=plot_collab_network(ddbj,"Funder.Group")
ggarrange(i1[[1]],i2[[1]],i3[[1]],i4[[1]])
ggsave(
  paste0("imgs/funder-colab-plot.png"),
  dpi = 320,
  width = 22,
  height = 14,
  limitsize = FALSE
)
rm(i1,i2,i3,i4)
ddbjcm=centrality_measures(ddbj,"Funder.Group")
ddbjcm$rep = "ddbj"
enacm = centrality_measures(ena, "Funder.Group")
enacm$rep = "ena"
giscm = centrality_measures(gisaid, "Funder.Group")
giscm$rep = "gisaid"
ncbicm = centrality_measures(ncbi,"Funder.Group")
ncbicm$rep = "ncbi"
cm = rbind(ddbjcm,enacm,giscm,ncbicm)gc()
### INSTITUTION PLOT - Figure 10
o1=plot_big_collab_network(gisaid,"Research.Organizations...standardized")
o2=plot_big_collab_network(ncbi,"Research.Organizations...standardized")
o3=plot_big_collab_network(ena,"Research.Organizations...standardized")
o4=plot_big_collab_network(ddbj,"Research.Organizations...standardized")
ggarrange(o1[[1]],o2[[1]],o3[[1]],o4[[1]])
ggsave(
  paste0("imgs/institutions-colab-plot.png"),
  dpi = 320,
  width = 22,
  height = 22,
  limitsize = FALSE
)
gc()
# COUNT PER PAPER - Table 4
count_papers_by_authors(ddbj)
count_papers_by_authors(ena)
count_papers_by_authors(ncbi)
count_papers_by_authors(gisaid)
gc()
#### AUTHOR NETWORK
x=author_network(gisaid)
xx=author_network(ena)
xxx=author_network(ncbi)
xxxx=author_netwvork(ddbj)
ggarrange(x,xx,xxx,xxxx)
ggsave(
  paste0("imgs/authors-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)
gc()




