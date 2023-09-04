# packages
package_list = c(
  "ggplot2", # use for paths creation
  "dplyr",
  "janitor", # useful functions for cleaning imported data
  "tidyr", # creating edges
  "igraph", # for creating networks
  "ggraph", # plotting networks
  "stringr",
  "tm"
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}
round_df = function(df, digits) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] = round(df[, nums], digits = digits)

  (df)
}
indicies = function(g) {
  res = matrix(0, igraph::vcount(g), 5)
  res[, 1] = V(g)$name
  res[, 2] = as.numeric(igraph::degree(g))
  res[, 3] = as.numeric(igraph::betweenness(g))
  res[, 4] = as.numeric(igraph::closeness(g))
  res[, 5] = as.numeric(igraph::eigen_centrality(g)$vector)
  res = as.data.frame(res)
  res[, c(2:5)] = sapply(res[, c(2:5)], as.numeric)
  res = round_df(res, 3)
  colnames(res) = c("name",
                    "degree",
                    "betweenness",
                    "closeness",
                    "eigen_centrality")
  return(res)
}
library(lubridate)
library(countrycode)
library(dplyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(countrycode)
library(purrr)
library(cowplot)
library(ggrepel)
library(ggpubr)
# data
ddjb = read.csv("../BIB DATA/DDJB/CSV/Dimensions-Publication-2023-08-23_13-20-29.csv",
                skip = 1)#
gisaid = read.csv("../BIB DATA/GISAID/CSV/Dimensions-Publication-2023-08-23_13-20-00.csv",
                skip = 1)
ncbi = read.csv("../BIB DATA/NCBI/CSV/Dimensions-Publication-2023-08-23_13-20-37.csv",
                  skip = 1)
ena = read.csv("../BIB DATA/ENA/CSV/Dimensions-Publication-2023-08-23_13-20-19.csv",
               skip = 1)


ddjb.refs = read.csv("../BIB DATA/ddjb/bib CSV/Dimensions-Publication-2023-08-24_13-58-27.csv",
                     skip = 1)

class = read.csv("../CLASS.csv")

## REPORTS
generate_report = function(df) {
  # MAIN INFORMATION
  timespan = range(df$PubYear, na.rm = TRUE)
  sources = length(unique(df$Source.title))
  documents = nrow(df)
  annual_growth_rate =
    (documents / nrow(df[df$PubYear == min(df$PubYear, na.rm = TRUE),])) ^ (1 / (max(df$PubYear, na.rm = TRUE) - min(df$PubYear, na.rm = TRUE))) - 1
  doc_avg_age = mean(df$PubYear, na.rm = TRUE)
  avg_citations_per_doc = mean(df$Times.cited, na.rm = TRUE)
  avg_citations_per_year_per_doc =
    avg_citations_per_doc / (max(df$PubYear, na.rm = TRUE) - min(df$PubYear, na.rm = TRUE))

  # Assuming you have a column named 'References' that indicates the count of references per document
  total_references = sum(df$References, na.rm = TRUE)

  # DOCUMENT TYPES
  document_types =
    table(unlist(strsplit(
      as.character(df$Publication.Type), split = ";"
    )))

  # DOCUMENT CONTENTS - Assuming you have columns named KeywordsPlus and AuthorKeywords
  keywords_plus =
    length(unique(unlist(strsplit(
      as.character(df$KeywordsPlus), split = ";"
    ))))
  authors_keywords =
    length(unique(unlist(strsplit(
      as.character(df$AuthorKeywords), split = ";"
    ))))

  # AUTHORS
  authors =
    unique(unlist(strsplit(as.character(df$Authors), split = ";")))
  num_authors = length(authors)
  author_appearances =
    sum(table(unlist(strsplit(
      as.character(df$Authors), split = ";"
    ))))
  single_authored_docs =
    sum(table(unlist(strsplit(
      as.character(df$Authors), split = ";"
    ))) == 1)

  # AUTHORS COLLABORATION
  doc_per_author = documents / num_authors
  co_authors_per_doc = author_appearances / documents
  international_collab =
    100 * sum(table(unlist(strsplit(
      as.character(df$Country.of.standardized.research.organization),
      split = ";"
    ))) > 1) / documents

  # ANNUAL SCIENTIFIC PRODUCTION
  annual_production = table(df$PubYear)

  # MOST PRODUCTIVE AUTHORS
  author_table =
    sort(table(unlist(strsplit(
      as.character(df$Authors), split = ";"
    ))), decreasing = TRUE)

  # TOP MANUSCRIPTS PER CITATIONS
  df_sorted_citations =
    df[order(-df$Times.cited), c("Title", "DOI", "Times.cited")]

  # CORRESPONDING AUTHOR's COUNTRIES - Simplified to just count
  country_table =
    sort(table(unlist(strsplit(
      as.character(df$Country.of.standardized.research.organization),
      split = ";"
    ))), decreasing = TRUE)

  # TODO: You'll need to add more parts to this function to cover every piece in your template

  # PRINT RESULTS
  cat(
    paste(
      "MAIN INFORMATION ABOUT DATA\n\n",
      "Timespan:",
      timespan[1],
      ":",
      timespan[2],
      "\n",
      "Sources (Journals, Books, etc):",
      sources,
      "\n",
      "Documents:",
      documents,
      "\n",
      "Annual Growth Rate %:",
      round(annual_growth_rate * 100, 2),
      "\n",
      "Document Average Age:",
      round(doc_avg_age, 0),
      "\n",
      "Average citations per doc:",
      round(avg_citations_per_doc, 2),
      "\n",
      "Average citations per year per doc:",
      round(avg_citations_per_year_per_doc, 2),
      "\n",
      "References:",
      total_references,
      "\n\n",
      "DOCUMENT TYPES\n"
    )

  )

  print(document_types)

  # ... Add more print statements for the rest of the results
}
generate_report(gisaid)
generate_report(ncbi)
generate_report(ena)
generate_report(ddjb)

## PUBLICATIONS
plot_total_publications = function(...) {

  # List of dataframes
  dfs = list(...)
  names = sapply(as.list(match.call())[-1], deparse)

  # Process each dataframe separately and bind them together
  total_publications_all = lapply(seq_along(dfs), function(i) {
    df = dfs[[i]]

    total_publications_per_month = df |>
      mutate(MonthYear = floor_date(ymd(Publication.date), unit = "month")) |>
      group_by(MonthYear) |>
      summarise(total_publications = n()) |>
      mutate(source = names[i])

    total_publications_per_month
  }) |> bind_rows()

  # Plot
  ggplot(total_publications_all, aes(x = MonthYear, y = total_publications, color = source)) +
    geom_line() +
    labs(title = "Total Publications", x = "Month-Year", y = "Total Publications") +
    custom_theme_oss() +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +   scale_color_discrete(labels = function(x) tools::toTitleCase(toupper(x))) +  # Making labels uppercase
    coord_cartesian(xlim = c(min(total_publications_all$MonthYear, na.rm = TRUE), as.Date("2023-01-31")), ylim = c(0, 600))
}
plot_total_citations = function(...) {

  # List of dataframes
  dfs = list(...)
  names = sapply(as.list(match.call())[-1], deparse)

  # Process each dataframe separately and bind them together
  total_citations_all = lapply(seq_along(dfs), function(i) {
    df = dfs[[i]]

    total_citations_per_month = df |>
      mutate(MonthYear = floor_date(ymd(Publication.date), unit = "month")) |>
      group_by(MonthYear) |>
      summarise(total_citations = sum(Times.cited, na.rm = TRUE)) |>
      mutate(source = names[i])

    total_citations_per_month
  }) |> bind_rows()

  # Plot
  ggplot(total_citations_all, aes(x = MonthYear, y = total_citations, color = source)) +
    geom_line() +
    labs(title = "Total Citations", x = "Month-Year", y = "Total Citations") +
    custom_theme_oss() +   scale_color_discrete(labels = function(x) tools::toTitleCase(toupper(x)))+
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
    coord_cartesian(xlim = c(min(total_citations_all$MonthYear, na.rm = TRUE), as.Date("2023-01-31")))
}
plot_average_citation = function(...) {

  # List of dataframes
  dfs = list(...)
  names = sapply(as.list(match.call())[-1], deparse)

  # Process each dataframe separately and bind them together
  avg_citations_all = lapply(seq_along(dfs), function(i) {
    df = dfs[[i]]

    avg_citations_per_month = df |>
      mutate(MonthYear = floor_date(ymd(Publication.date), unit = "month")) |>
      group_by(MonthYear) |>
      summarise(avg_citations = mean(Times.cited, na.rm = TRUE)) |>
      mutate(source = names[i])

    avg_citations_per_month
  }) |> bind_rows()

  # Plot
  ggplot(avg_citations_all, aes(x = MonthYear, y = avg_citations, color = source)) +
    geom_line() +
    labs(title = "Average Article Citations", x = "Month-Year", y = "Average Citations") +
    custom_theme_oss() +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +   scale_color_discrete(labels = function(x) tools::toTitleCase(toupper(x))) +
    coord_cartesian(xlim = c(min(avg_citations_all$MonthYear, na.rm = TRUE), as.Date("2023-01-31")))
}
plot_average_altmetric = function(...) {

  # List of dataframes
  dfs = list(...)
  names = sapply(as.list(match.call())[-1], deparse)

  # Process each dataframe separately and bind them together
  avg_altmetrics_all = lapply(seq_along(dfs), function(i) {
    df = dfs[[i]]

    avg_altmetrics_per_month = df %>%
      mutate(MonthYear = floor_date(ymd(Publication.date), unit = "month")) %>%
      group_by(MonthYear) %>%
      summarise(avg_altmetrics = mean(Altmetric, na.rm = TRUE)) %>%  # Replace `Times.cited` with `Altmetric_Score`
      mutate(source = names[i])

    avg_altmetrics_per_month
  }) %>% bind_rows()

  # Plot
  ggplot(avg_altmetrics_all, aes(x = MonthYear, y = avg_altmetrics, color = source)) +
    geom_line() +
    labs(title = "Average Altmetric Score", x = "Month-Year", y = "Average Altmetric Score") +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") + custom_theme_oss() +   scale_color_discrete(labels = function(x) tools::toTitleCase(toupper(x)))+
    coord_cartesian(xlim = c(min(avg_altmetrics_all$MonthYear, na.rm = TRUE), as.Date("2023-01-31")))  # Assuming you have a function custom_theme_oss defined
}

p1= plot_total_publications(gisaid,ena,ddjb,ncbi)
p2 = plot_average_altmetric(gisaid,ena,ddjb,ncbi)
p3 = plot_average_citation(gisaid,ena,ddjb,ncbi)
p4 = plot_total_citations(gisaid,ena,ddjb,ncbi)
ggarrange(p1,p3,p2,p4)

ggsave(
  paste0("imgs/publikcations-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

ACCESS##
plot_access = function(name1,name2,name3,name4){
  get_oa = function(data,name){
    oa = as.data.frame(table(data$Open.Access)) |> dplyr::rename("Access Type" = Var1)
    oa$DI = name
    return(oa)
  }
  oa_data = rbind(
    get_oa(gisaid, name1),
    get_oa(ena, name2),
    get_oa(ddjb,name3),
    get_oa(ncbi, name4))

  ggplot(oa_data,
         aes(
           area = Freq,
           fill = Freq,
           label = `Access Type`,
           subgroup = DI
         )) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 5) +
    geom_treemap_subgroup_text(
      place = "centre",
      grow = TRUE,
      alpha = 0.8,
      colour = "black",
      fontface = "italic"
    ) +
    geom_treemap_text(
      colour = "white",
      place = "centre",
      size = 15,
      grow = TRUE
    ) + ggtitle("Publication Access Types") + theme_pubclean() + scale_fill_continuous(type = "gradient", guide = guide_legend()) +
    theme(legend.position = "bottom") + custom_theme_oss() +
    scale_fill_viridis_c()
}
plot_access("GISAID","ENA","DDJB","NCBI")
ggsave(
  paste0("imgs/access-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

## PUBLISHERS
plot_publisher = function(name1, name2, name3, name4) {
  get_publisher = function(data, name) {
    publisher_data = as.data.frame(table(data$Publisher)) %>% dplyr::rename("Publisher" = Var1)
    publisher_data$DI = name
    return(publisher_data)
  }

  publisher_data = rbind(
    get_publisher(gisaid, name1),
    get_publisher(ena, name2),
    get_publisher(ddjb, name3),
    get_publisher(ncbi, name4)
  )

  ggplot(publisher_data,
         aes(
           area = Freq,
           fill = Freq,
           label = Publisher,
           subgroup = DI
         )) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 5) +
    geom_treemap_subgroup_text(
      place = "centre",
      grow = TRUE,
      alpha = 0.8,
      colour = "black",
      fontface = "italic"
    ) +
    geom_treemap_text(
      colour = "white",
      place = "centre",
      size = 15,
      grow = TRUE
    ) +
    ggtitle("Publication Publishers") +
    theme_pubclean() +
    scale_fill_continuous(type = "gradient", guide = guide_legend()) +
    theme(legend.position = "bottom") +
    custom_theme_oss() +
    scale_fill_viridis_c()
}
plot_publisher("GISAID", "ENA", "DDJB", "NCBI")
ggsave(
  paste0("imgs/publishers-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

## KEYWORDS AND THEMATIC MAPPING
plot_keyword_graph = function(data) {
  ## Capture the name of the data
  data_name = deparse(substitute(data))

  ## KEYWORDS NETWORK
  keywords_split = data |>
    separate_rows(MeSH.terms, sep = "[;,]") |>
    mutate(MeSH.terms = str_trim(MeSH.terms)) |>
    filter(MeSH.terms != "")

  keyword_freq = table(keywords_split$MeSH.terms)

  # Filter to get top 40 keywords
  top_keywords = names(sort(keyword_freq, decreasing = TRUE)[1:13])
  keywords_filtered = keywords_split |> filter(MeSH.terms %in% top_keywords)

  # Counting co-occurrences
  cooccurrence_matrix = crossprod(table(keywords_filtered$Publication.ID, keywords_filtered$MeSH.terms))
  diag(cooccurrence_matrix) = 0

  # Filter edges with minimum weight for clarity
  min_weight = 5
  cooccurrence_matrix[cooccurrence_matrix < min_weight] = 0

  # Create the graph object
  graph = graph_from_adjacency_matrix(cooccurrence_matrix, weighted = TRUE, mode = "undirected")
  graph = simplify(graph)  # Removes self-loops

  # Graph plotting
  set.seed(123)
  ggraph(graph, layout = 'circle') +
    geom_edge_arc0(aes(color = weight, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
    scale_edge_width_continuous(range = c(0.1, 2)) +
    scale_edge_colour_identity() +
    geom_node_point(aes(size = degree(graph), color = betweenness(graph))) +
    geom_node_text(aes(label = name), vjust = 0, hjust = 0.3, check_overlap = TRUE, size = 5, color = "red", fontface = "bold", family = "Times New Roman") +
    scale_color_viridis_c() +
    scale_size_continuous(range = c(0.3, 16), name = "degree") +
    theme(
      plot.background = element_rect(fill = "black", color = "black"),
      panel.background = element_rect(fill = "black", color = "black"),
      legend.background = element_rect(fill = "grey42"),
      legend.text = element_text(color = "white"),  # Changed text color to white
      legend.title = element_text(color = "white"),  # Changed title color to white
      plot.title = element_text(color = "white", size = 20),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    ) +
    custom_theme_oss_no_axis() +
    labs(title = paste(stringr::str_to_upper(data_name))) +
    guides(size = guide_legend(override.aes = list(color = "white")))  # Changed 'fill' to 'color' to match the aesthetic used in geom_node_point
}
custom_theme_oss_no_axis = function() {
  theme_minimal(base_family = "Times") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color = "orange", size = 20),
      plot.subtitle = element_text(color = "orange", size = 14),
      plot.caption = element_text(color = "orange"),
      axis.title.x = element_blank(),  # Remove axis title
      axis.title.y = element_blank(),  # Remove axis title
      axis.text.x = element_blank(),  # Remove axis text
      axis.text.y = element_blank(),  # Remove axis text
      axis.ticks = element_line(color = "aquamarine2"),
      axis.line = element_line(color = "aquamarine2"),
      panel.background = element_rect(fill = "black"),
      panel.grid.major = element_line(color = "black"),
      panel.grid.minor = element_line(color = "black"),
      legend.background = element_rect(fill = "transparent"),  # Change legend background to white
      legend.text = element_text(color = "aquamarine2"),
      legend.title = element_text(color = "orange", size = 12)

    )
}
custom_theme_oss = function() {
  theme_minimal(base_family = "Times") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color = "orange", size = 20),
      plot.subtitle = element_text(color = "orange", size = 14),
      plot.caption = element_text(color = "orange"),
      axis.title.x = element_text(color = "aquamarine2"),
      axis.title.y = element_text(color = "aquamarine2"),
      axis.text.x = element_text(color = "aquamarine2"),
      axis.text.y =element_text(color = "aquamarine2"),
      axis.ticks = element_line(color = "aquamarine2"),
      axis.line = element_line(color = "aquamarine2"),
      panel.background = element_rect(fill = "black"),
      panel.grid.major = element_line(color = "black"),
      panel.grid.minor = element_line(color = "black"),
      legend.background = element_rect(fill = "transparent"),  # Change legend background to white
      legend.text = element_text(color = "aquamarine2"),
      legend.title = element_text(color = "orange", size = 12)

    )
}
a = plot_keyword_graph(gisaid)
b = plot_keyword_graph(ncbi)
c = plot_keyword_graph(ena)
d = plot_keyword_graph(ddjb)
ggarrange(a,b,c,d)
ggsave(
  paste0("imgs/keywords-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

plot_variants <- function(data) {
  # Define the WHO labels and Pango Lineages
  who_labels <- c("alpha", "beta", "gamma", "delta", "epsilon", "eta", "iota", "kappa", "omicron", "zeta", "mu")
  pango_lineages <- c("b.1.1.7", "b.1.351", "p.1", "b.1.617.2", "b.1.427", "b.1.429", "b.1.525", "b.1.526", "b.1.617.1", "b.1.617.3", "b.1.1.529", "p.2", "b.1.621", "b.1.621.1")
  data_name = deparse(substitute(data))
  # Combine the labels and lineages into a single vector
  all_variants <- c(who_labels, pango_lineages)

  # Create an empty dataframe to hold the counts
  variant_counts <- data.frame(word = character(), n = integer())

  # Convert the Abstracts to lowercase
  data$Abstract <- tolower(data$Abstract)

  # Loop through all variants to count occurrences in each abstract
  for (variant in all_variants) {
    temp_df <- data %>%
      filter(str_detect(Abstract, regex(paste0("\\b", variant, "\\b"), ignore_case = TRUE))) %>%
      summarise(n = n())

    if (nrow(temp_df) > 0 && temp_df$n > 0) {
      variant_counts <- bind_rows(variant_counts, data.frame(word = variant, n = temp_df$n))
    }
  }

  # Filter out zero counts
  variant_counts <- variant_counts %>% filter(n > 0)

  # Create the plot
  ggplot(variant_counts, aes(x = reorder(word, -n), y = n)) +
    geom_col(fill = "orange") +
    xlab("Variant / Lineage") +
    ylab("Number of Mentions") +
    ggtitle(paste(stringr::str_to_upper(data_name))) +
    coord_flip() + custom_theme_oss()  # Assuming you have a function custom_theme_oss defined
}

j9 = plot_variants(ena)
j10 = plot_variants(gisaid)
j11 = plot_variants(ncbi)
j12 = plot_variants(ddjb)

ggarrange(j10,j11,j9,j12)
ggsave(
  paste0("imgs/variants-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)


### SCP vs MCP
plot_scp_mcp = function(data){

  # Calculate collaboration based on Region
  data_processed = data %>%
    mutate(Country = str_split(Country.of.standardized.research.organization, ";")) %>%
    unnest(Country) %>%
    mutate(Country = str_trim(Country)) %>%
    filter(Country != "", !is.na(Country)) %>%  # Remove empty or NA countries
    inner_join(class, by = c("Country" = "Economy")) %>%
    group_by(Country.of.standardized.research.organization) %>%
    mutate(collaboration = if_else(length(unique(Region)) > 1, "Multi-region", "Single-region")) %>%
    ungroup() %>%
    group_by(Country, collaboration) %>%
    summarize(Articles = n(), .groups = "drop")

  # Get top 20 countries
  top20 = data_processed %>%
    group_by(Country) %>%
    summarize(Total = sum(Articles)) %>%
    top_n(20, Total)

  data_final = filter(data_processed, Country %in% top20$Country)

  ggplot(data_final, aes(fill = collaboration, y = Articles, x = reorder(Country, -Articles, sum))) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = stringr::str_to_upper(deparse(substitute(data)))) +
    xlab("Country") +
    ylab("No. Documents") +
    coord_flip()  +
    theme(axis.text  = element_text(colour = "black", size = 17, face = "bold"),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          legend.position = "none") +  # Hide the legend
    custom_theme_oss()
}
j = plot_scp_mcp(gisaid)
k = plot_scp_mcp(ncbi)
l = plot_scp_mcp(ddjb)
m = plot_scp_mcp(ena)
ggarrange(j,k,m,l,common.legend = TRUE)
ggsave(
  paste0("imgs/region-colab-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

plot_income_colab = function(data){

  # Calculate collaboration based on Income.group
  data_processed = data %>%
    mutate(Country = str_split(Country.of.standardized.research.organization, ";")) %>%
    unnest(Country) %>%
    mutate(Country = str_trim(Country)) %>%
    filter(Country != "", !is.na(Country)) %>%  # Remove empty or NA countries
    inner_join(class, by = c("Country" = "Economy")) %>%
    group_by(Country.of.standardized.research.organization) %>%
    mutate(collaboration = paste(sort(unique(Income.group)), collapse = "-")) %>%
    ungroup() %>%
    group_by(Country, collaboration) %>%
    summarize(Articles = n(), .groups = "drop")

  # Get top 20 countries
  top20 = data_processed %>%
    group_by(Country) %>%
    summarize(Total = sum(Articles)) %>%
    top_n(20, Total)

  data_final = filter(data_processed, Country %in% top20$Country)

  ggplot(data_final, aes(fill = collaboration, y = Articles, x = reorder(Country, -Articles, sum))) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = stringr::str_to_upper(deparse(substitute(data)))) +
    xlab("Country") +
    ylab("No. Documents") +
    coord_flip()  +
    theme(axis.text  = element_text(colour = "black", size = 17, face = "bold"),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          legend.position = "none") +  # Hide the legend
    custom_theme_oss()
}
j1 = plot_income_colab(gisaid)
k1 = plot_income_colab(ncbi)
l1 = plot_income_colab(ddjb)
m1 = plot_income_colab(ena)
ggarrange(j1,k1,m1,l1,common.legend = TRUE)
ggsave(
  paste0("imgs/income-colab-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)

plot_palma_ratio <- function(data) {
  data_name = deparse(substitute(data))
  # Data Cleaning
  df_clean <- data %>%
    separate_rows(Country.of.standardized.research.organization, sep = ";") %>%
    mutate(Country.of.standardized.research.organization = str_trim(Country.of.standardized.research.organization)) %>%
    filter(Country.of.standardized.research.organization != "")

  # Count Articles for Each Country
  country_counts <- df_clean %>%
    group_by(Country.of.standardized.research.organization) %>%
    tally() %>%
    arrange(desc(n))

  country_counts$Code = countrycode(country_counts$Country.of.standardized.research.organization, "country.name", "iso3c")

  country_counts = inner_join(country_counts,class)
  country_counts <- country_counts %>% filter(!is.na(Income.group))

  # Calculate Palma Ratio for Each Country
  total_articles <- sum(country_counts$n)
  country_counts <- country_counts %>%
    mutate(Palma_Ratio = n / total_articles)

  country_counts$Income.group = factor(country_counts$Income.group,
                                       levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))

  # Create Plot
  # Create Plot
  p <- ggplot((country_counts), aes(x = n, y = Palma_Ratio, label = Country.of.standardized.research.organization)) +
    geom_point(aes(size = Income.group), color = "white", na.rm = TRUE) +  # specify the size and color here
    scale_size_manual(breaks = levels(country_counts$Income.group), values = c(1, 2, 3, 4)) + # You can specify the sizes here
    geom_text_repel(nudge_x = 30, nudge_y = 0.005, box.padding = 0.5, colour = "white") +
    labs(title = paste(stringr::str_to_upper(data_name)),
         x = "Number of Collaborators",
         y = "Palma Ratio") +
    custom_theme_oss()  # Assuming you have a custom theme function named `custom_theme_oss`

  print(p)

}
gg = plot_palma_ratio(gisaid)
nn = plot_palma_ratio(ncbi)
ee = plot_palma_ratio(ena)
dd = plot_palma_ratio(ddjb)
ggarrange(gg,nn,ee,dd,common.legend = TRUE)
ggsave(
  paste0("imgs/palma-ratio-plot.png"),
  dpi = 320,
  width = 16,
  height = 18,
  limitsize = FALSE
)



split_collab_matrix = function(data, col_name) {
  # create list of individual authors for each paper
  V = data[, c(col_name)]
  pub_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))
  pub_auths = lapply(pub_auths, trimws)
  # for each paper, form a data frame of unique author pairs
  auth_pairs = lapply(pub_auths, function(x) {
    z  = expand.grid(x, x, stringsAsFactors = FALSE)
    z[z$Var1 < z$Var2, ]
  })
  # combine list of matrices for each paper into one data frame
  auth_pairs = do.call(rbind, auth_pairs)
  # count papers for each author pair
  auth_count = aggregate(paste(Var1, Var2)  ~ Var1 + Var2 , data = auth_pairs, length)
  colnames(auth_count) = c("country1", "country2", "weight")

  return(auth_count)
}
plot_collaboration_map = function(data) {

  d = split_collab_matrix(data,"Country.of.standardized.research.organization")
  world_map <- map_data("world")

  # Calculate centroids for countries
  country_centroids <- world_map %>%
    group_by(region) %>%
    summarise(x = mean(long), y = mean(lat))
  # Using your code to prepare data
  df = d |>
    left_join(country_centroids, by = c("country1" = "region")) |>
    rename(x1 = x, y1 = y) |>
    left_join(country_centroids, by = c("country2" = "region")) |>
    rename(x2 = x, y2 = y)

  collaborations = df |>
    gather("country_type", "country", country1, country2) |>
    group_by(country) |>
    summarise(total_collabs = n())

  collaborations=collaborations |> dplyr::rename(Economy = country)
  collaborations = inner_join(collaborations,class)

  world_map = map_data("world") |>
    mutate(region = ifelse(region == "USA", "United States", region))


  # Merge total collaborations with world map
  world_map = world_map |>
    left_join(collaborations, by = c("region" = "Economy"))

  # Determine the min and max weight values
  weight_min = min(df$weight, na.rm = TRUE)
  weight_max = max(df$weight, na.rm = TRUE)

  # Plot
  N = 5
  top_countries = collaborations |>
    group_by(Economy) |>
    summarise(total = sum(total_collabs, na.rm = TRUE)) |>
    top_n(N, wt = total) |>
    pull(Economy)

  # Pseudo McArthur projection by inverting latitudes


  # Calculate the 75th percentile cut-off value for weight
  top_quartile_weight = quantile(df$weight, 0.75, na.rm = TRUE)

  # Filter df to include only rows with weight in the top quartile
  df_filtered = df %>% filter(weight >= top_quartile_weight)

  # Gall-Peters projection added using coord_map
  map = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = total_collabs),
                 color = ifelse(world_map$region %in% top_countries, "red", "#e0e0e0")) +
    geom_segment(data = df_filtered,  # Use filtered data
                 aes(x = x1, xend = x2, y = y1, yend = y2, size = weight, color = weight, alpha = 0.2),
                 lineend = 'round') +
    scale_fill_viridis_c(name = "Total Collaborations", na.value = "#f5f5f5", option = "viridis") +
    scale_color_viridis_c(name = "Log(Collaboration Weight)", trans = "log10", option = "plasma") +
    scale_size_continuous(name = "Weight", range = c(0.5, 2)) +
    scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
    custom_theme_oss() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#ffffff", colour = "#e0e0e0"),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10))


  # Compute means, standard error, and 95% CI for Income.group
  income_stats = collaborations |>
    group_by(Income.group) |>
    summarise(mean_collabs = mean(total_collabs, na.rm = TRUE),
              se = sd(total_collabs, na.rm = TRUE) / sqrt(n()),
              lower = mean_collabs - (1.96 * se),
              upper = mean_collabs + (1.96 * se))

  # Compute means, standard error, and 95% CI for Region
  region_stats = collaborations |>
    group_by(Region) |>
    summarise(mean_collabs = mean(total_collabs, na.rm = TRUE),
              se = sd(total_collabs, na.rm = TRUE) / sqrt(n()),
              lower = mean_collabs - (1.96 * se),
              upper = mean_collabs + (1.96 * se))

  # Plotting for Income.group
  bar1 = ggplot(income_stats, aes(x = Income.group, y = mean_collabs)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
    labs(title = "Mean Collaboration with 95% CI by Income Group",
         y = "Mean Collaborations") +
    custom_theme_oss()

  # Plotting for Region
  bar2 = ggplot(region_stats, aes(x = Region, y = mean_collabs)) +
    geom_bar(stat = "identity", fill = "lightcoral") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
    labs(title = "Mean Collaboration with 95% CI by Region",
         y = "Mean Collaborations") +
   custom_theme_oss() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggarrange(
    map,          # Top plot (map)
    ggarrange(bar2, bar1, ncol = 2), # Bottom plots (bar graphs)
    nrow = 2,
    heights = c(2, 1) # 2/3 of the height allocated for the map, 1/3 for the bars
  )

}
plot_collab_network = function(data, type) {
  d = split_collab_matrix(data,type)

  df_top = d |>
    arrange(desc(weight)) |>
    head(250)

  g = graph_from_data_frame(df_top, directed = FALSE)

  # Calculate betweenness centrality
  V(g)$betweenness = betweenness(g)

  # Detect communities
  communities = cluster_louvain(g)
  V(g)$community = communities$membership

  # Extract names of top nodes for labels
  top_node_names = V(g)[order(-V(g)$betweenness)][1:15]$name

  # Plot the graph using ggraph
  ggraph(g, layout = "circle") +
    geom_edge_link(aes(width = sqrt(weight), color = weight), alpha = 0.5) +
    geom_node_point(aes(size = betweenness, color = as.factor(community))) +  # Color nodes by community
    geom_node_text(aes(label = ifelse(name %in% top_node_names, name, NA), size = betweenness),
                   repel = TRUE,
                   nudge_y = 0.5,
                   check_overlap = TRUE,
                   color = "black") +
    scale_edge_color_gradient(low = "gray70", high = "tomato3") +
    scale_color_viridis_d() +
    custom_theme_oss() +
    theme(legend.position = "right",
          plot.background = element_rect(fill = "ivory", color = "ivory"),
          legend.title = element_text(face = "bold")) +
    labs(title = paste0("Collaboration Network of Researchers based on",type),
         subtitle = "Colored by detected network communities",
         caption = "Node colors represent network communities. Edge colors represent collaboration weights.",
         x = "", y = "")
}



### MAP
plot_collaboration_map(ena)
plot_collaboration_map(gisaid)
plot_collaboration_map(ena)
plot_collaboration_map(ena)



plot_collab_network(ena,"Research.Organizations...standardized")


plot_colbs_graph = function(data, column_name) {

  ## KEYWORDS NETWORK
  keywords_split = data |>
    separate_rows(!!sym(column_name), sep = ";") |>
    mutate(across(!!sym(column_name), str_trim)) |>
    filter(!!sym(column_name) != "")

  keyword_freq = table(keywords_split[[column_name]])

  # Filter to get top 40 keywords
  top_keywords = names(sort(keyword_freq, decreasing = TRUE)[1:13])
  keywords_filtered = keywords_split |> filter(!!sym(column_name) %in% top_keywords)

  # Counting co-occurrences
  cooccurrence_matrix = crossprod(table(keywords_filtered$Publication.ID, keywords_filtered[[column_name]]))
  diag(cooccurrence_matrix) = 0

  # Filter edges with minimum weight for clarity
  min_weight = 5
  cooccurrence_matrix[cooccurrence_matrix < min_weight] = 0

  # Create the graph object
  graph = graph_from_adjacency_matrix(cooccurrence_matrix, weighted = TRUE, mode = "undirected")
  graph = simplify(graph)  # Removes self-loops

  # Call indices function to get metrics for the graph
  graph_indices = indicies(graph)

  # Graph plotting
  set.seed(123)
  g = ggraph(graph, layout = 'circle') +
    geom_edge_arc0(aes(color = weight, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
    scale_edge_width_continuous(range = c(0.1, 2)) +
    scale_edge_colour_identity() +
    geom_node_point(aes(size = degree(graph), color = betweenness(graph))) +
    geom_node_text(aes(label = name), vjust = 0, hjust = 0.3, check_overlap = TRUE, size = 5, color = "red", fontface = "bold", family = "Times New Roman") +
    scale_color_viridis_c() +
    scale_size_continuous(range = c(0.3, 16), name = "degree") +
    theme(
      plot.background = element_rect(fill = "black", color = "black"),
      panel.background = element_rect(fill = "black", color = "black"),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      plot.title = element_text(color = "white", size = 20),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    ) + custom_theme_oss_no_axis() +
    labs(title = paste(stringr::str_to_title(column_name))) +
    guides(size = guide_legend(override.aes = list(color = "white")))

  return(list(graph = g, indices = graph_indices))
}

# Function to generate and save plots for each data frame
generate_and_save_plots <- function(data, data_name) {
  # Initialize a data frame to store all indices
  all_indices <- data.frame()

  # Define columns to analyze
  columns_to_analyze <- c("Research.Organizations...standardized",
                          "Country.of.standardized.research.organization",
                          "City.of.standardized.research.organization",
                          "Authors",
                          "Funder.Group",
                          "Funder.Country")

  # Generate individual plots and indices based on the given data frame
  plots <- list()
  for (column_name in columns_to_analyze) {
    plot_data <- plot_colbs_graph(data, column_name)
    plots[[column_name]] <- plot_data$graph

    # Add a "DataName" and "ColumnName" column to identify the data frame and column
    tmp_indices <- cbind(data.frame(DataName = data_name, ColumnName = column_name),
                         plot_data$indices)
    all_indices <- rbind(all_indices, tmp_indices)
  }

  # Combine the plots using ggarrange
  pop = ggarrange(plots$Research.Organizations...standardized,
                  plots$Country.of.standardized.research.organization,
                  plots$City.of.standardized.research.organization,
                  plots$Authors,
                  plots$Funder.Group,
                  plots$Funder.Country,
                  nrow = 2, ncol = 3)

  # Display the annotated figure
  print(pop)

  # Save the plot
  ggsave(
    paste0("imgs/", data_name, "-networks-plot.png"),
    dpi = 320,
    width = 22,
    height = 22,
    limitsize = FALSE
  )

  # Save the indices data frame as a CSV file
  write.csv(all_indices, paste0("indices_", data_name, ".csv"), row.names = FALSE)
}

# List of data frames and their names
data_frames = list(gisaid = gisaid, ncbi = ncbi, ena = ena, ddjb = ddjb)

# Loop through each data frame and generate/save plots
for (data_name in names(data_frames)) {
  generate_and_save_plots(data_frames[[data_name]], data_name)
}




## OLD
### GINI
# plot_gini_collab = function(data) {
#   # Extract countries from the affiliations
#   data_name = deparse(substitute(data))
#   # 1. Break down the country affiliations
#   country_collab = data |>
#     select(Country.of.standardized.research.organization) |>
#     unnest(country = strsplit(as.character(Country.of.standardized.research.organization), ';')) |>
#     dplyr::mutate(country = stringr::str_trim(country)) |>  # Remove whitespaces from both sides
#     dplyr::mutate(country = stringr::str_squish(country)) |>  # Ensure that any multi-space sequences within the name are turned into a single space
#     group_by(country) |>
#     tally() |>
#     arrange(-n)  # Optional: Order by descending counts
#
#   # 2. Compute the number of collaborators per country
#   total_collaborators = sum(country_collab$n)
#
#   # 3. Calculate each country's ratio of collaborators
#   country_collab = country_collab |>
#     mutate(ratio = n / total_collaborators) |>
#     arrange(-n)
#
#   country_collab$Code = countrycode(country_collab$country, "country.name", "iso3c")
#
#   country_collab = inner_join(country_collab,class)
#
#   # 4. Plot
#   # Assuming you've merged and have the 'Income.Group' column in the country_collab dataframe
#   country_collab$Income.group = factor(country_collab$Income.group,
#                                        levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))
#
#   country_collab = country_collab[!is.na(country_collab$Income.group) & country_collab$Income.group != "", ]
#
#   ggplot(country_collab, aes(x=n, y=ratio, label=country)) +
#
#     # Adjusting the geom_point aesthetics
#     geom_point(aes(color=Income.group, size=Income.group), alpha=0.7) +
#
#     # Label for top 5 countries
#     geom_text(data=head(country_collab, 5), aes(label=country), vjust=-1, hjust=1, color="white") +
#
#     # Add some jitter to separate overlapping points
#     geom_jitter() +
#
#     # Titles and labels
#     labs(title = paste(stringr::str_to_upper(data_name)),
#          x = "Number of Collaborators (Log Scale)",
#          y = "Ratio of Total Collaborators") +
#
#     # Log scale for x-axis
#     scale_x_log10() +
#
#     # Minimal theme from hrbrthemes
#     custom_theme_oss() +
#
#     # Position the legend at the bottom
#     theme(legend.position="bottom")
# }
# e = plot_gini_collab(ddjb)
# f = plot_gini_collab(ena)
# h = plot_palma_collab(gisaid)
# i = plot_gini_collab(ncbi)
# ggarrange(e,f,h,i, common.legend = TRUE)
# ggsave(
#   paste0("imgs/gini-colab-plot.png"),
#   dpi = 320,
#   width = 16,
#   height = 18,
#   limitsize = FALSE
# )
