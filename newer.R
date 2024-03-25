# packages
package_list <- c(
  "here", # use for paths creation
  "tidyverse",
  "janitor", # useful functions for cleaning imported data
  "biblionetwork", # creating edges
  "tidygraph", # for creating networks
  "ggraph" # plotting networks
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c(
  "agoutsmedt/networkflow", # manipulating network
  "ParkerICI/vite" # needed for the spatialisation of the network
)
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}

# paths
data_path <- here(path.expand("~"),
                  "data",
                  "tuto_biblio_dsge")
dimensions_path <- here(data_path,
                        "dimensions")



dimensions_data <- read_csv(here(
                                   "../../../Downloads/ddjb.csv"),
                              skip = 1)

dimensions_data <- dimensions_data |>
  clean_names() # cleaning column names with janitor to transform them

dimensions_data

duplicated_articles <- dimensions_data %>%
  add_count(title) %>%
  filter(n > 1) %>%
  arrange(title, cited_references)

to_keep <- duplicated_articles %>%
  group_by(title) %>%
  slice_head(n = 1)

to_remove <- duplicated_articles %>%
  filter(! publication_id %in% to_keep$publication_id)

dimensions_data <- dimensions_data %>%
  filter(! publication_id %in% to_remove$publication_id)

dimensions_affiliations <- dimensions_data %>%
  select(publication_id,
         authors_affiliations_name_of_research_organization,
         authors_affiliations_country_of_research_organization) %>%
  separate_rows(starts_with("authors"), sep = "; ")

knitr::kable(head(dimensions_affiliations))

dimensions_affiliations <- dimensions_affiliations %>%
  mutate(data = "dimensions")

affiliations <- dimensions_affiliations %>%
  rename("affiliations" = authors_affiliations_name_of_research_organization,
         "country" = authors_affiliations_country_of_research_organization,
         "citing_id" = publication_id)

affiliations %>%
  filter(!is.na(country)) %>%
  group_by(data) %>%
  count(country) %>%
  mutate(proportion = n/sum(n)) %>%
  slice_max(order_by = proportion, n = 10, with_ties = FALSE) %>%  # see https://juliasilge.com/blog/reorder-within/ for more info
  ggplot(aes(proportion, country, n = data)) +
  geom_col(show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  labs(x = "Proportion of the Country in all affiliations (excluding NA)") +
  theme_classic(base_size = 16)


references_extract <- dimensions_data %>%
  filter(! is.na(cited_references)) %>%
  rename("citing_id" = publication_id) %>% # because the "publication_id" column is also the identifier of the reference
  select(citing_id, cited_references) %>%
  separate_rows(cited_references, sep = ";(?=\\[)") %>%
  as_tibble

column_names <- c("authors",
                  "author_id",
                  "source",
                  "year",
                  "volume",
                  "issue",
                  "pagination",
                  "doi",
                  "publication_id",
                  "times_cited")

dimensions_direct_citation <- references_extract %>%
  separate(col = cited_references, into = column_names, sep = "\\|")

knitr::kable(head(dimensions_direct_citation, n = 5))

dimensions_references <- dimensions_direct_citation %>%
  distinct(publication_id, .keep_all = TRUE) %>%
  select(-citing_id)

knitr::kable(tail(dimensions_references, n = 5))

dimensions_references <- dimensions_references %>%
  mutate(references = paste0(authors, ", ", year, ", ", source))

dimensions_top_ref <- dimensions_direct_citation %>%
  add_count(publication_id) %>%
  mutate(proportion = n/n()) %>%
  select(publication_id, proportion) %>%
  unique() %>%
  slice_max(proportion, n = 10) %>%
  left_join(select(dimensions_references, publication_id, references)) %>%
  select(references, proportion) %>%
  mutate(data = "dimensions")

dimensions_top_ref %>%
  mutate(references = str_wrap(references, 35)) %>%
  ggplot(aes(proportion, fct_reorder(references, proportion))) +
  geom_col(show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  labs(x = "Proportion on all citations") +
  theme_classic(base_size = 13)

citations <- dimensions_direct_citation %>%
  add_count(publication_id) %>%
  select(publication_id, n) %>%
  unique

references_filtered <- dimensions_references %>%
  left_join(citations) %>%
  filter(n >= 5)

edges <- biblionetwork::biblio_cocitation(filter(dimensions_direct_citation, publication_id %in% references_filtered$publication_id),
                                          "citing_id",
                                          "publication_id",
                                          weight_threshold = 3)
edges

references_filtered <- references_filtered %>%
  relocate(publication_id, .before = authors) # first column has to be the identifier

graph <- tbl_main_component(nodes = references_filtered,
                            edges = edges,
                            directed = FALSE)

tbl_main_component <- function(edges, nodes, nb_components = 1, threshold_alert = 0.05, ...) {

  # Listing the variables not in the global environment to avoid a "note" saying "no visible binding for global variable ..." when using check()
  # See https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  components_att <- NULL
  lifecycle::deprecate_warn("0.1.0", "tbl_main_component()", "filter_components()")

  # creating the tidygraph object
  graph <- tidygraph::tbl_graph(references_filtered,edges)

  # attributing a number to the different components (1 is the biggest components)
  graph <- graph %N>%
    dplyr::mutate(components_att = tidygraph::group_components(type = "weak")) %>%
    dplyr::rename_at(1, ~"Id") # renamed the first column to a standard format

  nb_components = 1
  threshold_alert = 0.05
  # looking at the biggest component just after the last one we have kept
  threshold <- graph %>%
    dplyr::filter(components_att == nb_components + 1) %>%
    dplyr::rename_at(1, ~"Id")

  # looking at the percentage of nodes in the biggest component just after the last one we have kept
  # trigger a warning if superior to the threshold_alert
  if (length(igraph::V(threshold)$Id) / length(igraph::V(graph)$Id) > threshold_alert) warning(paste0("Warning: you have removed a component gathering more than ", threshold_alert, "% of the nodes"))

  # keeping only the number of components we want
  graph <- graph %>%
    dplyr::filter(components_att <= nb_components) %>%
    dplyr::select(-components_att) # we remove the column as it won't be useful after that
}
graph
