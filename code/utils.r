generate_report = function(df) {
  # MAIN INFORMATION
  timespan = range(df$PubYear, na.rm = TRUE)
  sources = length(unique(df$Source.title))
  documents = nrow(df)
  annual_growth_rate =
    (documents / nrow(df[df$PubYear == min(df$PubYear, na.rm = TRUE), ])) ^ (1 / (max(df$PubYear, na.rm = TRUE) - min(df$PubYear, na.rm = TRUE))) - 1
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
  ggplot(total_publications_all,
         aes(x = MonthYear, y = total_publications, color = source)) +
    geom_line() +
    labs(title = "Total Publications", x = "Month-Year", y = "Total Publications") +
    custom_theme_oss() +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
    scale_color_discrete(
      labels = function(x)
        tools::toTitleCase(toupper(x))
    ) +
    # Making labels uppercase
    coord_cartesian(xlim = c(
      min(total_publications_all$MonthYear, na.rm = TRUE),
      as.Date("2023-01-31")
    ), ylim = c(0, 600))
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
  ggplot(total_citations_all,
         aes(x = MonthYear, y = total_citations, color = source)) +
    geom_line() +
    labs(title = "Total Citations", x = "Month-Year", y = "Total Citations") +
    custom_theme_oss() +   scale_color_discrete(
      labels = function(x)
        tools::toTitleCase(toupper(x))
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
    coord_cartesian(xlim = c(
      min(total_citations_all$MonthYear, na.rm = TRUE),
      as.Date("2023-01-31")
    ))
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
  ggplot(avg_citations_all,
         aes(x = MonthYear, y = avg_citations, color = source)) +
    geom_line() +
    labs(title = "Average Article Citations", x = "Month-Year", y = "Average Citations") +
    custom_theme_oss() +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +   scale_color_discrete(
      labels = function(x)
        tools::toTitleCase(toupper(x))
    ) +
    coord_cartesian(xlim = c(
      min(avg_citations_all$MonthYear, na.rm = TRUE),
      as.Date("2023-01-31")
    ))
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
  ggplot(avg_altmetrics_all,
         aes(x = MonthYear, y = avg_altmetrics, color = source)) +
    geom_line() +
    labs(title = "Average Altmetric Score", x = "Month-Year", y = "Average Altmetric Score") +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") + custom_theme_oss() +   scale_color_discrete(
      labels = function(x)
        tools::toTitleCase(toupper(x))
    ) +
    coord_cartesian(xlim = c(
      min(avg_altmetrics_all$MonthYear, na.rm = TRUE),
      as.Date("2023-01-31")
    ))  # Assuming you have a function custom_theme_oss defined
}
plot_access = function(name1, name2, name3, name4) {
  get_oa = function(data, name) {
    oa = as.data.frame(table(data$Open.Access)) |> dplyr::rename("Access Type" = Var1)
    oa$DI = name
    oa$Percent = (oa$Freq / sum(oa$Freq)) * 100
    return(oa)
  }
  oa_data = rbind(get_oa(gisaid, name1),
                  get_oa(ena, name2),
                  get_oa(ddbj, name3),
                  get_oa(ncbi, name4))

  oa_data = oa_data %>% dplyr::arrange(DI, desc(Percent))

  # Print percentage distribution
  print("Percentage Distribution of Access Types:")
  print(oa_data %>% dplyr::select(DI, 'Access Type', Percent))


  ggplot(oa_data,
         aes(
           area = Freq,
           fill = Freq,
           label = `Access Type`,
           subgroup = DI
         )) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 14) +
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
      fontface = "bold",
      size = 12,
      grow = TRUE
    ) + ggtitle("Publication Access Types") + theme_pubclean() + scale_fill_continuous(type = "gradient", guide = guide_legend()) +
    theme(legend.position = "bottom") + custom_theme_oss() +
    scale_fill_viridis_c()
}
plot_publisher = function(name1, name2, name3, name4) {
  get_publisher = function(data, name) {
    publisher_data = as.data.frame(table(data$Publisher)) %>% dplyr::rename("Publisher" = Var1)
    publisher_data$DI = name
    # Calculate percentage distribution
    publisher_data$Percent = (publisher_data$Freq / sum(publisher_data$Freq)) * 100
    return(publisher_data)
  }

  publisher_data = rbind(
    get_publisher(gisaid, name1),
    get_publisher(ena, name2),
    get_publisher(ddbj, name3),
    get_publisher(ncbi, name4)
  )

  publisher_data = publisher_data %>% dplyr::arrange(DI, desc(Percent))
  # Print percentage distribution
  print("Percentage Distribution of Access Types:")
  print(publisher_data %>% dplyr::select(DI, Publisher, Percent))

  ggplot(publisher_data,
         aes(
           area = Freq,
           fill = Freq,
           label = Publisher,
           subgroup = DI
         )) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 14) +
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
      fontface = "bold",
      size = 12,
      grow = TRUE
    ) + ggtitle("Publication Journal Types") + theme_pubclean() + scale_fill_continuous(type = "gradient", guide = guide_legend()) +
    theme(legend.position = "bottom") + custom_theme_oss() +
    scale_fill_viridis_c()
}
plot_keyword_graph = function(data) {
  ## Capture the name of the data
  data_name = deparse(substitute(data))

  ## KEYWORDS NETWORK
  keywords_split = data |>
    separate_rows(MeSH.terms, sep = "[;,]") |>
    mutate(MeSH.terms = str_trim(MeSH.terms)) |>
    filter(MeSH.terms != "") |>
    mutate(MeSH.terms = stringr::str_to_lower(MeSH.terms)) |>
    filter(MeSH.terms != "covid-19") |>
    filter(MeSH.terms != "sars-cov-2") |>
    filter(MeSH.terms != "coronavirus") |>
    filter(MeSH.terms != "") |>
    filter(MeSH.terms != " ")


  keyword_freq = table(keywords_split$MeSH.terms)

  # Filter to get top 40 keywords
  top_keywords = names(sort(keyword_freq, decreasing = TRUE)[1:15])
  keywords_filtered = keywords_split |> filter(MeSH.terms %in% top_keywords)

  # Counting co-occurrences
  cooccurrence_matrix = crossprod(table(
    keywords_filtered$Publication.ID,
    keywords_filtered$MeSH.terms
  ))
  diag(cooccurrence_matrix) = 0

  # Filter edges with minimum weight for clarity
  min_weight = 5
  cooccurrence_matrix[cooccurrence_matrix < min_weight] = 0

  # Create the graph object
  graph = graph_from_adjacency_matrix(cooccurrence_matrix, weighted = TRUE, mode = "undirected")
  graph = simplify(graph)  # Removes self-loops

  # Compute weighted degree centrality
  # Compute weighted degree centrality using strength function
  weighted_degree = strength(graph)

  # Graph plotting
  set.seed(123)
  ggraph(graph, layout = 'circle') +
    geom_edge_arc0(
      aes(color = weight, width = weight),
      alpha = 0.4,
      strength = 0.2,
      show.legend = FALSE
    ) +
    scale_edge_width_continuous(range = c(0.1, 2)) +
    scale_edge_colour_identity() +
    geom_node_point(aes(size = degree(graph), color = weighted_degree)) +  # Using weighted degree centrality for color
    geom_node_text(
      aes(label = name),
      vjust = 0,
      hjust = 0.3,
      check_overlap = TRUE,
      size = 7,
      color = "red",
      fontface = "bold",
      family = "Times New Roman"
    ) +
    scale_color_gradient() +
    scale_size_continuous(range = c(1, 16), name = "degree") +
    custom_theme_oss_no_axis() +
    labs(title = paste(stringr::str_to_upper(data_name))) +
    theme(legend.position = "right")
}
plot_variants = function(data) {
  # Define the WHO labels and Pango Lineages
  who_labels =
    c(
      "alpha",
      "beta",
      "gamma",
      "delta",
      "epsilon",
      "eta",
      "iota",
      "kappa",
      "omicron",
      "zeta",
      "mu"
    )
  pango_lineages =
    c(
      "b.1.1.7",
      "b.1.351",
      "p.1",
      "b.1.617.2",
      "b.1.427",
      "b.1.429",
      "b.1.525",
      "b.1.526",
      "b.1.617.1",
      "b.1.617.3",
      "b.1.1.529",
      "p.2",
      "b.1.621",
      "b.1.621.1"
    )
  data_name = deparse(substitute(data))
  # Combine the labels and lineages into a single vector
  all_variants = c(who_labels, pango_lineages)

  # Create an empty dataframe to hold the counts
  variant_counts = data.frame(word = character(), n = integer())

  # Convert the Abstracts to lowercase
  data$Abstract = tolower(data$Abstract)

  # Loop through all variants to count occurrences in each abstract
  for (variant in all_variants) {
    temp_df = data %>%
      filter(str_detect(Abstract, regex(
        paste0("\\b", variant, "\\b"), ignore_case = TRUE
      ))) %>%
      summarise(n = n())

    if (nrow(temp_df) > 0 && temp_df$n > 0) {
      variant_counts =
        bind_rows(variant_counts, data.frame(word = variant, n = temp_df$n))
    }
  }

  # Filter out zero counts
  variant_counts = variant_counts %>% filter(n > 0)

  # Create the plot
  ggplot(variant_counts, aes(x = reorder(word,-n), y = n)) +
    geom_col(fill = "orange") +
    xlab("Variant / Lineage") +
    ylab("Number of Mentions") +
    ggtitle(paste(stringr::str_to_upper(data_name))) +
    coord_flip() + custom_theme_oss()  # Assuming you have a function custom_theme_oss defined
}
plot_scp_mcp = function(data) {
  # Calculate collaboration based on Region
  data_processed = data %>%
    mutate(Country = str_split(Country.of.standardized.research.organization, ";")) %>%
    unnest(Country) %>%
    mutate(Country = str_trim(Country)) %>%
    filter(Country != "",!is.na(Country)) %>%  # Remove empty or NA countries
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

  ggplot(data_final, aes(
    fill = collaboration,
    y = Articles,
    x = reorder(Country,-Articles, sum)
  )) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = stringr::str_to_upper(deparse(substitute(data)))) +
    xlab("Country") +
    ylab("No. Documents") +
    coord_flip()  +
    theme(
      axis.text  = element_text(
        colour = "black",
        size = 17,
        face = "bold"
      ),
      axis.text.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      legend.position = "none"
    ) +  # Hide the legend
    custom_theme_oss()
}
plot_income_colab = function(data) {
  data_name = deparse(substitute(data))

  # Calculate collaboration based on Income.group
  data_processed = data %>%
    mutate(Country = str_split(Country.of.standardized.research.organization, ";")) %>%
    unnest(Country) %>%
    mutate(Country = str_trim(Country)) %>%
    filter(Country != "",!is.na(Country)) %>%  # Remove empty or NA countries
    inner_join(class, by = c("Country" = "Economy")) %>%
    group_by(Country.of.standardized.research.organization) %>%
    filter(length(unique(Country)) > 1) %>%  # Filter out internal collaborations
    mutate(
      collaboration = case_when(
        all(Income.group %in% c("High income")) ~ "HI",
        all(Income.group %in% c("High income", "Upper middle income")) ~ "HI-UMI",
        all(Income.group %in% c("High income", "Lower middle income")) ~ "HI-LMI",
        all(Income.group %in% c("High income", "Low income")) ~ "HI-LI",
        "High income" %in% Income.group ~ "HI-MIX",
        all(
          Income.group %in% c("Upper middle income", "Lower middle income")
        ) ~ "UMI-LMI",
        all(Income.group %in% c("Upper middle income", "Low income")) ~ "UMI-LI",
        "Upper middle income" %in% Income.group ~ "UMI-MIX",
        all(Income.group %in% c("Lower middle income", "Low income")) ~ "LMI-LI",
        TRUE ~ "OTHER"
      )
    ) %>%
    ungroup() %>%
    group_by(Country, collaboration) %>%
    summarize(Articles = n(), .groups = "drop")

  # Filter out the 'OTHER' (Optional, but removes the unnecessary category)
  data_processed = filter(data_processed, collaboration != "OTHER")

  incomestats = data_processed %>%
    group_by(Country) %>%
    mutate(Percent = Articles / sum(Articles) * 100) %>%
    ungroup()

  # Get top 20 countries
  top20 = data_processed %>%
    group_by(Country) %>%
    summarize(Total = sum(Articles)) %>%
    top_n(50, Total)

  data_final = filter(data_processed, Country %in% top20$Country)

  # Plot
  # Reorder the Country factor levels within the data frame
  data_final$Country =
    factor(
      data_final$Country,
      levels = data_final %>%
        group_by(Country) %>%
        summarize(Total = sum(Articles)) %>%
        arrange(-Total) %>%
        pull(Country)
    )

  # Plot
  p = ggplot(data_final, aes(fill = collaboration, y = Articles, x = Country)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(
      title = paste(stringr::str_to_upper(data_name)),
      xlab("Country (Ordered by Total Document Count)"),
      ylab("Number of Documents")
    ) +
    coord_flip() +
    theme(
      axis.text  = element_text(
        colour = "black",
        size = 17,
        face = "bold"
      ),
      axis.text.y = element_text(size = 16),
      axis.text.x = element_text(size = 14)
    ) +
    custom_theme_oss() +
    scale_fill_discrete(drop = FALSE, name = "Income-Group Collaboration")


  return(list(incomestats, p))
}
plot_palma_ratio = function(data) {
  data_name = deparse(substitute(data))
  # Data Cleaning
  df_clean = data %>%
    separate_rows(Country.of.standardized.research.organization, sep = ";") %>%
    mutate(
      Country.of.standardized.research.organization = str_trim(Country.of.standardized.research.organization)
    ) %>%
    filter(Country.of.standardized.research.organization != "")

  # Count Articles for Each Country
  country_counts = df_clean %>%
    group_by(Country.of.standardized.research.organization) %>%
    tally() %>%
    arrange(desc(n))

  country_counts$Code = countrycode(
    country_counts$Country.of.standardized.research.organization,
    "country.name",
    "iso3c"
  )

  country_counts = inner_join(country_counts, class)
  country_counts = country_counts %>% filter(!is.na(Income.group))

  # Calculate Palma Ratio for Each Country
  total_articles = sum(country_counts$n)
  country_counts = country_counts %>%
    mutate(Palma_Ratio = n / total_articles)

  country_counts$Income.group = factor(
    country_counts$Income.group,
    levels = c(
      "Low income",
      "Lower middle income",
      "Upper middle income",
      "High income"
    )
  )

  # Create Plot
  # Create Plot
  p =
    ggplot((country_counts),
           aes(x = n, y = Palma_Ratio, label = Country.of.standardized.research.organization)
    ) +
    geom_point(aes(size = Income.group, colour = Income.group), na.rm = TRUE) +  # specify the size and color here
    scale_size_manual(breaks = levels(country_counts$Income.group),
                      values = c(1, 2, 3, 4)) + # You can specify the sizes here
    geom_text_repel(
      nudge_x = 30,
      nudge_y = 0.005,
      box.padding = 0.5,
      colour = "white"
    ) +
    labs(title = paste(stringr::str_to_upper(data_name)),
         x = "Number of Collaborators",
         y = "Palma Ratio") +
    custom_theme_oss()  # Assuming you have a custom theme function named `custom_theme_oss`

  print(p)

}
split_collab_matrix = function(dt, col_name) {
  # Ensure that 'dt' is a data.table
  if (!is.data.table(dt)) {
    dt = data.table::data.table(dt)
  }

  # Remove all spaces and split collaborations
  pub_collabs =
    strsplit(gsub(" ", "", dt[[col_name]]), split = ";")

  # Generate collaboration pairs for each record
  collab_pairs =
    data.table::rbindlist(lapply(pub_collabs, function(collabs) {
      if (length(collabs) < 2)
        return(NULL)  # Skip if fewer than 2 collaborations

      pairs = data.table::CJ(collab1 = collabs, collab2 = collabs)
      pairs =
        pairs[collab1 < collab2]  # This ensures we don't duplicate pairs

      return(pairs)
    }),
    use.names = TRUE,
    fill = TRUE)

  # Count records for each collaboration pair
  collab_count =
    collab_pairs[, .(weight = .N), by = .(country1 = collab1, country2 = collab2)]

  return(collab_count)
}
stats_collab_network = function(data, type) {
  # Add a source column to each data frame and bind them together
  d = split_collab_matrix(data, type)
  d = as.data.frame(d)

  g = graph_from_data_frame(d, directed = FALSE)
  netstat = bibliometrix::networkStat(g)
  summary(netstat, k = 10)

}
stats_author_collab_network = function(data, type) {
  # Add a source column to each data frame and bind them together
  d = split_collab_matrix(data, type)
  d = as.data.frame(d)
  d = d %>% filter(weight > 5)

  g = graph_from_data_frame(d, directed = FALSE)
  netstat = bibliometrix::networkStat(g)
  summary(netstat, k = 10)

}
plot_collaboration_map = function(data) {
  d = split_collab_matrix(ddbj, "Country.of.standardized.research.organization")

  d$country1 =
    gsub("(?<=\\p{Lu})(?=\\p{Lu}|$)", " ", d$country1, perl = TRUE)
  d$country2 =
    gsub("(?<=\\p{Lu})(?=\\p{Lu}|$)", " ", d$country1, perl = TRUE)



  world_map = map_data("world")

  # Calculate centroids for countries
  country_centroids = world_map %>%
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

  collaborations = collaborations |> dplyr::rename(Economy = country)
  collaborations = inner_join(collaborations, class)

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

  # Calculate the 75th percentile cut-off value for weight
  top_quartile_weight = quantile(df$weight, 0.15, na.rm = TRUE)

  # Filter df to include only rows with weight in the top quartile
  df_filtered = df %>% filter(weight >= top_quartile_weight)

  # Gall-Peters projection added using coord_map
  ggplot() +
    geom_polygon(
      data = world_map,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = total_collabs
      ),
      color = ifelse(world_map$region %in% top_countries, "red", "#e0e0e0")
    ) +
    geom_segment(
      data = df_filtered,
      # Use filtered data
      aes(
        x = x1,
        xend = x2,
        y = y1,
        yend = y2,
        size = weight,
        color = weight,
        alpha = 0.2
      ),
      lineend = 'round'
    ) +
    scale_fill_viridis_c(name = "Total Collaborations",
                         na.value = "#f5f5f5",
                         option = "viridis") +
    scale_color_viridis_c(name = "Log(Collaboration Weight)",
                          trans = "log10",
                          option = "plasma") +
    scale_size_continuous(name = "Weight", range = c(0.5, 2)) +
    scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
    custom_theme_oss() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#ffffff", colour = "#e0e0e0"),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
  #
  #
  #   # Compute means, standard error, and 95% CI for Income.group
  #   income_stats = collaborations |>
  #     group_by(Income.group) |>
  #     summarise(mean_collabs = mean(total_collabs, na.rm = TRUE),
  #               se = sd(total_collabs, na.rm = TRUE) / sqrt(n()),
  #               lower = mean_collabs - (1.96 * se),
  #               upper = mean_collabs + (1.96 * se))
  #
  #   # Compute means, standard error, and 95% CI for Region
  #   region_stats = collaborations |>
  #     group_by(Region) |>
  #     summarise(mean_collabs = mean(total_collabs, na.rm = TRUE),
  #               se = sd(total_collabs, na.rm = TRUE) / sqrt(n()),
  #               lower = mean_collabs - (1.96 * se),
  #               upper = mean_collabs + (1.96 * se))
  #
  #   # Plotting for Income.group
  #   bar1 = ggplot(income_stats, aes(x = Income.group, y = mean_collabs)) +
  #     geom_bar(stat = "identity", fill = "skyblue") +
  #     geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
  #     labs(title = "Mean Collaboration with 95% CI by Income Group",
  #          y = "Mean Collaborations") +
  #     custom_theme_oss()
  #
  #   # Plotting for Region
  #   bar2 = ggplot(region_stats, aes(x = Region, y = mean_collabs)) +
  #     geom_bar(stat = "identity", fill = "lightcoral") +
  #     geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
  #     labs(title = "Mean Collaboration with 95% CI by Region",
  #          y = "Mean Collaborations") +
  #     custom_theme_oss() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #
  #   ggarrange(
  #     map,          # Top plot (map)
  #     ggarrange(bar2, bar1, ncol = 2), # Bottom plots (bar graphs)
  #     nrow = 2,
  #     heights = c(2, 1) # 2/3 of the height allocated for the map, 1/3 for the bars
  #   )

}
plot_collab_network = function(data, type) {
  data_name = deparse(substitute(data))
  d = split_collab_matrix(data, type)
  d = as.data.frame(d)

  df_top = d |>
    arrange(desc(weight)) %>%
    mutate(
      country1 = str_replace(country1, "-.*", ""),
      country2 = str_replace(country2, "-.*", "")
    )
  top_collab = head(df_top,20)

  g = graph_from_data_frame(df_top, directed = FALSE)

  vop=indicies(g) %>% arrange(desc(Degree))
  # Calculate Bonacich power centrality
  V(g)$betweeness = igraph::betweenness(g)

  # Compute edge width based on the number of interactions (weight)
  E(g)$interactions = E(g)$weight

  # Extract names of top nodes for labels
  top_node_names = V(g)[order(-V(g)$betweeness)]$name

  # Calculate weighted mean for node size
  V(g)$weighted_mean = strength(g, mode = "all", weights = E(g)$weight)

  # Compute weighted degree centrality using strength function
  weighted_degree = strength(g)

  # Now, let's filter the graph for leading collaborations (e.g., top 10% edge weights)
  threshold_weight = quantile(E(g)$weight, 0.1)
  subg = subgraph.edges(g, E(g)[weight > threshold_weight], delete.vertices = TRUE)

  top_node_names = V(subg)[order(-V(subg)$betweeness)]$name

  # Conditional labels for the top 10 nodes based on weighted mean
  V(subg)$label = ifelse(V(subg)$name %in% top_node_names, V(subg)$name, "")


  gg = # Plot the graph using ggraph
    ggraph(subg, layout = 'kk') +
    geom_edge_link0(aes(width = interactions),
                    color = "coral",
                    alpha = 0.8) +
    geom_node_point(aes(size = weighted_mean, color = betweeness)) +
    geom_node_text(
      aes(label = name),
      vjust = 0,
      hjust = 0.3,
      check_overlap = TRUE,
      size = 9,
      fontface = "bold",
      color = "red",
      family = "Times New Roman"
    ) +
    scale_edge_width_continuous(range = c(0.5, 2), name = "Interactions") +
    scale_color_gradient(name = "Betweeness Centrality") +
    scale_size_continuous(
      range = c(2, 15),
      breaks = c(
        min(V(g)$weighted_mean),
        median(V(g)$weighted_mean),
        max(V(g)$weighted_mean)
      ),
      name = "Weighted Mean"
    ) +
    theme_classic(base_family = "Times New Roman") +
    custom_theme_oss_no_axis() +
    labs(title = paste(stringr::str_to_upper(data_name))) +
    theme(legend.position = "bottom")

  return(list(graph = gg, graph_stats = vop,top=top_collab))

}
plot_big_collab_network = function(data, type) {
  data_name = deparse(substitute(data))
  d = split_collab_matrix(data, type)
  d = as.data.frame(d)

  df_top = d |>
    arrange(desc(weight)) %>%
    mutate(
      country1 = str_replace(country1, "-.*", ""),
      country2 = str_replace(country2, "-.*", "")
    )


  top_collab = head(df_top,20)

  g = graph_from_data_frame(df_top, directed = FALSE)
  degree_values <- degree(g)
  hist(degree_values, main="Degree Distribution", xlab="Degree", ylab="Frequency", col="skyblue", border="black")




  vop=indicies(g) %>% arrange(desc(Degree))
  # Now, let's filter the graph for leading collaborations (e.g., top 10% edge weights)
  threshold_weight = quantile(E(g)$weight, 0.95)
  subg = subgraph.edges(g, E(g)[weight > threshold_weight], delete.vertices = TRUE)


  # Calculate Bonacich power centrality
  V(subg)$betweeness = igraph::betweenness(subg)

  # Compute edge width based on the number of interactions (weight)
  E(subg)$interactions = E(subg)$weight

  # Extract names of top nodes for labels
  top_node_names = V(subg)[order(-V(subg)$betweeness)]$name

  # Calculate weighted mean for node size
  V(subg)$weighted_mean = strength(subg, mode = "all", weights = E(subg)$weight)

  # Compute weighted degree centrality using strength function
  weighted_degree = strength(subg)
  top_node_names = V(subg)[order(-V(subg)$betweeness)]$name

  # Conditional labels for the top 10 nodes based on weighted mean
  V(subg)$label = ifelse(V(subg)$name %in% top_node_names, V(subg)$name, "")


  gg = # Plot the graph using ggraph
    ggraph(subg, layout = 'lgl') +
    geom_edge_link0(aes(width = interactions),
                    color = "coral",
                    alpha = 0.8) +
    geom_node_point(aes(size = weighted_mean, color = betweeness)) +
    geom_node_text(
      aes(label = name),
      vjust = 0,
      hjust = 0.3,
      check_overlap = TRUE,
      size = 3.5,
      fontface = "bold",
      color = "purple",
      fontface = "bold",
      family = "Times New Roman"
    ) +
    scale_edge_width_continuous(range = c(0.5, 2), name = "Interactions") +
    scale_color_viridis_c(name = "Betweeness Centrality") +
    scale_size_continuous(
      range = c(2, 15),
      breaks = c(
        min(V(g)$weighted_mean),
        median(V(g)$weighted_mean),
        max(V(g)$weighted_mean)
      ),
      name = "Weighted Mean"
    ) +
    theme_classic(base_family = "Times New Roman") +
    custom_theme_oss_no_axis() +
    labs(title = paste(stringr::str_to_upper(data_name))) +
    theme(legend.position = "bottom")

  return(list(graph = gg, graph_stats = vop, top = top_collab))

}

custom_theme_oss_no_axis = function() {
  theme_minimal(base_family = "Times") +
    theme(
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(color = "black", size = 20),
      plot.subtitle = element_text(color = "grey50", size = 14),
      plot.caption = element_text(color = "grey50"),
      axis.title.x = element_blank(),
      # Remove axis title
      axis.title.y = element_blank(),
      # Remove axis title
      axis.text.x = element_blank(),
      # Remove axis text
      axis.text.y = element_blank(),
      # Remove axis text
      axis.ticks = element_blank(),
      # Removing as axis text is also blank
      axis.line = element_blank(),
      # Removing as axis text is also blank
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      legend.background = element_rect(fill = "white"),
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "grey70", size = 12)
    )
}
custom_theme_oss = function() {
  theme_minimal(base_family = "Times") +
    theme(
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(color = "black", size = 20),
      plot.subtitle = element_text(color = "black", size = 14),
      plot.caption = element_text(color = "black"),
      axis.title.x = element_text(color = "black", size = 12),
      axis.title.y = element_text(color = "black", size = 12),
      axis.text.x = element_text(color = "black", size = 12),
      axis.text.y = element_text(color = "black", size = 12),
      axis.ticks = element_line(color = "grey80"),
      axis.line = element_line(color = "grey80"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      legend.background = element_rect(fill = "white"),
      legend.text = element_text(color = "black", size = 12),
      legend.title = element_text(color = "black", size = 14)
    )
}

generate_and_save_plots = function(data, data_name) {
  # Initialize a data frame to store all indices
  all_indices = data.frame()

  # Define columns to analyze
  columns_to_analyze = names(data)

  # Generate individual plots and indices based on the given data frame
  plots = list()
  for (column_name in columns_to_analyze) {
    plot_data = plot_colbs_graph(data, column_name)
    plots[[column_name]] = plot_data$graph

    # Add a "DataName" and "ColumnName" column to identify the data frame and column
    tmp_indices =
      cbind(data.frame(DataName = data_name, ColumnName = column_name),
            plot_data$indices)
    all_indices = rbind(all_indices, tmp_indices)
  }

  # Arrange plots dynamically based on the number of columns
  plot_list = lapply(columns_to_analyze, function(x)
    plots[[x]])
  n_cols =
    ifelse(length(columns_to_analyze) <= 3,
           length(columns_to_analyze),
           3)
  n_rows = ceiling(length(columns_to_analyze) / n_cols)

  pop =
    do.call(ggarrange, c(plot_list, list(nrow = n_rows, ncol = n_cols)))

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
  write.csv(all_indices,
            paste0("indices_", data_name, ".csv"),
            row.names = FALSE)
}
round_df = function(df, digits) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] = round(df[, nums], digits = digits)

  (df)
}
indicies = function(g) {
  # Number of vertices
  n = igraph::vcount(g)

  # Pre-allocate the result dataframe
  res = data.frame(
    Name = character(n),
    Degree = numeric(n),
    Betweenness = numeric(n),
    Closeness = numeric(n),
    EigenVector = numeric(n),
    PageRank = numeric(n),
    PowerCentrality = numeric(n)
  )

  # Fill the dataframe
  res$Name = V(g)$name
  res$Degree = igraph::degree(g)
  res$Betweenness = igraph::betweenness(g)
  res$Closeness = igraph::closeness(g)
  res$EigenVector = igraph::evcent(g)$vector
  res$PageRank = igraph::page_rank(g, directed = TRUE)$vector
  res$PowerCentrality = power_centrality(g, exponent = -2, rescale = TRUE)

  # Round the dataframe values
  res[,-1] = round(res[,-1], 6)

  return(res)
}
centrality_measures = function(data,type){
  d = split_collab_matrix(data, type)
  d = as.data.frame(d)

  g = graph_from_data_frame(d, directed = FALSE)

  vop=indicies(g)

  vop = vop |>
    arrange(desc(Degree))

  return(head(vop,20))
}
author_network = function(data){

  data_name = deparse(substitute(data))
  d = split_collab_matrix(data, "Authors")
  d_top = d |> arrange(desc(weight))
  # Subset to get top 10 collaborations
  top10 = d_top[order(-d_top$weight), ][1:20, ]
  # Calculate order outside the plotting function
  top10$order1 = as.factor(seq(1, nrow(top10)))
  top10$order2 = as.factor(seq(1, nrow(top10)))

  # Create the plot using this order
  p = ggplot(top10, aes(x = 2, y = order1)) +

    # Connect countries with lines, size represents weight
    geom_segment(aes(xend = 3, yend = order2, size = weight), color = "#009E73", alpha = 0.6) +

    # Add country names as text slightly left of center
    geom_text(aes(label = country1, x = 3), hjust = "right", fontface = "bold", size = 5.5, color = "#0072B2") +

    # Add country names as text slightly right of center
    geom_text(aes(label = country2, x = 2), hjust = "left", fontface = "bold", size = 5.5, color = "#D55E00") +

    # Add weight numbers directly in the center
    geom_text(aes(label = weight, x = 2.5, y = (as.numeric(order1) + as.numeric(order2))/2),
              color = "red", vjust = 1.5, size = 5.5, fontface = "italic") +

    # Set visual themes and aesthetics
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.background = element_rect(fill = "#F0F0F0")
    ) +

    labs(title = paste0("Top 20 Author Collaborations ", stringr::str_to_upper(data_name))) +
    scale_size_continuous(range = c(1,3))

  # Print the plot
  p


}

count_papers_by_authors <- function(df) {
  # Calculate the number of authors per paper
  df <- df %>%
    mutate(num_authors = sapply(strsplit(as.character(Authors), ";"), length))

  # Categorize based on the number of authors
  df <- df %>%
    mutate(category = case_when(
      num_authors == 1 ~ "1",
      num_authors >= 2 & num_authors <= 5 ~ "2-5",
      num_authors > 5 & num_authors <= 10 ~ "5-10",
      num_authors >= 10 & num_authors <= 20 ~ "10-20",
      num_authors >= 20 & num_authors <= 50 ~ "20-50",
      num_authors >= 50 & num_authors <= 100 ~ "50-100",
      num_authors >= 100 & num_authors <= 250 ~ "100-250",
      num_authors >= 250 & num_authors <= 500 ~ "250-500",
      num_authors >= 500 & num_authors <= 1000 ~ "500-1000",
      num_authors >= 1000 & num_authors <= 5000 ~ "1000-5000",
      TRUE ~ NA_character_
    ))

  # Count the number of papers in each category, broken down by year
  result <- df %>%
    group_by(PubYear, category) %>%
    summarize(count = n(), .groups = 'drop')

  # Pivot the dataframe to make it wide
  wide_result <- result %>%
    pivot_wider(names_from = category, values_from = count, values_fill = list(count = 0))

  return(wide_result)
}
plot_country_collab = function(data){
  type = "Country.of.standardized.research.organization"
  data_name = deparse(substitute(data))
  # Convert country names to ISO 3166-1 alpha-3 codes
  d = split_collab_matrix(data, type)
  top_collab = d %>% arrange(desc(weight))

  d$country1 <-
    gsub("(?<=\\p{Lu})(?=\\p{Lu}|$)", " ", d$country1, perl = TRUE)
  d$country2 <-
    gsub("(?<=\\p{Lu})(?=\\p{Lu}|$)", " ", d$country2, perl = TRUE)


  d$country1_code <- countrycode(d$country1, "country.name", "iso3c")
  d$country2_code <- countrycode(d$country2, "country.name", "iso3c")



  # Get the 'world' data
  world_data <-
    rnaturalearth::ne_countries(scale = "medium",
                                type = "countries",
                                returnclass = "sf")

  # Extract the iso_a3, name and geometry data
  spatial_data <- world_data %>%
    select(iso_a3, name)


  sf::sf_use_s2(FALSE)
  # Compute the centroids for each country
  centroids <- st_centroid(st_geometry(world_data))
  spatial_data$long <- st_coordinates(centroids)[, 1]
  spatial_data$lat <- st_coordinates(centroids)[, 2]


  # Join to get coordinates for country1
  df_with_coords <- d %>%
    left_join(spatial_data, by = c("country1_code" = "iso_a3")) %>%
    rename(country1_long = long, country1_lat = lat)

  # Join to get coordinates for country2
  df_with_coords <- df_with_coords %>%
    left_join(spatial_data, by = c("country2_code" = "iso_a3")) %>%
    rename(country2_long = long, country2_lat = lat)

  # Calculate total weights for country1_code
  collaborations_country1 <- df_with_coords %>%
    group_by(country1_code) %>%
    summarise(weight1 = sum(weight)) %>%
    ungroup()

  # Calculate total weights for country2_code
  collaborations_country2 <- df_with_coords %>%
    group_by(country2_code) %>%
    summarise(weight2 = sum(weight)) %>%
    ungroup()

  # Join and calculate total collaborations
  total_collaborations <- collaborations_country1 %>%
    full_join(collaborations_country2,
              by = c("country1_code" = "country2_code")) %>%
    mutate(total_weight = ifelse(is.na(weight1), 0, weight1) +
             ifelse(is.na(weight2), 0, weight2)) %>%
    select(country_code = country1_code, total_weight)

  total_collaborations = na.omit(total_collaborations)

  # Merge total_collaborations with spatial_data
  spatial_data <- spatial_data %>%
    left_join(total_collaborations, by = c("iso_a3" = "country_code"))

  # Sort countries by collaboration count and extract the top 10
  top_countries <- spatial_data %>%
    arrange(-total_weight) %>%
    head(10) %>%
    pull(iso_a3)

  # Flag for top 10 countries in spatial_data
  spatial_data$top_country <-
    ifelse(spatial_data$iso_a3 %in% top_countries, TRUE, FALSE)
  spatial_data = na.omit(spatial_data)
  # Map
  # Map
  map_plot <- ggplot() +
    # Base map with country fill based on collaboration count
    geom_sf(
      data = spatial_data, # Change from world_data to spatial_data
      aes(fill = total_weight),
      color = "gray90",
      size = 0.2
    ) +

    # Flow lines
    geom_segment(
      data = df_with_coords,
      aes(
        x = country1_long,
        y = country1_lat,
        xend = country2_long,
        yend = country2_lat,
        color = weight
      ),
      lineend = "round",
      alpha = 0.1
    ) +
    # Color and other scales
    scale_fill_viridis_c(name = "Total Collaborations", trans = "log10") +
    scale_color_gradient(high = "red", low = "coral", name = "Flow Weight", trans = "log10") +
    scale_size_continuous(range = c(0.5, 3), name = "Flow Weight") +

    # Theme and labels
    theme_void() +
    labs(title = paste0(stringr::str_to_upper(data_name)))+

    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )

  # Display the map
  print(map_plot)


  ggsave(
    paste0("imgs/" ,data_name,"country-plot.png"),
    dpi = 320,
    width = 14,
    height = 14,
    limitsize = FALSE
  )

  return(top_collab)

}
remove_duplicate = function(data) {
  # Keep only the first occurrence of each title and remove the duplicates
  data_cleaned = data[!duplicated(data$Title), ]
  data_cleaned = data_cleaned[!duplicated(data_cleaned$Publication.ID), ]
  data_cleaned = data_cleaned[!duplicated(data_cleaned$DOI), ]
  return(data_cleaned)
}
remove_empty_strings <- function(data) {
  # Remove rows where specified columns have empty strings
  data_cleaned <- data[!(data$Country.of.standardized.research.organization == ""), ]

  return(data_cleaned)
}
clean_duplicates <- function(authors_string) {
  # Split and trim whitespace
  authors_list <- unlist(strsplit(authors_string, ";"))
  authors_list <- trimws(authors_list)

  # Remove duplicates
  unique_authors <- unique(authors_list)

  # Combine back to a single string
  return(paste(unique_authors, collapse = "; "))
}

calculate_percentage_collaboration = function(df) {
    # Calculate the total articles across all collaborations
    total_articles <- sum(df$Articles, na.rm = TRUE)

    # Summarize the dataframe by collaboration type and calculate the percentage and total count
    collaboration_summary <- df %>%
      group_by(collaboration) %>%
      summarize(total_articles_by_collab = sum(Articles, na.rm = TRUE)) %>%
      mutate(percentage = (total_articles_by_collab / total_articles) * 100) %>%
      ungroup() %>%
      select(collaboration, total_articles_by_collab, percentage)

    return(collaboration_summary)
}

calculate_total_articles = function(df) {
  # Summarize the dataframe by collaboration type
  summarized_df <- df %>%
    group_by(collaboration) %>%
    summarize(total_articles = sum(Articles, na.rm = TRUE)) %>%
    ungroup()

  # Reshape the dataframe to have collaboration types as columns
  reshaped_df <- summarized_df %>%
    spread(key = collaboration, value = total_articles) %>%
    select(`Single-region`, `Multi-region`)

  return(reshaped_df)
}
plot_income_colab = function(data){

  data_name = deparse(substitute(data))

  # Calculate collaboration based on Income.group
  data_processed = data %>%
    mutate(Country = str_split(Country.of.standardized.research.organization, ";")) %>%
    unnest(Country) %>%
    mutate(Country = str_trim(Country)) %>%
    filter(Country != "", !is.na(Country)) %>%  # Remove empty or NA countries
    inner_join(class, by = c("Country" = "Economy")) %>%
    group_by(Country.of.standardized.research.organization) %>%
    filter(length(unique(Country)) > 1) %>%  # Filter out internal collaborations
    mutate(collaboration = case_when(
      all(Income.group %in% c("High income")) ~ "HI",
      all(Income.group %in% c("High income", "Upper middle income")) ~ "HI-UMI",
      all(Income.group %in% c("High income", "Lower middle income")) ~ "HI-LMI",
      all(Income.group %in% c("High income", "Low income")) ~ "HI-LI",
      "High income" %in% Income.group ~ "HI-MIX",
      all(Income.group %in% c("Upper middle income", "Lower middle income")) ~ "UMI-LMI",
      all(Income.group %in% c("Upper middle income", "Low income")) ~ "UMI-LI",
      "Upper middle income" %in% Income.group ~ "UMI-MIX",
      all(Income.group %in% c("Lower middle income", "Low income")) ~ "LMI-LI",
      TRUE ~ "OTHER"
    )) %>%
    ungroup() %>%
    group_by(Country, collaboration) %>%
    summarize(Articles = n(), .groups = "drop")

  # Filter out the 'OTHER' (Optional, but removes the unnecessary category)
  data_processed <- filter(data_processed, collaboration != "OTHER")

  incomestats = data_processed %>%
    group_by(Country) %>%
    mutate(Percent = Articles / sum(Articles) * 100) %>%
    ungroup()

  # Get top 20 countries
  top20 = data_processed %>%
    group_by(Country) %>%
    summarize(Total = sum(Articles)) %>%
    top_n(50, Total)

  data_final = filter(data_processed, Country %in% top20$Country)

  # Plot
  # Reorder the Country factor levels within the data frame
  data_final$Country <- factor(data_final$Country, levels = data_final %>%
                                 group_by(Country) %>%
                                 summarize(Total = sum(Articles)) %>%
                                 arrange(-Total) %>%
                                 pull(Country))

  # Plot
  p = ggplot(data_final, aes(fill = collaboration, y = Articles, x = Country)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = paste(stringr::str_to_upper(data_name)),
         xlab("Country (Ordered by Total Document Count)"),
         ylab("Number of Documents")) +
    coord_flip() +
    theme(axis.text  = element_text(colour = "black", size = 17, face = "bold"),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 14)) +
    custom_theme_oss() +
    scale_fill_discrete(drop = FALSE, name = "Income-Group Collaboration")


  return(list(incomestats,p))
}
