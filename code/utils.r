## ---------------------------
##
## Script name: utils.r
##
## Purpose of script: load utility functions
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-04-28
##
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

##################################################################
##                       Helper Functions                       ## thanx for helpin
##################################################################

# pacman - library management ---------------------------------------------
#' Takes a list of packages and installs and loads them in parrael
#'
#' @param pkg A list of packages
#' @examples
#' packman(pkg)
#'
pacman = function(pkg) {
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# fetch data - quick api tool ---------------------------------------------
#' Downloads data to a defined path
#'
#' @param url A url to download data from
#' @param path A path on the local machine to save the file
#' @examples
#' fetch_data(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",path = "data/covid-jh.csv")
#'
fetch_data = function(url, path) {
  url = url
  path = path
  download.file(url, path)
  read.csv(path)
}

# isodate- week to year function ------------------------------------------
#' Formats a date from week and year to iso format
#'
#' @examples
#' isodate("22/11")
#'
isodate = function (x = Sys.Date()) {
  xday = ISOdate(year(x), month(x), day(x), tz = tz(x))
  dn = 1 + (wday(x) + 5) %% 7
  nth = xday + ddays(4 - dn)
  jan1 = ISOdate(year(nth), 1, 1, tz = tz(x))
  return(sprintf("%s/%02d", format(nth, "%y"), 1 + (nth - jan1) %/% ddays(7)))
}

# tree plot - ggplot2 theme -----------------------------------------------
#' Tree plot ggplot2 theme
#'
#' @examples
#' + theme_tree()
#'
theme_tree = function() {
  theme(
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 14
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 6,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      vjust = 0.2,
      colour = textcol,
      size = 14
    ),
    axis.ticks = element_line(size = 0.4),
    plot.caption = element_text(colour = textcol, size = 10),
    axis.title = element_text(
      size = 14,
      face = "bold",
      colour = textcol,
      hjust = 0.1
    ),
    panel.border = element_blank(),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 0.5,
      linetype = "solid"
    ),
    panel.grid.major = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 22,
      face = "bold",
      vjust = 0.9
    )
  )
}
theme_reg = function() {
  theme(
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 14
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 6,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      vjust = 0.2,
      colour = textcol,
      size = 14
    ),
    axis.ticks = element_line(size = 0.4),
    plot.caption = element_text(colour = textcol, size = 10),
    axis.title = element_text(
      size = 14,
      face = "bold",
      colour = textcol,
      hjust = 0.1
    ),
    panel.border = element_blank(),
    panel.background = element_rect(
      fill = "white",
      colour = "white",
      size = 0.5,
      linetype = "solid"
    ),
    panel.grid.major = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 22,
      face = "bold",
      vjust = 0.9
    )
  )
}

# K-elbow plot - ggplot2 theme -------------------------------------------
#' K-elbow plot ggplot2 theme
#'
#' @examples
#' + theme_temporal()
#'
theme_k = function() {
  theme(
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 14
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
      face = "bold"
    ),
    legend.key.height = grid::unit(0.8, "cm"),
    legend.key.width = grid::unit(0.2, "cm"),
    axis.text.x = element_text(
      size = 14,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      vjust = 0.2,
      colour = textcol,
      size = 14
    ),
    axis.ticks = element_line(size = 0.4),
    plot.caption = element_text(colour = textcol, size = 10),
    axis.title = element_text(
      size = 12,
      face = "bold",
      colour = textcol,
      hjust = 0.1
    ),
    panel.border = element_blank(),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 0.5,
      linetype = "solid"
    ),
    plot.background = element_rect(fill = "gray12"),
    legend.background = element_rect(fill = "gray12"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
  )
}

# temporal plot - ggplot2 theme -------------------------------------------
#' Temporal plot ggplot2 theme
#'
#' @examples
#' + theme_temporal()
#'
theme_temporal = function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 14
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
      face = "bold"
    ),
    legend.key.height = grid::unit(1.4, "cm"),
    legend.key.width = grid::unit(0.8, "cm"),
    axis.text.x = element_text(
      size = 6,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      vjust = 0.2,
      colour = textcol,
      size = 14
    ),
    axis.ticks = element_line(size = 0.4),
    plot.caption = element_text(colour = textcol, size = 10),
    axis.title = element_text(
      size = 12,
      face = "bold",
      colour = textcol,
      hjust = 0.1
    ),
    panel.border = element_blank(),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 0.5,
      linetype = "solid"
    ),
    panel.grid.major = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "gray12"),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
  )
}

# landscape plot - ggplot2 theme ------------------------------------------
#' Landscape plot ggplot2 theme
#'
#' @examples
#' + theme_landscape()
#'
theme_landscape = function() {
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 18
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 18,
      face = "bold"
    ),
    legend.key.height = grid::unit(1.4, "cm"),
    legend.key.width = grid::unit(0.8, "cm"),
    axis.text.x = element_text(
      size = 12,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      hjust = 0,
      colour = textcol,
      size = 8
    ),
    axis.ticks = element_line(size = 0.4),
    plot.caption = element_text(colour = textcol, size = 10),
    axis.title = element_text(
      size = 12,
      face = "bold",
      colour = textcol,
      hjust = 0.1
    ),
    panel.border = element_blank(),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 0.5,
      linetype = "solid"
    ),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
  )
}

# network_stat_df - generate network stats --------------------------------
#' Create a dataframe of network statistics of a given graph
#'
#' @param network A igraph network object
#' @examples
#' network_stat_df(author_colab)
#'
network_stat_df = function(network) {
  v = data.frame(
    "size" = network$network$networkSize,
    "density" = network$network$networkDensity,
    "transitivity" = network$network$networkTransitivity,
    "diameter" = network$network$networkDiameter,
    "distance" = network$network$networkCentrDegree,
    "avgpath" = network$network$NetworkAverPathLeng
  )
  return(v)
}

# split_author_matrix = Create colab matrix -------------------------------
#' Create a dataframe of network statistics of a given graph
#'
#' @param network A igraph network object
#' @examples
#' network_stat_df(author_colab)
#'
split_author_matrix = function(data, col_name) {
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
  colnames(auth_count) = c("datum1", "datum2", "weight")

  return(auth_count)
}

# build_networks - create data files for network plots --------------------
#' Save graphs for author,geography and institution colab and author,keyword,abstract,title co-occurrence
#'
#' @param data A biblographic dataframe
#' @param path A path save files
#' @examples
#' build_networks(M, "data/networks/gisaid/")
#'
build_networks = function(data, path) {
  message("Building collaboration networks")
  ##################################################################
  ##                    Collaboration Networks                    ##
  ##################################################################

  # generate network
  author_colab = biblioNetwork(M,
                               analysis = "collaboration",
                               network = "authors",
                               sep = ";")
  message("Author network complete")
  institution_colab = biblioNetwork(M,
                                    analysis = "collaboration",
                                    network = "universities",
                                    sep = ";")
  message("University network complete")
  geog_colab = biblioNetwork(M,
                             analysis = "collaboration",
                             network = "countries",
                             sep = ";")
  message("Geography network complete")
  institution_co_oc = split_author_matrix("Authors") |> igraph::graph_from_data_frame()
  message("Institution network complete")
  funding_co_oc = split_author_matrix("FU") |> igraph::graph_from_data_frame()
  message("Funding network complete")
  funding_group_co_oc = split_author_matrix("Funder.Group") |> igraph::graph_from_data_frame()
  message("Funding group network complete")
  funding_country_co_oc = split_author_matrix("Funder.Country") |> igraph::graph_from_data_frame()
  message("Funding country network complete")

  saveRDS(author_colab, paste0(path, "author_colab.rds"))
  saveRDS(institution_colab, paste0(path, "institution_colab.rds"))
  saveRDS(geog_colab, paste0(path, "geog_colab.rds"))
  saveRDS(institution_co_oc, paste0(path, "institution_co_ocs.rds"))
  saveRDS(funding_co_oc, paste0(path, "funding_co_ocs.rds"))
  saveRDS(funding_group_co_oc,
          paste0(path, "funding_group_co_ocs.rds"))
  saveRDS(funding_country_co_oc,
          paste0(path, "funding_country_keywords_co_ocs.rds"))

  # calculate network statistics
  message("Calculating colab network stats")
  author_colab_stats = networkStat(author_colab) |> network_stat_df()
  institution_colab_stats = networkStat(institution_colab) |> network_stat_df()
  geog_colab_stats = networkStat(geog_colab) |> network_stat_df()
  inititution_co_ocs_stats = networkStat(institution_co_oc) |> network_stat_df()
  funding_co_ocs_stats = networkStat(funding_co_oc) |> network_stat_df()
  funding_group_co_ocs_stats = networkStat(funding_group_co_oc) |> network_stat_df()
  funding_country_keywords_co_ocs_stats = networkStat(funding_country_co_oc) |> network_stat_df()

  category = c(
    "author",
    "institution",
    "geography",
    "insitution",
    "funding",
    "funding-group",
    "funding country"
  )

  colab_stats = rbind(
    author_colab_stats,
    institution_colab_stats,
    geog_colab_stats,
    inititution_co_ocs_stats,
    funding_co_ocs_stats,
    funding_group_co_ocs_stats,
    funding_country_keywords_co_ocs_stats
  ) |>
    dplyr::mutate(category = category)

  saveRDS(colab_stats,
          paste0(path, "colab_network_stats.rds"))
  # remove redundant vars
  rm(author_colab_stats,
     institution_colab_stats,
     geog_colab_stats)

  #################################################################
  ##                   Co-occurrences Networks                   ##
  #################################################################
  message("Building co-occurrences networks")
  author_co_ocs = biblioNetwork(M,
                                analysis = "co-occurrences",
                                network = "authors",
                                sep = ";")
  message("Author network complete")
  journals_co_ocs = biblioNetwork(M,
                                  analysis = "co-occurrences",
                                  network = "sources",
                                  sep = ";")
  message("Journal network complete")
  keywords_co_ocs = biblioNetwork(M,
                                  analysis = "co-occurrences",
                                  network = "keywords",
                                  sep = ";")
  message("Keyword network complete")
  author_keywords_co_ocs = biblioNetwork(M,
                                         analysis = "co-occurrences",
                                         network = "author_keywords",
                                         sep = ";")
  message("Author keyword network complete")
  saveRDS(author_co_ocs, paste0(path, "author_co_ocs.rds"))
  saveRDS(journals_co_ocs, paste0(path, "journals_co_ocs.rds"))
  saveRDS(keywords_co_ocs, paste0(path, "keywords_co_ocs.rds"))
  saveRDS(author_keywords_co_ocs,
          paste0(path, "author_keywords_co_ocs.rds"))

  # calculate network statistics
  message("Calculating colab network stats")
  author_co_ocs_stats = networkStat(author_co_ocs) |> network_stat_df()
  journals_co_ocs_stats = networkStat(journals_co_ocs) |> network_stat_df()
  keywords_co_ocs_stats = networkStat(keywords_co_ocs) |> network_stat_df()
  author_keywords_co_ocs_stats = networkStat(author_keywords_co_ocs) |> network_stat_df()

  category = c("author", "journal", "keywords", "autho-keywords")

  co_oc_stats = rbind(
    author_co_ocs_stats,
    journals_co_ocs_stats,
    keywords_co_ocs_stats,
    author_keywords_co_ocs_stats
  ) |>
    dplyr::mutate(category = category)

  saveRDS(co_oc_stats, paste0(path, "co_oc_network_stats.rds"))
  # remove redundant vars
  rm(
    author_co_ocs_stats,
    journals_co_ocs_stats,
    keywords_co_ocs_stats,
    author_keywords_co_ocs_stats
  )
}

# Plot biblometrix using VOSViewer ----------------------------------------
#' Plot igraph collaboration network using VOSViewer
#'
#' @param network A igraph network object
#' @param vos A path to VOS on local machine
#' @examples
#' plot_colab_network(author_colab, "../VosViewer)
#'
plot_colab_network = function(network, vos) {
  # plot in igraph
  net_author = networkPlot(
    network,
    n = 50,
    type = "fruchterman",
    size = 10,
    size.cex = T,
    halo = TRUE,
    edgesize = 3,
    labelsize = 1
  )
  # plot in vosviewer
  ## Repulsion 0, attraction 10 OR -1, 1, method: strength link, font: sans serif
  if (vos == TRUE) {
    net2VOSviewer(net_author, vos.path = "../VOSViewer/")
  } else {
    print("Network plotted!")
  }
}

# Plot biblometrix using VOSViewer ----------------------------------------
#' Plot igraph collaboration network using VOSViewer
#'
#' @param net A igraph network object
#' @param vos.path A path to VOS on local machine
#' @examples
#' plot_colab_network(author_colab, "../VosViewer)
#'
#'@details
#'Edited from the biblometrix package with the first line removed in order to allow igraph functionlity

viewerigraph = function(net, vos.path = NULL) {
  V(net)$id = V(net)$name

  if (is.null(vos.path)) {
    vos.path = getwd()
  }
  if (sum(dir(vos.path) %in% "VOSviewer.jar") == 0) {
    cat(
      paste(
        "VOSviewer.jar does not exist in the path",
        vos.path,
        "\n\nPlese download it from https://www.vosviewer.com/download",
        "\n(Java version for other systems)\n"
      )
    )
  }
  else{
    netfile = paste(vos.path, "/", "vosnetwork.net", sep = "")
    VOScommand = paste("java -jar ",
                       vos.path,
                       "/",
                       "VOSviewer.jar -pajek_network ",
                       netfile,
                       sep = "")
    write.graph(graph = net,
                file = netfile,
                format = "pajek")
    system(VOScommand, wait = FALSE)
  }

}


# Round DF----------------------------------------
#' Round all numeric values in dataframe by specified amount
#'
#' @param df a dataframe
#' @param digits the number to round by
#' @examples
#' round_df(df)
#'
round_df = function(df, digits) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] = round(df[, nums], digits = digits)

  (df)
}

# Plot MCP and SCP----------------------------------------
#' Rlot country distribution of multi cited papers vs single cited papers
#'
#' @param data a dataframe
#' @param db the name of the database to save files for
#' @examples
#' plot_mcp_scp(S,"GISAID)
#'
plot_mcp_scp = function(data, db) {
  Country = S$MostProdCountries$Country
  SCP = S$MostProdCountries$SCP
  MCP = S$MostProdCountries$MCP
  Articles = S$MostProdCountries$Articles

  dataa = as.data.frame(cbind(Country, SCP, MCP, Articles)) |>
    arrange(desc(Articles)) |>
    pivot_longer(c(SCP, MCP)) |>
    mutate(value = as.numeric(value)) |>
    mutate(Articles = as.numeric(Articles)) |>
    mutate(collaboration = name)

  cv = ggplot(data |>   {
    \(.) {
      replace(., is.na(.), 0)
    }
  }(), aes(
    fill = collaboration,
    y = value,
    x = reorder(Country, Articles)
  )) +
    geom_bar(position = "stack", stat = "identity") +
    labs(
      title = paste0(
        "Leading 20 countries mentioning CV19DP",
        "\nin scientific publications"
      ),
      caption  = "Publications containing the search query ‘The Covid-19 Data Portal’ OR 'European Nucleotide Archive' \nwere accessed via the Dimensions Analytics API and filtered to include publications between January 1st 2019 \nand October 1st 2021 which contain the phrase 'covid-19' OR 'sars-cov-2' in the full text.\nSCP: Single Country Publication. MCP: Multi Country Publication"
    ) +
    xlab("Country") +
    ylab("No. Documents") +
    coord_flip()  + theme(axis.text  = element_text(
      colour = textcol,
      size = 17,
      face = "bold"
    )) + theme_landscape() + theme(axis.text.y = element_text(size = 16),
                                   axis.text.x = element_text(size = 14))

  ggsave(
    paste0("reg.png"),
    dpi = 320,
    width = 16,
    height = 12,
    limitsize = FALSE
  )
}
# Calculate network statistics from igraph----------------------------------------
#' Plot igraph collaboration network using VOSViewer
#'
#' @param g An igraph network object
#' @examples
#' all_indices(g)
#'
all_indices =  function(g) {
  res = matrix(0, igraph::vcount(g), 35)
  res[, 1] = V(g)$name
  res[, 2] = as.numeric(igraph::degree(g))
  res[, 3] = as.numeric(igraph::betweenness(g))
  res[, 4] = as.numeric(igraph::closeness(g))
  res[, 5] = as.numeric(igraph::eigen_centrality(g)$vector)
  res[, 6] = as.numeric(1 / igraph::eccentricity(g))
  res[, 7] = as.numeric(igraph::subgraph_centrality(g))

  A = get.adjacency(g, sparse = F)
  res[, 8] = as.numeric(sna::flowbet(A))
  res[, 9] = as.numeric(sna::loadcent(A))
  res[, 10] = as.numeric(sna::gilschmidt(A))
  res[, 11] = as.numeric(sna::infocent(A))
  res[, 12] = as.numeric(sna::stresscent(A))
  res[, 13] = as.numeric(1 / centiserve::averagedis(g))
  res[, 14] = as.numeric(centiserve::barycenter(g))
  res[, 15] = as.numeric(centiserve::closeness.currentflow(g))
  res[, 16] = as.numeric(centiserve::closeness.latora(g))
  res[, 17] = as.numeric(centiserve::closeness.residual(g))
  res[, 18] = as.numeric(centiserve::communibet(g))
  res[, 19] = as.numeric(centiserve::crossclique(g))
  res[, 20] = as.numeric(centiserve::decay(g))
  res[, 21] = as.numeric(centiserve::diffusion.degree(g))
  res[, 22] = as.numeric(1 / centiserve::entropy(g))
  res[, 23] = as.numeric(centiserve::geokpath(g))
  res[, 24] = as.numeric(centiserve::laplacian(g))
  res[, 25] = as.numeric(centiserve::leverage(g))
  res[, 26] = as.numeric(centiserve::lincent(g))
  res[, 27] = as.numeric(centiserve::lobby(g))
  res[, 28] = as.numeric(centiserve::markovcent(g))
  res[, 29] = as.numeric(centiserve::mnc(g))
  res[, 20] = as.numeric(centiserve::radiality(g))
  res[, 31] = as.numeric(centiserve::semilocal(g))
  res[, 32] = as.numeric(1 / centiserve::topocoefficient(g))
  res[, 33] = as.numeric(CINNA::dangalchev_closeness_centrality(g))
  res[, 34] = as.numeric(CINNA::harmonic_centrality(g))
  res[, 35] = as.numeric(1 / CINNA::local_bridging_centrality(g))
  res = as.data.frame(res)
  res[, c(2:35)] = sapply(res[, c(2:35)], as.numeric)
  res = round_df(res, 3)
  colnames(res) = c(
    "name",
    "degree",
    "betweenness",
    "closeness",
    "eigen_centrality",
    "eccentricity",
    "subgraph_centrality",
    "flowbet",
    "loadcent",
    "gilschmidt",
    "infocent",
    "stresscent",
    "averagedis",
    "barycenter",
    "closeness.currentflow",
    "closeness.latora",
    "closeness.residual",
    "communibet",
    "crossclique",
    "decay",
    "diffusion.degree",
    "entropy",
    "geokpath",
    "laplacian",
    "leverage",
    "lincent",
    "lobby",
    "markovcent",
    "mnc",
    "radiality",
    "semilocal",
    "topocoefficient",
    "dangalchev_closeness_centrality",
    "harmonic_centrality",
    "local_bridging_centrality"
  )
  return(res)

}
few_indicies = function(g) {
  res = matrix(0, igraph::vcount(g), 5)
  res[, 1] = V(g)$name
  res[, 2] = as.numeric(igraph::degree(g))
  res[, 3] = as.numeric(igraph::betweenness(g))
  res[, 4] = as.numeric(igraph::closeness(g))
  res[, 5] = as.numeric(igraph::eigen_centrality(g)$vector)

  colnames(res) = c("name",
                    "degree",
                    "betweenness",
                    "closeness",
                    "eigen_centrality")
  return(res)
}

some_indicies = function(g) {
  res = matrix(0, igraph::vcount(g), 12)
  res[, 1] = V(g)$name
  res[, 2] = as.numeric(igraph::degree(g))
  res[, 3] = as.numeric(igraph::betweenness(g))
  res[, 4] = as.numeric(igraph::closeness(g))
  res[, 5] = as.numeric(igraph::eigen_centrality(g)$vector)
  res[, 6] = as.numeric(1 / igraph::eccentricity(g))
  res[, 7] = as.numeric(igraph::subgraph_centrality(g))

  A = get.adjacency(g, sparse = F)
  res[, 8] = as.numeric(sna::flowbet(A))
  res[, 9] = as.numeric(sna::loadcent(A))
  res[, 10] = as.numeric(sna::gilschmidt(A))
  res[, 11] = as.numeric(sna::infocent(A))
  res[, 12] = as.numeric(sna::stresscent(A))
  res = as.data.frame(res)
  res[, c(2:12)] = sapply(res[, c(2:12)], as.numeric)
  res = round_df(res, 3)
  colnames(res) = c(
    "name",
    "degree",
    "betweenness",
    "closeness",
    "eigen_centrality",
    "eccentricity",
    "subgraph_centrality",
    "flowbet",
    "loadcent",
    "gilschmidt",
    "infocent",
    "stresscent"
  )
  return(res)
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



# Network Functions -------------------------------------------------------

#' Convert biblography into dataframe
#'
#' @param file An path to a dimensions api biblometric file
#' @examples
#' convert_biblo(file_pat)
#'
convert_biblo = function(file) {
  data = bibliometrix::convert2df(file, dbsource = "dimensions", format = "csv")
}

#'   Tidy and Filter Data
#'   Remove any publications without a year
#'   Remove any publications without an abstract
#'   Filter to only include articles after 2020
#'   Only include articles which directly mention covid|coronavirus|sars-cov-2 in the abstract
#'
#' @param data A biblometric dataframe
#' @examples
#' tf_data(gisaid)
#'
tf_data = function(data) {
  x = data |> dplyr::filter(PY != is.na(PY)) |>
    dplyr::filter(AB != is.na(AB)) |>
    dplyr::mutate(Year = substr(PY, 1, 4)) |>
    dplyr::filter(Year > 2019) |>
    dplyr::filter(Year < 2023) |>
    dplyr::select(-c(Year)) |>
    dplyr::mutate(AB = stringr::str_to_lower(AB)) |>
    dplyr::filter(grepl('covid|coronavirus|sars-cov-2', AB))
}

#' Genrate summary statistics for biblometric dataframe
#'
#' @param data A biblometric dataframe
#' @examples
#' biblio_analysis(gisaid)
#'
biblio_analysis = function(data) {
  results = bibliometrix::biblioAnalysis(data, sep = ";")
  options(width = 100)
  S = summary(object = results,
              k = 20,
              pause = FALSE)
}

#' Get publication production for spatio temporal analysis
#'
#' @param data The results of a bibloAnalysis
#' @param name The name of the infrastructure
#' @examples
#' get_pc(nbi_res, "NBI"))
#'
get_pc = function(data, name) {
  Country = data$MostProdCountries$Country
  SCP = data$MostProdCountries$SCP
  MCP = data$MostProdCountries$MCP
  Articles = data$MostProdCountries$Articles
  pc = as.data.frame(cbind(Country, SCP, MCP, Articles))
  pc$DI = name
  return(pc)
}

#' Get publication access types
#'
#' @param data The results of a bibloAnalysis
#' @param name The name of the infrastructure
#' @examples
#' get_oa(nbi_res, "NBI"))
#'
get_oa = function(data, name) {
  oa = as.data.frame(table(data$Open.Access)) |> dplyr::rename("Access Type" = Var1)
  oa$DI = name
  return(oa)
}

#' Get nean citations for spatio temporal analysis
#'
#' @param data The results of a bibloAnalysis
#' @param name The name of the infrastructure
#' @examples
#' get_pc(nbi_res, "NBI"))
#'
get_tc = function(data, name) {
  tc = data$TCperCountries$`Total Citations`
  aac = data$TCperCountries$`Average Article Citations`
  Country = data$TCperCountries$Country
  tc = as.data.frame(cbind(tc, aac, Country))
  tc$DI = name
  return(tc)
}

#' Plot summary statistics for biblometric results
#'
#' @param data1 The results for the first infrastructure
#' @param data2 results for second infrastructure
#' @param data3 The results for the third infrastructure
#' @examples
#' plot_ss_network(data1,data2,data3))
#'
plot_ss_network = function(data2, data3) {
  oa_data = rbind(
                  get_oa(data2, "GISAID"),
                  get_oa(data3, "CV19-DP"))

  oa_data$`Access Type` = stringr::str_remove_all(oa_data$`Access Type`, "ALL OA;") |> stringr::str_trim()
  oa_plot = ggplot(oa_data,
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
    theme(legend.position = "bottom")

  # # # 3.4 Plot for publication countries collaboration per data-infrastrcuture
  pc_data = rbind(
                  get_pc(gisaid_res, "GISAID"),
                  get_pc(ena_res, "CV19-DP")) |>
    arrange(desc(Articles)) |>
    pivot_longer(c(SCP, MCP)) |>
    mutate(value = as.numeric(value)) |>
    mutate(Articles = as.numeric(Articles)) |>
    mutate(collaboration = name)

  pc_plot = ggplot(pc_data |> {
    \(.) {
      replace(., is.na(.), 0)
    }
  }(),
  aes(
    fill = collaboration,
    color = DI,
    y = log10(value),
    x = reorder(Country, Articles)
  )) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = c("white",
                                        "yellow")) +
                                          labs(title = paste0(
                                            "Multi-country publications \nvs \nSingle-country publications"
                                          )) +
    xlab("Country") +
    ylab("No. Documents (log10)") +
    coord_flip()  + theme(axis.text  = element_text(
      colour = "black",
      size = 12,
      face = "bold"
    )) +   theme(
      plot.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      panel.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      plot.title = element_text(hjust = 0.5,
                                face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) + theme(axis.text.y = element_text(size = 16),
              axis.text.x = element_text(size = 14)) + coord_cartesian() +
    coord_flip() + theme_pubclean()



  # Mean Citations ----------------------------------------------------------
  tc_data = rbind(get_tc(gisaid_res, "GISAID"),
                  get_tc(ena_res, "CV19-DP"))


  loll = rbind(
    gisaid_clean |> mutate(DI = "GISAID"),
    ena_clean |> mutate(DI = "CV19-DP")
  ) |>
    select(Publication.Date, TC, DI)

  lv = loll |> mutate(Publication.Date = (lubridate::as_date(loll$Publication.Date))) |>
    na.omit()

  ww = lv |> mutate(week = week(lubridate::as_date(Publication.Date))) |>  # create a new column 'week' based on publication date
    group_by(week, DI) |>  # group the data by week
    summarise(total_citations = mean(TC))

  oo = ggplot(ww, aes(x = week, y = total_citations, color = DI)) +
    geom_line(linewidth = 1.4) +
    labs(x = "Week", y = "Mean Citations", color = "Category DI") +  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      panel.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      plot.title = element_text(hjust = 0.5,
                                face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) + ggtitle("Mean Citations per week") +

    ### theme adjustments
    theme(
      text = element_text(face = "bold"),
      panel.background = element_blank(),
      plot.subtitle = element_text(size = 7),
      panel.grid.major.x = element_line(color = "gray25"),
    ) + theme_pubclean()



  # # # 3.5 Plot for temporal (per week) per data infrastrucure
  ll = rbind(
    gisaid_clean |> mutate(DI = "GISAID"),
    ena_clean |> mutate(DI = "CV19-DP")
  ) |>
    select(Publication.Date, DI)

  v = ll |> mutate(Publication.Date = (lubridate::as_date(ll$Publication.Date))) |>
    na.omit()

  w = v |> mutate(week = week(lubridate::as_date(Publication.Date))) |>  # create a new column 'week' based on publication date
    group_by(week, DI) |>  # group the data by week
    summarise(total_publications = n())

  time_plot = ggplot(w, aes(
    x = week,
    y = (total_publications),
    color = DI
  )) +
    geom_line(linewidth = 1.4) +
    labs(x = "Week", y = "Total Publications", color = "Category DI") +  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      panel.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      plot.title = element_text(hjust = 0.5,
                                face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) + ggtitle("Total Publications per week") +

    ### theme adjustments
    theme(
      text = element_text(face = "bold"),
      panel.background = element_blank(),
      plot.subtitle = element_text(size = 7),
      panel.grid.major.x = element_line(color = "gray25"),
    ) + theme_pubclean()


  fig = ggarrange(
    ggarrange(time_plot,
              ggarrange(pc_plot, oa_plot, ncol = 2, widths = c(1, 1)),
              oo,
              ncol = 1,
              heights = c(0.4, 1, 0.4)
    ))

  ggsave(
    paste0("plots/summary-plot.png"),
    dpi = 320,
    width = 16,
    height = 18,
    limitsize = FALSE
  )

  return(fig)
}

#' Calculate networks and save results to a specified path
#'
#' @param data A biblometric dataframe
#' @param path Path to save files
#' @examples
#' calculate_networks(data,path))
#'
calculate_networks = function(data, path) {
  # Create Networks and Calculate Statistics --------------------------------

  message(
    "Starting Network Build...This might take a while, why not play chess? Write a Poem? Or some other enjoyable activity."
  )
  # Scale 1: Researcher
  author_net = bibliometrix::biblioNetwork(data,
                                           analysis = "collaboration",
                                           network = "authors",
                                           sep = ";")
  author_net_stats = networkStat(author_net) |> network_stat_df()

  author_net_node_stats = indicies(igraph::graph_from_adjacency_matrix(author_net))
  saveRDS(author_net, paste0(path, "author_net.rds"))
  message("Author network complete")

  # Scale 2: Environment Structure
  institution_net = split_author_matrix(data, "AU_UN") |> igraph::graph_from_data_frame()
  institution_net_stats = networkStat(institution_net) |> network_stat_df()
  saveRDS(institution_net, paste0(path, "institution_net.rds"))
  institution_net_node_stats = indicies(igraph::as.undirected(institution_net))
  message("Institution network complete")

  city_net = split_author_matrix(data, "City.of.Research.organization") |> igraph::graph_from_data_frame()
  city_net_stats = networkStat(city_net) |> network_stat_df()
  saveRDS(city_net, paste0(path, "city_net.rds"))
  city_net_node_stats = indicies(as.undirected(city_net))
  message("City network complete")

  country_net = split_author_matrix(data, "AU_CO") |> igraph::graph_from_data_frame()
  country_net_stats = networkStat(country_net) |> network_stat_df()
  saveRDS(country_net, paste0(path, "country_net.rds"))
  country_net_node_stats = indicies(as.undirected(country_net))
  message("Country network complete")

  # Scale 3: Scaffolding Structure
  funding_net = split_author_matrix(data, "FU") |> igraph::graph_from_data_frame()
  funding_net_stats = networkStat(funding_net) |> network_stat_df()
  saveRDS(funding_net, paste0(path, "funding_net.rds"))
  funding_net_node_stats = indicies(as.undirected(funding_net))
  message("Funding network complete")

  funding_country_net = split_author_matrix(data, "Funder.Country") |> igraph::graph_from_data_frame()
  funding_country_net_stats = networkStat(funding_country_net) |> network_stat_df()
  saveRDS(funding_country_net,
          paste0(path, "funding_country_net.rds"))
  funding_country_net_node_stats = indicies(as.undirected(funding_country_net))
  message("Funding country network complete")


  # Merge and Save Data -----------------------------------------------------
  category = c("author",
               "institution",
               "city",
               "country",
               "funding",
               "funding country")
  net_stats = rbind(
    author_net_stats,
    institution_net_stats,
    city_net_stats,
    country_net_stats,
    funding_net_stats,
    funding_country_net_stats
  ) |> dplyr::mutate(categories = category) |> round_df(2)

  centrality_stats = as.list(
    author_net_node_stats,
    institution_net_node_stats,
    city_net_node_stats,
    country_net_node_stats,
    funding_net_node_stats,
    funding_country_net_node_stats,
    funding_group_net_node_stats
  )

  saveRDS(net_stats, paste0(path, "network_stats.rds"))
  saveRDS(centrality_stats, paste0(path, "centrality_stats.rds"))

}

#' Plot networks and save figure
#'
#' @param network A network to plot
#' @param type Path to save files
#' @param di Name of infrastructure
#' @examples
#' plot_network(country_network,"country","GISAID))
#'
plot_network = function(network, type, di) {
  # create df
  df = as_data_frame(network) |> dplyr::mutate(from = str_to_lower(from)) |> dplyr::mutate(to = str_to_lower(to))
  if (file.exists(paste0("data/", di, "/", type, "_coords.rds"))) {
    dfc = readRDS(paste0("data/", di, "/", type, "_coords.rds"))
  } else {
    countries = as.data.frame(unique(c(df$from, df$to)))
    colnames(countries) = "name"
    dfc = countries  |> tidygeocoder::geocode(name, lat = latitude, long = longitude)

    saveRDS(dfc, paste0("data/", di, "/", type, "_coords.rds"))
  }

  # Merge coordinates with data frame
  df = merge(df, dfc, by.x = "from", by.y = "name")
  df =
    merge(
      df,
      dfc,
      by.x = "to",
      by.y = "name",
      suffixes = c(".from", ".to")

    )

  weights = df$weight
  q9=as.data.frame(quantile(weights,c(0.98)))
  q98 = as.numeric(q9[1])

  print(paste0(q98, " be the 98%"))

  df = na.omit(df)
  dfc = na.omit(dfc)

  map_world = map_data(map = "world") |>
    filter(region != "Antarctica")
  ploy_country = sfheaders::sf_polygon(
    obj = map_world
    , x = "long"
    , y = "lat"
    , polygon_id = "group"
  ) |> st_as_sf()
  ploy_country <- st_set_crs(ploy_country, 4326)
  points = st_as_sf(dfc, coords = c("longitude", "latitude"),
                    crs = 4326)
  (ploy_country$pt_count <- lengths(st_intersects(ploy_country, points)))
  (ploy_country$pt_count <- ifelse(ploy_country$pt_count==0,NA,ploy_country$pt_count))

  if(type == "institution" | type == "city") {
    ll=
      ggplot(ploy_country) +
      geom_sf(colour = "white") +
      geom_sf(aes(geometry = geometry, fill = pt_count)) +
      labs(fill='Node Count') +
      geom_segment(
        data =  df |> filter(weight > q98),
        aes(
          x = longitude.from,
          y = latitude.from,
          xend = longitude.to,
          yend = latitude.to,
          color = weight,
          size = 10
        ),
        arrow = arrow(length = unit(0.1, "inches")), # optional arrows
        # type = "closed"),
        size = 0.25,
        alpha = 0.5
      ) +
      theme_pubclean() +
      theme(
        plot.title = element_text(
          size = 18,
          hjust = 0
        ),
        # ^make title left-justified
        plot.subtitle = element_text(
          size = 7,
          face = "bold"
        ),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(size = 7),
        legend.position = "bottom",
        # make legend keys vertical and left-justified
        legend.direction = "horizontal",
        # remove white backgrounds from legend and keys
        legend.background = element_blank(),
        legend.key = element_blank()
      )+        scale_color_distiller(
        palette = "Greys",
        trans = "log10",
        name = "Collaborations (98%) \nlog10 scale"
      ) + labs(title = paste(di,":",type), x="",y="") + scale_fill_gradient(low="gold", high="darkorchid", na.value="black")
  } else {

    ploy_country$pt_count[ploy_country$pt_count == 2] <- 1
    ll=
      ggplot(ploy_country) +
      geom_sf(colour = "white") +
      geom_sf(aes(geometry = geometry,fill = pt_count)) +
      guides(fill="none") +
      geom_segment(
        data =  df |> filter(weight > q98),
        aes(
          x = longitude.from,
          y = latitude.from,
          xend = longitude.to,
          yend = latitude.to,
          color = weight,
          size = 10
        ),
        arrow = arrow(length = unit(0.1, "inches")), # optional arrows
        # type = "closed"),
        size = 0.25,
        alpha = 0.5
      ) +
      geom_point(data = dfc,
                 aes(longitude, latitude),
                 color = "red")  + theme_pubclean() +
      theme(
        plot.title = element_text(
          size = 18,
          hjust = 0
        ),
        # ^make title left-justified
        plot.subtitle = element_text(
          size = 7,
          face = "bold"
        ),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(size = 7),
        legend.position = "bottom",
        # make legend keys vertical and left-justified
        legend.direction = "horizontal",
        # remove white backgrounds from legend and keys
        legend.background = element_blank(),
        legend.key = element_blank()
      ) +
      scale_color_distiller(
        palette = "Greys",
        trans = "log10",
        name = "Collaborations (98%) \nlog10 scale"
      ) + labs(title = paste(di,":",type), x="",y="") + scale_fill_gradient(low="gold", high="darkorchid", na.value="black")


  }

  return(ll)

}
