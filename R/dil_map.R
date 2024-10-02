#'@import units
.conc_ratio <- function(c1, c2){
  # assert starts with digit
  stopifnot(grepl("^\\d.*", c1))
  stopifnot(grepl("^\\d.*", c2))

  # remove well position after _
  c2 <- gsub("(.*)(_.*)", "\\1", c2)

  # parse
  unit_c1 <- gsub("^[0-9]\\d*(\\.\\d+)?", "", c1)
  unit_c2 <- gsub("^[0-9]\\d*(\\.\\d+)?", "", c2)
  c1 <- gsub("^([0-9.]\\d*\\.?\\d*)(.*)", "\\1", c1) |> as.numeric()
  c2 <- gsub("^([0-9.]\\d*\\.?\\d*)(.*)", "\\1", c2) |> as.numeric()

  # assert either both has units or not
  is.empty <- \(x) x == ""

  stopifnot(is.empty(unit_c2) == is.empty(unit_c1))
  stopifnot(is.numeric(c1) & is.numeric(c2))


  if(!is.empty(unit_c1)) {
    units::units_options(set_units_mode = "standard")

    c1 <- units::set_units(c1, unit_c1)
    c2 <- units::set_units(c2, unit_c2)
  }

  # return ratio factor as factor:1
  (c1/c2) |> as.character()
}

# generate edges
.gen_edges <- function(df){
  edges <- data.frame(from = NULL, to = NULL)
  d <- as.data.frame(df)

  if(ncol(d) < 2){
    stop("Need more than 2 columns")
  } else if(ncol(d) ==2){
    edges <- data.frame(from = d[, 1], to = d[, 2])
  } else{
    for(i in 1:nrow(d)){
      for(ii in 1:ncol(d)){
        if(ii != ncol(d)){
          edges <- rbind(edges, data.frame(from = d[i, ii], to = d[i, ii+1]))
        }
      }
    }
  }

  dplyr::distinct(edges) |> 
    dplyr::mutate(color = sample(grDevices::colors(), n())) |>
    dplyr::rowwise() |>
    dplyr::mutate(label = paste0("1:", .conc_ratio(.data$from, .data$to)))

}


# generate nodes
.gen_nodes <- function(df){
  df |>
    dplyr::mutate(y = row_number()) |>
    tidyr::pivot_longer(-matches("y"), names_to = "name", values_to = "label") |>
    dplyr::arrange((.data$name)) |> # ensure priority to first
    dplyr::distinct(.data$label, .keep_all = TRUE) |>


    dplyr::mutate(shape = "polygon") |>
    dplyr::mutate(style = "filled") |>
    dplyr::mutate(color = "black") |>
    dplyr::mutate(fillcolor = "lightgrey") |>

    dplyr::mutate(constraint = "false") |>
    dplyr::mutate(overlap = "false") |>
    dplyr::mutate(splines = "spline") |>
    dplyr::mutate(x = gsub("v", "-", x = .data$name) |> as.numeric()) |>
    dplyr::add_count(as.character(.data$x)) |>  # see if a level is dangeling
    dplyr::mutate(y = ifelse(n == 1, 3, .data$y)) # recenter danglings
    #mutate (x = x*100, y = y*100)
}




# generate graph from nodes and edges and print it
.gen_graph <- function(df){
  nodes <- .gen_nodes(df)
  edges <- .gen_edges(df)

  DiagrammeR::create_graph() |>
    DiagrammeR::add_global_graph_attrs("graph", attr = "rankdir", value = "LR") |>
    DiagrammeR::add_global_graph_attrs("graph", attr = "splines", value = "spline") |>
    DiagrammeR::add_global_graph_attrs("graph", attr = "nodesep", value = "2") |>
    DiagrammeR::add_global_graph_attrs(
      attr = "overlap",
      value = "false",
      attr_type = "graph") |>

    DiagrammeR::add_nodes_from_table(nodes, label = "value") |>
    DiagrammeR::add_edges_from_table(edges, from_col = "from", 
      to_col = "to", from_to_map = "label") |> # adding edges
    DiagrammeR::mutate_node_attrs(label = gsub("(.*)_(.*)", "\\1@\\_\\{\\2\\}",
     .data$label)) |> # sub location
    DiagrammeR::mutate_node_attrs(label = paste0(.data$label, '@^{', .data$id, '}')) |> #
    DiagrammeR::mutate_node_attrs(width = 1) |>
    DiagrammeR::mutate_node_attrs(x = .data$x*2) # expand

}


.parallel_dilution <- function(plate, fold = 10, unit = "ng/ml", type, rep = 1){
  checkmate::assertNumeric(fold, lower = 0.1, upper = 10000)
  checkmate::assertNumber(rep, lower = 1, upper = 20)
  checkmate::assertChoice(type, choices = c("Standard", "QC") )
  df <- plate$df

  if(.last_std(plate) == 0){
    stop("No standard found")
  }

  df <- df |> dplyr::filter(.data$TYPE == type, .data$std_rep == rep) |>
    dplyr::mutate(v1 = paste0(fold * as.numeric(.data$conc), unit)) |>
    dplyr::mutate(v0 = paste0(.data$conc, unit, "_", .data$SAMPLE_LOCATION)) |>
    dplyr::select(matches("v1"), matches("v0"), matches("TYPE"))

  if(nrow(df) < 1 ){
    stop("This combination is not present in the plate")
  }
  df

}

# no use now after separate logic
.multi_graph  <- function(df){
  x <- split(as.data.frame(df), df$TYPE) |> 
    lapply(\(x) dplyr::select(x, -matches("TYPE"))) |> 
    lapply(\(x) .gen_graph(x))

  # if(length(x) == 1){
  #   x
  # } else if(length(x) == 2){

  #   x[[1]] <- x[[1]] |> DiagrammeR::mutate_node_attrs(cluster = "A")
    
  #   x[[2]] <- x[[2]] |> DiagrammeR::mutate_node_attrs(cluster = "B")

  #   nodes <- rbind(get_node_df(x[[1]]), get_node_df(x[[2]]))
  #   edges <- rbind(get_edge_df(x[[1]]), get_edge_df(x[[2]]))

  #   create_graph() |> add_node_df(nodes) |> 
  #     add_edge_df(edges) 
  #   # DiagrammeR::combine_graphs(x[[1]], x[[2]])
  # } else{
  #   stop("Too many types")
  # }

  x
}


.final_vol <- function(x, vol){
  # x is in format 1:10
  x <- strsplit(x, ":")[[1]]

  v2 <- vol 
  v1 <- (as.numeric(x[1])* v2) / as.numeric(x[2])
  v2 <- v2 - v1
  paste0(v1, ":", v2)
}
