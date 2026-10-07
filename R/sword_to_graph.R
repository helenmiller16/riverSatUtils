#' Convert SWORD reaches to directed network graph
#'
#' Converts an `sf` reaches object into an `sfnetwork`, where nodes represent
#' reaches and edges represent possible upstream/downstream connectivity.
#'
#' @param sword_reaches `sf` object containing SWORD reaches and required
#'   topology columns. It should be some subset of the reaches from SWORD 17c.
#' @param version SWORD version; currently only `"17c"` is supported.
#' @return A directed [sfnetworks::sfnetwork()] object.
#' @details
#' Expected fields include:
#' `reach_id`, upstream IDs (`rch_id_up_1`...`rch_id_up_4`), downstream IDs
#' (`rch_id_dn_1`...`rch_id_dn_4`), geometry, and `reach_len`, following the
#' format of SWORD 17c dataset.
#'
#' SWORD 17c can be downloaded to the cache directory via [download_sword()],
#' and loaded via [load_sword()].
#' @export
sword_to_graph <- function(sword_reaches, version = "17c") {
  if (version != "17c") stop("only v17c currently supported.")
  nodes <- as.data.table(sword_reaches)
  nodes$node_id <- 1:nrow(nodes)
  nodes$geom <- st_cast(nodes$geom, "LINESTRING", group_or_split = F)
  nodes$geom <- st_line_interpolate(st_transform(nodes$geom, "EPSG:5070"), .5, TRUE)

  # Make edges: get all combinations of from-to for each reach.
  up_cols <- paste0("rch_id_up_", 1:4)
  dn_cols <- paste0("rch_id_dn_", 1:4)

  up_long <- melt(
    nodes,
    id.vars = "reach_id",
    measure.vars = up_cols,
    value.name = "from"
  )[from != 0 & !is.na(from), .(reach_id, from)]

  dn_long <- melt(
    nodes,
    id.vars = "reach_id",
    measure.vars = dn_cols,
    value.name = "to"
  )[to != 0 & !is.na(to), .(reach_id, to)]

  edges <- up_long[
    dn_long, on = "reach_id", allow.cartesian = TRUE
  ][, .(from, to)]

  edges <- edges[!duplicated(edges)]

  # remove edges which come from reaches not in the network
  edges <- edges[from %in% nodes$reach_id]
  edges <- edges[to %in% nodes$reach_id]

  # Remove edges which go to themselves
  edges <- edges[from != to]

  # Calculate length of each edge. Since they go from the center of two
  # reaches, it is the mean length of those two reaches
  edges$from_len <- nodes[.(edges$from), on = "reach_id"]$reach_len
  edges$to_len <- nodes[.(edges$to), on = "reach_id"]$reach_len
  edges[, length := (from_len + to_len)/2]
  edges$from <- nodes[.(edges$from), on = "reach_id"]$node_id
  edges$to <- nodes[.(edges$to), on = "reach_id"]$node_id

  sfnetworks::sfnetwork(
    st_as_sf(nodes),
    edges,
    directed = TRUE)
}


# TODO: get_dam_info() can fail for empty obstruction classes
#
# igraph::distances() with no source vertices can create edge cases.
# matrixStats::colMins on empty matrices may error; defensive checks recommended.

#' Compute distance-to-obstruction summaries for reaches
#'
#' Builds a river graph from reaches and computes downstream graph distances to
#' obstruction classes 1-4 (`obstr_type`), using edge lengths as weights.
#'
#' @param sword_reaches `sf` object of SWORD reaches.
#' @param append Logical; when `TRUE`, merge computed fields back into
#'   `sword_reaches`.
#' @return If `append = TRUE`, returns input reaches with added columns
#'   `dist_to_obst1`...`dist_to_obst4`; otherwise returns a table keyed by
#'   `reach_id`.
#' @export
get_dam_info <- function(sword_reaches, # shapefile of reaches (subset of sword dataset)
                         append = TRUE) {
  # Add dam info to reaches.
  # Use graph functionality, meaning we need to make a graph.
  # Use the function in src_interpolate/functions for this.
  graph <- sword_to_graph(sword_reaches)
  nodes <- as.data.table(sfnetworks::activate(graph, "nodes"))
  edges <- as.data.table(sfnetworks::activate(graph, "edges"))
  # matrix giving distance downstream from each dam to all nodes
  dam_dist_list <- lapply(1:4, \(o) {
    igraph::distances(graph,
              v = which(nodes$obstr_type == o),
              mode = "out",
              weights = edges$length)
  })
  dam_dist_list <- lapply(dam_dist_list, matrixStats::colMins)
  nodes$dist_to_obst1 <- dam_dist_list[[1]]
  nodes$dist_to_obst2 <- dam_dist_list[[2]]
  nodes$dist_to_obst3 <- dam_dist_list[[3]]
  nodes$dist_to_obst4 <- dam_dist_list[[4]]
  dam_info <- nodes[, .(reach_id, dist_to_obst1, dist_to_obst2, dist_to_obst3, dist_to_obst4)]
  if (append) {
    merge(sword_reaches, dam_info)
  } else {
    dam_info
  }
}
