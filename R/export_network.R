#' Export network data to a file in GEXF format
#'
#' Exports a network to a GEXF (XML) file that can be loaded by network
#' visualization software such as Gephi.
#'
#' @param filename Name of output file. \code{".gexf"} is appended
#'   automatically if not already present.
#' @param labels Character vector of length \code{N} with node labels.
#' @param mi MI/network matrix (\code{N} by \code{N}).
#' @param directed Logical; whether edge directionality should be
#'   considered. If \code{FALSE}, \code{mi} is symmetrized via
#'   \code{pmax(mi, t(mi))} and only the lower triangle (diagonal included)
#'   is used as the edge list, matching MATLAB's \code{tril()} behavior.
#'   Note: if \code{diag(mi)} is nonzero, this preserves self-loop edges
#'   in the export, matching the original MATLAB behavior exactly.
#' @param ... Optional extra node/edge properties. Each must be a named
#'   list with elements \code{name}, \code{data}, and \code{type}
#'   (one of \code{"boolean"}, \code{"int"}, \code{"long"}, \code{"float"},
#'   \code{"double"}, \code{"string"}). Whether a property is a node or
#'   edge property is inferred from the shape of \code{data}: a
#'   length-\code{N} vector (or \code{1 x N} / \code{N x 1} matrix) is a
#'   node property; an \code{N x N} matrix is an edge property.
#'
#'   Node data cannot be named \code{"id"} or \code{"label"}, and edge data
#'   cannot be named \code{"id"}, \code{"source"}, \code{"target"}, or
#'   \code{"weight"}.
#'
#' @return \code{NULL}, invisibly. This function is called for its side
#'   effect of writing the network to \code{filename}.
#'
#' @family networkanalysis
#' @export
export_network <- function(filename, labels, mi, directed, ...) {
  props <- list(...)
  N <- length(labels)

  .prop_dims <- function(d) {
    dm <- dim(d)
    if (is.null(dm)) length(d) else dm
  }
  .is_node_prop <- function(d) {
    dm <- .prop_dims(d)
    if (length(dm) == 1) return(dm == N)
    if (length(dm) == 2) return((dm[1] == 1 && dm[2] == N) || (dm[1] == N && dm[2] == 1))
    FALSE
  }
  .is_edge_prop <- function(d) {
    dm <- .prop_dims(d)
    length(dm) == 2 && dm[1] == N && dm[2] == N
  }

  valid_types <- c("boolean", "int", "long", "float", "double", "string")
  node_props <- list()
  edge_props <- list()

  for (i in seq_along(props)) {
    p <- props[[i]]
    ending <- sprintf(" in extra parameter %d", i)
    for (f in c("name", "data", "type")) {
      if (!(f %in% names(p))) stop(sprintf("%s field missing%s", f, ending))
    }
    if (.is_node_prop(p$data)) {
      node_props[[length(node_props) + 1]] <- p
    } else if (.is_edge_prop(p$data)) {
      edge_props[[length(edge_props) + 1]] <- p
    } else {
      stop(sprintf("Wrong size [%s]%s",
                   paste(.prop_dims(p$data), collapse = " "), ending))
    }
    if (!(p$type %in% valid_types)) {
      stop(sprintf("Unknown datatype %s%s", p$type, ending))
    }
  }

  ## Symmetrize / triangularize for undirected networks -----------------------
  if (!directed) {
    mi <- pmax(mi, t(mi))
    mi[!lower.tri(mi, diag = TRUE)] <- 0  # tril() equivalent, diagonal kept
  }

  ## Create XML document --------------------------------------------------
  message("Creating document...")
  doc <- xml2::xml_new_root("gexf", version = "1.3")
  graph <- xml2::xml_add_child(doc, "graph")
  xml2::xml_set_attr(graph, "mode", "static")
  xml2::xml_set_attr(graph, "defaultedgetype", if (directed) "directed" else "undirected")

  .fmt_val <- function(x) {
    if (is.character(x)) x else as.character(x)
  }

  if (length(node_props) > 0) {
    atts <- xml2::xml_add_child(graph, "attributes")
    xml2::xml_set_attr(atts, "class", "node")
    for (i in seq_along(node_props)) {
      att <- xml2::xml_add_child(atts, "attribute")
      xml2::xml_set_attr(att, "id", as.character(i - 1))
      xml2::xml_set_attr(att, "title", node_props[[i]]$name)
      xml2::xml_set_attr(att, "type", node_props[[i]]$type)
    }
  }

  if (length(edge_props) > 0) {
    atts <- xml2::xml_add_child(graph, "attributes")
    xml2::xml_set_attr(atts, "class", "edge")
    for (i in seq_along(edge_props)) {
      att <- xml2::xml_add_child(atts, "attribute")
      xml2::xml_set_attr(att, "id", as.character(i - 1))
      xml2::xml_set_attr(att, "title", edge_props[[i]]$name)
      xml2::xml_set_attr(att, "type", edge_props[[i]]$type)
    }
  }

  nodes_el <- xml2::xml_add_child(graph, "nodes")
  edges_el <- xml2::xml_add_child(graph, "edges")

  ## Add node data ----------------------------------------------------------
  message("Writing node data to document...")
  for (i in seq_len(N)) {
    node <- xml2::xml_add_child(nodes_el, "node")
    xml2::xml_set_attr(node, "id", as.character(i - 1))
    xml2::xml_set_attr(node, "label", labels[[i]])
    if (length(node_props) > 0) {
      avs <- xml2::xml_add_child(node, "attvalues")
      for (j in seq_along(node_props)) {
        av <- xml2::xml_add_child(avs, "attvalue")
        xml2::xml_set_attr(av, "for", as.character(j - 1))
        xml2::xml_set_attr(av, "value", .fmt_val(node_props[[j]]$data[[i]]))
      }
    }
  }

  ## Add edge data ------------------------------------------------------------
  message("Writing edge data to document...")
  edge_id <- 0L
  for (i in seq_len(N)) {
    for (j in seq_len(N)) {
      if (mi[i, j] == 0) next
      edge <- xml2::xml_add_child(edges_el, "edge")
      xml2::xml_set_attr(edge, "id", as.character(edge_id))
      xml2::xml_set_attr(edge, "source", as.character(i - 1))
      xml2::xml_set_attr(edge, "target", as.character(j - 1))
      xml2::xml_set_attr(edge, "weight", as.character(mi[i, j]))
      if (length(edge_props) > 0) {
        avs <- xml2::xml_add_child(edge, "attvalues")
        for (k in seq_along(edge_props)) {
          av <- xml2::xml_add_child(avs, "attvalue")
          xml2::xml_set_attr(av, "for", as.character(k - 1))
          xml2::xml_set_attr(av, "value", .fmt_val(edge_props[[k]]$data[i, j]))
        }
      }
      edge_id <- edge_id + 1L
    }
  }

  if (!grepl("\\.gexf$", filename)) {
    filename <- paste0(filename, ".gexf")
  }

  xml2::write_xml(doc, filename)
  invisible(NULL)
}
