test_that("export_network writes a basic undirected GEXF file", {
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file), add = TRUE)

  mi <- matrix(
    c(
      0,   0.5, 0,
      0.5, 0,   0.8,
      0,   0.8, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  labels <- c("A", "B", "C")

  export_network(file, labels, mi, directed = FALSE)

  expect_true(file.exists(file))

  doc <- xml2::read_xml(file)

  nodes <- xml2::xml_find_all(doc, ".//nodes/node")
  edges <- xml2::xml_find_all(doc, ".//edges/edge")

  expect_length(nodes, 3)
  expect_length(edges, 2)

  expect_equal(
    xml2::xml_attr(nodes, "label"),
    c("A", "B", "C")
  )
})


test_that("export_network preserves directed edges", {
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file), add = TRUE)

  mi <- matrix(
    c(
      0,   0.5,
      0.8, 0
    ),
    nrow = 2,
    byrow = TRUE
  )

  export_network(
    file,
    labels = c("A", "B"),
    mi = mi,
    directed = TRUE
  )

  doc <- xml2::read_xml(file)

  graph <- xml2::xml_find_first(doc, ".//graph")
  edges <- xml2::xml_find_all(doc, ".//edges/edge")

  expect_equal(
    xml2::xml_attr(graph, "defaultedgetype"),
    "directed"
  )

  expect_length(edges, 2)

  expect_equal(
    sort(as.numeric(xml2::xml_attr(edges, "weight"))),
    c(0.5, 0.8)
  )
})


test_that("export_network writes node and edge properties", {
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file), add = TRUE)

  mi <- matrix(
    c(
      0,   0.5,
      0.5, 0
    ),
    nrow = 2,
    byrow = TRUE
  )

  node_prop <- list(
    name = "group",
    data = c("X", "Y"),
    type = "string"
  )

  edge_prop <- list(
    name = "strength",
    data = matrix(
      c(
        0, 10,
        10, 0
      ),
      nrow = 2,
      byrow = TRUE
    ),
    type = "double"
  )

  export_network(
    file,
    labels = c("A", "B"),
    mi = mi,
    directed = FALSE,
    node_prop,
    edge_prop
  )

  doc <- xml2::read_xml(file)

  node_attributes <- xml2::xml_find_all(
    doc,
    ".//attributes[@class='node']/attribute"
  )

  edge_attributes <- xml2::xml_find_all(
    doc,
    ".//attributes[@class='edge']/attribute"
  )

  expect_equal(
    xml2::xml_attr(node_attributes, "title"),
    "group"
  )

  expect_equal(
    xml2::xml_attr(edge_attributes, "title"),
    "strength"
  )

  node_values <- xml2::xml_find_all(
    doc,
    ".//nodes/node/attvalues/attvalue"
  )

  expect_equal(
    xml2::xml_attr(node_values, "value"),
    c("X", "Y")
  )

  edge_values <- xml2::xml_find_all(
    doc,
    ".//edges/edge/attvalues/attvalue"
  )

  expect_equal(
    xml2::xml_attr(edge_values, "value"),
    "10"
  )
})

test_that("export_network rejects invalid property types", {
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file), add = TRUE)

  mi <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  bad_prop <- list(
    name = "group",
    data = c("A", "B"),
    type = "banana"
  )

  expect_error(
    export_network(
      file,
      labels = c("A", "B"),
      mi = mi,
      directed = FALSE,
      bad_prop
    ),
    "Unknown datatype"
  )
})
