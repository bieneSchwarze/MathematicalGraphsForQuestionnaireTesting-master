###############################################################################
###                          Additional functions                           ###
###############################################################################

# @author Katharina Stark
# 04.12.2020

### Questionnaire graph generation --------------------------------------------
#   Test, if the questionnaire graph was generated correctly resp. if the
#   template of the questionnaire programming is error-free
#   (see Elliott, 2012: 12)
#   - Starting and ending vertices are included on the right position
#   - Connectedness
#   - All edges are directed
#   - Parallel edges
test_graph_chars <- function(graph) {
  # Is starting vertex included?
  print(paste("Starting vertex included:", V(graph)["BEGIN"] %in% V(graph)))
  # Is ending vertex included?
  print(paste("Ending vertex included:", V(graph)["END"] %in% V(graph)))
  # Is graph connected?
  print(paste("Graph connected:", is_connected(graph = graph)))
  # Is graph directed?
  print(paste("Graph directed:", is_directed(graph = graph)))
  # Do multiple/parallel edges exist?
  print(paste("Parallel edges:", any(which_multiple(graph = graph))))
}



### KEY PROPERTIES ------------------------------------------------------------
#   - GENERATION OF ALL POSSIBLE FILTER PATTERNS
patgen <- function(path_num) {
  spspace_list <- vector(mode = "list", length = path_num)
  for(j in 1:path_num) {
    y <- paste(1, 1:path_num, sep = "")
    x <- combn(y, j)
    x_p <- vector(mode = "character")
    for(i in 1:length(x[1,])) {
      x_p[i] <- paste(x[,i], collapse = "-")
    }
    spspace_list[[j]] <- x_p
    allPosPat <- unlist(spspace_list)
  }
  return(allPosPat)
}

#   - FILTER QUESTIONS
filter_fun <- function(graph) {
  V(graph)[degree(graph, mode = c("out")) > 1]
}
