###############################################################################
### Using Mathematical Graphs for Questionnaire Testing: A simple Example   ###
###          R Program for graph generation and testing procedure           ###
###                   Testing Scenario 2: Redundant paths                   ###
###############################################################################

# @author Katharina Stark
# 04.12.2020

###############################################################################
### Preliminaries                                                           ###
###############################################################################

rm(list = ls())

##  File path
path <- ""
setwd(path)

##  Packages
if(!require(igraph)) install.packages("igraph"); library(igraph)
if(!require(utils)) install.packages("utils"); library(utils)
if(!require(openxlsx)) install.packages("openxlsx"); library(xlsx)
if(!require(tidyverse)) {
  install.packages("tidyverse")
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
}
library(data.table)
library(zoo)

##  Load further functions
source("Functions.R")




###############################################################################
### 1.0 Questionnaire Graph                                                 ###
###############################################################################

###############################################################################
### 1.1 Derivation of the questionnaire graph from the progression table    ###
###############################################################################

##  Load progression table (pt) in the respective file format 
#   (in this case, it's an excel file)
#   In the following, the progression table is saved with the object name 
#   quest_pt for questionnaire progression table
quest_pt <- read.xlsx("ProgressionTable.xlsx", 
                      sheet = "ProgressionTable",
                      colNames = TRUE, sep.names = " ")

##  Build questionnaire graph (quest_graph) for graph analysis from progression
#   table
quest_graph <- graph_from_data_frame(quest_pt[,c("FROM","TO")])

##  Test graph characteristics (see Stark and Zinn 2021) and if the graph was
#   generated correctly
test_graph_chars(graph = quest_graph)
# -> no errors, i.e., graph was generated correctly
# -> no parallel edges and thus no simplification needed
# Note: All paths through the questionnaire can only be computed for simple
#       graphs, i.e., the algorithm ignores multiple edges and loops. In case
#       of parallel edges we have to simplify our graph but at the same time,
#       we must keep all edge attributes after assigning them to the graph (see
#       below).



### ASSIGN IMPORTANT EDGE ATTRIBUTES TO THE GRAPH -----------------------------

##  Filter instructions (from progression table)
quest_graph <- set_edge_attr(quest_graph, "Filter Instructions",
                             value = quest_pt[,"FILTER"])
#   Test
#   Note: Under those 'Test' headings you can check, if different assignments
#         were executed correctly. In some cases these test code lines must be
#         adapted to the graph under examination.
edge_attr(quest_graph, "Filter Instructions")

##  All possible answer options
quest_graph <- set_edge_attr(quest_graph, "All Possible Answer Options",
                             value = quest_pt[,"ANSWER OPTIONS"])
#   Test
# edge_attr(quest_graph, "All Possible Answer Options",     # 1; 2
#           E(quest_graph)["sex|edu"])
# edge_attr(quest_graph, "All Possible Answer Options",     # 1
#           E(quest_graph)["smoker|smodev"])



### SIMPLIFY GRAPH ------------------------------------------------------------
#   Note: This step is only necessary, if parallel edges occur (see above).
#         Since we don't have parallel edges in this simple example, we skip
#         this part.



## Plot graph -----------------------------------------------------------------
#  Note: This piece of code plots the questionnaire graph for the simple
#        example.
#  Determine graph coordinates
coord <- as.matrix(cbind(c(690, 690, 690, 690, 690, 560, 560, 560, 820, 690, 690, 1000),
                         c(500, 440, 380, 320, 260, 220, 160, 100, 220,  60,   0,  260)))
#  Determine filter questions
quest_filter <- filter_fun(quest_graph)
#  Determine different properties of the graph plot
V(quest_graph)$color <- ifelse(V(quest_graph) %in% quest_filter,
                               "lightblue2", "white")
V(quest_graph)$label.cex <- 0.8
E(quest_graph)$arrow.size <- 0.2
V(quest_graph)$label.color <- "black"

#  Plot graph
plot(quest_graph, layout = coord, edge.color = "black", main = "Questionnaire Graph")



###############################################################################
### 1.2 Graph properties                                                    ###
###############################################################################

##  KEY PROPERTIES ------------------------------------------------------------

#   1. All possible paths 
quest_paths <- all_simple_paths(graph = quest_graph, 
                                from = V(quest_graph)["BEGIN"],
                                to = V(quest_graph)["END"])

#      Number of all possible paths
(quest_path_count <- length(quest_paths))     # 5     

#      Name all possible paths
names(quest_paths) <- paste("posPath", 1:length(quest_paths), sep="")
all_pos_paths <- c(1:quest_path_count)
names(all_pos_paths) <- paste0("all_pos_paths", 1:quest_path_count)
all_pos_paths <- sapply(quest_paths, 
                        function(x) paste(as_ids(x), collapse = "-"))
all_pos_paths <- all_pos_paths[order(sapply(quest_paths, length),
                                     decreasing = TRUE)]
all_pos_paths_ls <- as.list(all_pos_paths)
#      Assign all possible paths as graph attribute
quest_graph <- set_graph_attr(quest_graph, "All Possible Paths",
                              all_pos_paths)

#   2. Filter patterns
#      Number of all possible filter patterns
#      (Sample space - 1 (empty space))
(quest_patnum <- (2^quest_path_count)-1)     # 31       
#      All possible filter patterns
#      Note: This line of code produces all possible filter patterns. In the
#            simple example, there are 31 possible filter patterns. As the
#            number of all possible filter patterns grows exponentially with
#            the number of filter questions, you might be better off to 
#            comment this line of code if you have a lot of filter questions in
#            your questionnaire (see Stark and Zinn 2021).            
allPosPatterns <- patgen(path_num = quest_path_count)



### SAVE EMPTY GRAPH ----------------------------------------------------------
save(quest_pt, quest_graph, file = "quest_graph.RData")




###############################################################################
### 2.0 Empirical graphs                                                    ###
###############################################################################

###############################################################################
### 2.1 Data set                                                            ###
###############################################################################
load("s2_data.RData")



###############################################################################
### 1.2 Derivation of the empirical graphs from the empirical data          ###
###############################################################################

### STEP 1: TRANSFORM DATASET INTO A DATA SHEET FOR GENERATING A GRAPH --------
data_sheet <- data
N <- dim(data)[1]

##  1. Convert the response IDs into the vertex numbers 
#      (Exact answer is not important yet)            
for (i in 1:N) {                            # Rows
  for (j in 2:length(data_sheet[1,])) {     # Columns
    data_sheet[i,j] <- if(is.na(data_sheet[i,j])==FALSE) {
      data_sheet[i,j] <- names(data_sheet[j])
    } else {
      data_sheet[i,j] <- NA                 # NA: question is skipped due to skip instructions
    }
  }        
}

##  2. Add BEGIN and END as starting and ending vertices to the graph ---------
data_sheet <- cbind(BEGIN = "BEGIN", data_sheet, END = "END")
data_sheet <- data_sheet[,c("ID", "BEGIN", 
                                colnames(data_sheet)[3:(dim(data_sheet)[2]-1)],
                                "END")]

##  3. Replace each NA in each line with the next non-NA value ----------------
data_sheet <- as.data.frame(t(apply(data_sheet, 1, na.locf, fromLast = TRUE)))


### STEP 2: DERIVE GRAPH FROM PROGRESSION TABLE -------------------------------

##  1. Generate progression table
#      Number of successions: subtract ID and 1 because there's one
#      succession less than vertices
succession <- paste("suc", 1:(length(data_sheet[1,])-2), sep="")

emp_pt <- data.frame()
for (i in 1:length(succession)) {
  suc <- data.frame(ID=c(), from=c(), to=c())
  suc <- data.frame(ID=1:N,
                    from=data_sheet[,i+1],
                    to=data_sheet[,i+2])
  assign(succession[i], suc)
}

emp_pt <- data.frame()
for(i in 1:length(succession)) {
  aux <- eval(parse(text = paste0("suc", i)))
  emp_pt <- rbind(emp_pt, aux)
}

##  2. Derive empirical individual graph (graph_indi) from progression table
emp_pt$to <- factor(emp_pt$to,
                    c("BEGIN",
                      # Next row: 3 till 12, i.e., no. of variables
                      colnames(data_sheet)[3:(length(data_sheet
                                                     [1,])-1)],
                      "END"))
emp_pt$from <- factor(emp_pt$from,
                      c("BEGIN",
                        colnames(data_sheet)[3:(length(data_sheet
                                                       [1,])-1)],
                        "END"))

graph_indi <- vector("list", N)
for(i in 1:N) {
  eg <- emp_pt[emp_pt$ID == i &
                 emp_pt$from != emp_pt$to,
               c("from", "to")]
  graph_indi[[i]] <- graph_from_data_frame(as.matrix(eg))
}

##  3. Reduce empirical individual graph to empirical sample graph 
#      (graph_sample)
graph_sample <- Reduce("+", graph_indi)



### STEP 3: ASSIGN IMPORTANT GRAPH, VERTEX AND EDGE ATTRIBUTES TO GRAPHS ------

##  1. Graph attributes
#   Assignment of respondent ID's to the empirical individual graph
for(i in 1:N) {
  graph_indi[[i]] <- set_graph_attr(graph_indi[[i]], 
                                    name = "ID",
                                    value = data$ID[i])
}

##  2. Edge attributes
#      - Assignment of filter instructions to the empirical individual graph
#        Generate edges of theoretical graph model
#load("quest_graph.RData")  # if required
quest_pt$edge <- paste(quest_pt$FROM, "|", quest_pt$TO, sep = "")

#        Generate edges of empirical graph
emp_pt$edge <- paste(emp_pt$from, "|", emp_pt$to, sep = "")
emp_pt$filter <- NA

for(k in 1:length(quest_pt[,1])) {
  idx <- which(quest_pt$edge[k] == emp_pt$edge)
  
  for(j in idx) {
    emp_pt[j, "filter"] <- as.character(quest_pt[k, "FILTER"])
  }
}

for(i in 1:N) {
  eg <- emp_pt[emp_pt$ID == i &
                 emp_pt$from != emp_pt$to,
               c("from", "filter", "to")]
  graph_indi[[i]] <- set_edge_attr(graph_indi[[i]],
                                   name = "Filter Instruction",
                                   E(graph_indi[[i]]),
                                   value = eg$filter)
}

#        Test
# edge_attr(graph_indi[[2]], "Filter Instruction")
# edge_attr(graph_indi[[6]], "Filter Instruction")


#      - Assignment of all given answers
#        List with all possible edges
quest_edges <- E(quest_graph)

for(i in 1:N) {
  for(j in 1:length(quest_edges)) {
    aux_var <- paste0("data$", unlist(strsplit(as_ids(quest_edges[j]), "|", 
                                                   fixed = TRUE))[1])
    
    if(as_ids(quest_edges[j]) %in% as_ids(E(graph_indi[[i]]))) {
      graph_indi[[i]] <- set_edge_attr(graph_indi[[i]], "Given Answer",
                                       E(graph_indi[[i]])[as_ids(quest_edges[j])],
                                       value = as.character(eval(parse(text = aux_var)))[i])
    }
  }
}

#        Test
# edge_attr(graph_indi[[2]], "Given Answer")
# edge_attr(graph_indi[[6]], "Given Answer")


##  3. Vertex Attributes
#      - Assignment of variable distribution and names -------------------------------
attach(data)
var_names <- names(data)[2:length(data[1,])]
var_names_dist <- c()
for(i in 1:length(var_names)) {
  var_names_dist[i] <- paste0(var_names[i], "_dist")
  dist <- as.factor(eval(parse(text = var_names[i])))
  assign(var_names_dist[i], dist)
}

#      - Delete variables (distributions) that have not been traversed
pos <- c()
for(i in 1:length(var_names)) {
  pos[i] <- length(table(as.character(eval(parse(text = var_names[i])))))
}

if(any(pos == 0)) {
  var_names_dist <- var_names_dist[-which(pos == 0)]
}

dist <- c()
for(i in 1:length(var_names_dist)) {
  dist[i] <- list(eval(parse(text = var_names_dist[i])))
}

detach(data)

#      - Assignment of variable names
if(any(pos == 0)) {
  var_names <- var_names[-which(pos==0)]
}
graph_sample <- set_vertex_attr(graph_sample, "Variable Name",
                                index = V(graph_sample)[var_names],
                                value = var_names)
graph_sample <- set_vertex_attr(graph_sample, "Variable Name",
                                index = V(graph_sample)[c("BEGIN", "END")],
                                value = c("BEGIN", "END"))

#        Test
# vertex_attr(graph_sample, "Variable Name")[[2]]  # sex

#      - Assignment of variable distributions
graph_sample <- set_vertex_attr(graph_sample, "Variable Distribution",
                                index = V(graph_sample)[var_names],
                                value = dist)
graph_sample <- set_vertex_attr(graph_sample, "Variable Distribution",
                                index = V(graph_sample)[c("BEGIN", "END")],
                                value = c(NA, NA))

#        Test
# table(vertex_attr(graph_sample, "Variable Distribution")[[2]])  # 99 101



###############################################################################
### 2.3 Graph properties                                                    ###
###############################################################################

##  KEY PROPERTIES ------------------------------------------------------------

#   1. All possible paths
pos_paths <- all_simple_paths(graph_sample,
                              from = V(graph_sample)["BEGIN"],
                              to = V(graph_sample)["END"])
pos_paths <- pos_paths[order(sapply(pos_paths, length), decreasing = TRUE)]

#      Number of all possible paths
(path_count <- length(pos_paths))     # 5

pos_paths <- sapply(pos_paths, function(x) paste(as_ids(x), collapse = "-"))

#   2. All traversed paths
trav_path_vec <- sapply(graph_indi,
                        function(x) all_simple_paths(x, from = V(x)["BEGIN"],
                                                     to = V(x)["END"]))
trav_path_vec <- sapply(trav_path_vec, 
                        function(x) paste(as_ids(x), collapse = "-"))

#      Assign corresponding path number from theoretical path as name
for(i in 1:length(trav_path_vec)) {
  if(trav_path_vec[i] %in% graph_attr(quest_graph, "All Possible Paths")) {
    names(trav_path_vec)[i] <- which(graph_attr(quest_graph, "All Possible Paths")
                                     %in% trav_path_vec[i])
  } else {
    names(trav_path_vec)[i] <- 0
  }
}

#      Theoretical path number and number of all traversed paths
trav_path <- names(table(names(trav_path_vec)))
length(trav_path)     # 5

#   3. Total path distribution
trav_path_dist <- paste(table(as.numeric(names(trav_path_vec))), collapse = "-")

#      Number of traversed paths (incl. impossible paths)
trav_path_num <- length(trav_path)


#      Add path number as graph attribute to empirical individual graph
for(i in 1:dim(data)[1]) {
  graph_indi[[i]] <- set_graph_attr(graph_indi[[i]], "Path Number",
                                    trav_path_vec[i])
}

#      Test
# graph_attr(graph_indi[[1]], "Path Number") # 2
# graph_attr(graph_indi[[6]], "Path Number") # 1

#      Add path number as variable in data set
data$trav_paths <- names(trav_path_vec)

#      Add path distribution as graph attribute to empirical sample graph
graph_sample <- set_graph_attr(graph_sample, "Path Distribution",
                               table(names(trav_path_vec)))

#      Generate filter pattern
filter_pat <- paste(sort(as.numeric(unique(names(trav_path_vec)))), 
                    collapse = "-")

#      Add filter pattern as attributes to the empirical individual and 
#      empirical sample graph
for(i in 1:dim(data)[1]) {
  graph_indi[[i]] <- set_graph_attr(graph_indi[[i]], name = "Filter Pattern",
                                    value = filter_pat)
  graph_sample     <- set_graph_attr(graph_sample, name = "Filter Pattern", 
                                        value = filter_pat)
}

#      Test
# graph_attr(graph_sample, "Filter Pattern")
# graph_attr(graph_indi[[1]], "Filter Pattern")



##  FURTHER PROPERTIES --------------------------------------------------------

# Plot empirical graph
# do, if required



### Save empirical graphs of scenario 2
#   Note: Change file name
save(graph_indi, graph_sample, file = "s2_empirical_graphs.RData")




###############################################################################
### 3.0 Tests                                                               ###
###############################################################################

# See Test Plan (Stark and Zinn 2021)

### FILTER STRUCTURE (FS) -----------------------------------------------------
##  No preconditions

##  FS1: IMPOSSIBLE PATHS
#   -> Check, if the filter pattern of the empirical sample graph includes a
#      Null, i.e., "path(s) 0".
graph_attr(graph_sample, "Filter Pattern")
#   -> IMPLICATION: Filter pattern includes no "path(s) 0", that means there
#                   are no impossible paths.
#   -> NEXT STEPS:  Continue with test case FS2!

#   FS2: INCORRECT FILTER PROGRAMMING
#   -> 1. Check, if there are edges in the empirical individual graph 
#         connecting the correct vertices but its edge attributes "Given 
#         Answer" do not conform with the edge attributes "All possible Answer
#         Options" in the questionnaire graph by comparing these two graphs.
#      2. Identify these edges.

#   -> Search for 'filter-relevant variable', which answer determines the
#      follow-up question
x <- edge_attr(quest_graph, "Filter Instructions")
x_aux <- x
for(i in 1:length(x)) {
  if(x[i] != "ALL") {
    x_aux[i] <- gsub('^|\\s* =.*$', '', x[i])
  }
}

#   -> Search position of variable, whose 'filter-relevant variable' does not
#      conform with itself AND not with the filter 'ALL'
pos <- c()
for(i in 1:length(x_aux)) {
  pos[i] <- (x_aux[i] != "ALL") & (as.character(quest_pt$FROM[i]) != x_aux[i])
}

#   -> Determine position of 'filter-relevant variable'
pos_aux <- which(as.character(quest_pt$FROM) == unique(x_aux[pos]))

#   -> Determine incorrect filter programming
err_edges_tab <- matrix(ncol = 5)
N <- length(graph_indi)
for(i in 1:N) {
  for(j in 1:length(E(quest_graph))) {
    
    # Variable and 'filter-relevant variable' conform with each other OR 
    # filter == 'ALL'
    if(as_ids(E(quest_graph)[j]) %in% as_ids(E(graph_indi[[i]])) &
       (x_aux[j] == "ALL" | as.character(quest_pt$FROM[j]) == x_aux[j])) {
      # Given Answer        
      x <- edge_attr(graph = graph_indi[[i]], 
                     name  = "Given Answer", 
                     index = E(graph_indi[[i]])[as_ids(E(quest_graph)[j])]) %in%
        # All possible Answer Options
        unlist(strsplit(unlist(edge_attr(quest_graph, "All Possible Answer Options", 
                                         E(quest_graph)[E(quest_graph)[j]])),
                        "; ", fixed = TRUE)) 
    }
    
    # Variable and 'filter-relevant variable' do not conform with each other
    # AND filter != 'ALL'
    if(as_ids(E(quest_graph)[j]) %in% as_ids(E(graph_indi[[i]])) &
       (x_aux[j] != "ALL" & as.character(quest_pt$FROM[j]) != x_aux[j])) {
      # Given Answer
      x <- edge_attr(graph = graph_indi[[i]],
                     name  = "Given Answer",
                     index = E(graph_indi[[i]])[as_ids(E(quest_graph)[j])]) %in%
        # All possible Answer Options
        unlist(strsplit(unlist(edge_attr(quest_graph,
                                         "All Possible Answer Options",
                                         E(quest_graph)[pos_aux])),
                        "; ", fixed = TRUE))
    }
    
    if(x == FALSE) {
      
      aux_mat <- matrix(ncol = 5)
      aux_mat[,1] <- graph_attr(graph_indi[[i]], "ID")
      aux_mat[,2] <- i
      aux_mat[,3] <- E(quest_graph)[j]
      aux_mat[,4] <- as_ids(E(quest_graph)[j])
      aux_mat[,5] <- edge_attr(graph_indi[[i]], "Given Answer", 
                               E(graph_indi[[i]])[as_ids(E(quest_graph)[j])])
      
      err_edges_tab <- rbind(err_edges_tab, aux_mat)
    }
  }
}

colnames(err_edges_tab) <- c("Target ID", "Graph No.", "Filter No.", 
                             "Filter Instruction", "Given Answer")
(err_edges_tab <- err_edges_tab[-1,])
length(err_edges_tab)
#        Save erroneous edges as excel-table, if erroneous edges exist
# write.xlsx(x = err_edges_tab, file = "erroneous_edges.xlsx")

#   -> If erroneous edges exist, identify unique wrong edges
(err_edges <- unique(err_edges_tab[,4]))
length(err_edges)
#   -> IMPLICATION: There exist no erroneous edges.
#   -> NEXT STEPS:  Continue with test case P1!



### PATHS ---------------------------------------------------------------------
##  Preconditions: No impossible paths and no incorrect filter programming

##  P1: TOO-LOW PATH FREQUENCIES
#   -> Check, if single path frequencies fall below a pre-specified benchmark 
#      value (default value is n=30).
graph_attr(graph_sample, "Path Distribution") > 30
#   -> IMPLICATION: The first two path frequencies fall below the benchmark 
#                   value, i.e. there are redundant paths and too-low case 
#                   numbers hindering feasible statistical analyses of 
#                   sub-populations. In this scenario, there are too less
#                   smokers in the data set.
#   -> NEXT STEPS:  Stop testing procedure & rethink/adapt questionnaire design!
