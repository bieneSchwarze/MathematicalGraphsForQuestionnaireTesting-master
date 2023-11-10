###############################################################################
### Using Mathematical Graphs for Questionnaire Testing: A simple Example   ###
###          R Program for graph generation and testing procedure           ###
###              Testing Scenario 1: Wrong filter programming               ###
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
load("s1_data.RData")



###############################################################################
### 2.2 Derivation of the empirical graphs from the empirical data          ###
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
#      Number of successions: subtract ID and 1 because there's one succession
#      less than vertices
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
#      - Assignment of variable distribution and names
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
# table(vertex_attr(graph_sample, "Variable Distribution")[[2]])  # 95 105



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
length(trav_path)     # 4

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
# graph_attr(graph_indi[[1]], "Path Number") # 0
# graph_attr(graph_indi[[6]], "Path Number") # 4

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

## Plot empirical sample graph ------------------------------------------------
#  Note: This piece of code plots the empirical sample graph for the simple
#        example. If you want to plot another graph, this code has to be
#        adapted to the graph you want to plot.
# Determine graph coordinates
coord <-           #BEGIN sex  edu  class smoker smofreq smostop smostopr subhealth END   smoever
  as.matrix(cbind(c(690,  690, 690, 690,  690,   560,    560,    690,     690,      1000, 820),
                  c(480,  420, 360, 300,  240,   200,    140,    100,     40,        240, 200)))
filter <- filter_fun(graph_sample)
V(graph_sample)$color <- ifelse(V(graph_sample) %in% filter,
                                   "lightblue2", "white")
V(graph_sample)$label.cex <- 0.8
E(graph_sample)$arrow.size <- 0.2
V(graph_sample)$label.color <- "black"


## Plot empirical individual graphs n_1 ---------------------------------------

coord_n1 <-        #BEGIN sex  edu  class smoker smofreq smostop smostopr subhealth END
  as.matrix(cbind(c(690,  690, 690, 690,  690,   560,    560,    690,     690,      1000),
                  c(480,  420, 360, 300,  240,   200,    140,    100,     40,        240)))

filter <- filter_fun(graph_sample)
V(graph_indi[[1]])$color <- ifelse(V(graph_indi[[1]]) %in% filter,
                                      "lightblue2", "white")
V(graph_indi[[1]])$label.cex <- 0.8
E(graph_indi[[1]])$arrow.size <- 0.2
V(graph_indi[[1]])$label.color <- "black"


## Plot empirical individual graphs n_4 ---------------------------------------
coord_n4 <-        #BEGIN sex  edu  class smoker smoever  subhealth END
  as.matrix(cbind(c(690,  690, 690, 690,  690,   820,     690,      1000),
                  c(480,  420, 360, 300,  240,   200,      40,       240)))
filter <- filter_fun(graph_sample)
V(graph_indi[[4]])$color <- c("white", "white", "white", "white",
                              "lightblue2", "lightblue2", "white", "white")
V(graph_indi[[4]])$label.cex <- 0.8
E(graph_indi[[4]])$arrow.size <- 0.2
V(graph_indi[[4]])$label.color <- "black"


## Plot empirical individual graphs n_59 --------------------------------------
coord_n59 <-       #BEGIN sex  edu  class smoker END
  as.matrix(cbind(c(690,  690, 690, 690,  690,   1000),
                  c(480,  420, 360, 300,  240,    240)))
filter <- filter_fun(graph_sample)
V(graph_indi[[59]])$color <- ifelse(V(graph_indi[[59]]) %in% filter,
                                    "lightblue2", "white")
V(graph_indi[[59]])$label.cex <- 0.8
E(graph_indi[[59]])$arrow.size <- 0.2
V(graph_indi[[59]])$label.color <- "black"


# Plot all graphs
par(mfrow = c(2,2))
plot(graph_sample, layout = coord, edge.color = "black", main = "Empirical sample graph scenario 1")
plot(graph_indi[[1]], layout = coord_n1, edge.color = "black", main = "Empirical individual graph ID 1 scenario 1")
plot(graph_indi[[4]], layout = coord_n4, edge.color = "black", main = "Empirical individual graph ID 4 scenario 1")
plot(graph_indi[[59]], layout = coord_n59, edge.color = "black", main = "Empirical individual graph ID 59 scenario 1")
par(mfrow = c(1,1))

### Save empirical graphs of scenario 1
save(graph_indi, graph_sample, file = "s1_empirical_graphs.RData")



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
#   -> Filter pattern includes "path(s) 0"
#   -> Number of impossible paths
graph_attr(graph_sample, "Path Distribution")
#   -> There are 84 impossible paths
#   -> IMPLICATION: There are "path(s) 0", that means there are impossible 
#                   paths, i.e., there are deviations between the programming 
#                   template and the actual programmed questionnaire. 
#   -> NEXT STEPS:  Continue with test case V1!



### VERTICES ------------------------------------------------------------------
##  Precondition: Impossible paths

#   -> Check, if there are fewer/more vertices in the empirical sample graph 
#      than in the questionnaire graph by comparing the number of vertices of
#      both graphs.
gorder(quest_graph); gorder(graph_sample)
#       -> There is one vertex less in the empirical sample graph than in the
#          questionnaire graph; identify this vertex.


##  V1: MISSING VERTICES
(miss_vert <- V(quest_graph)[!as_ids(V(quest_graph)) %in% as_ids(V(graph_sample))])
#   -> Question 5 about the smoking device (smodev) is missing
#   -> How to identify, if this question was not applicable or cannot be 
#      reached due to wrong filtering?
#   -> Find out previous question with the help of the questionnaire graph and
#      have a look at the variable distribution of the previous variable, then
#      check the filter instruction
neighbors(quest_graph, v = miss_vert, mode = "in")
table(vertex_attr(graph_sample, "Variable Distribution",
                  index = V(graph_sample)["smoker"]))
#   -> 84 respondents gave the answer option 1 "yes" and thus should have been
#      led to smodev
#   -> IMPLICATION: There is one missing vertex, i.e. there is one question
#                   that is missing or could not be answered due to either 
#                   wrongly programmed skip instructions or inadequate sample.
#   -> NEXT STEPS:  Stop testing procedure & correct questionnaire programming!

### The further analyses are for illustrating purposes! ###

##  V2: ADDITIONAL VERTICES IF EXISTING
(add_vert <- V(graph_sample)[!as_ids(V(graph_sample)) %in% as_ids(V(quest_graph))])
#   -> IMPLICATION: There are no additional vertices.
#   -> NEXT STEPS:  Continue with test case E1!



### EDGES ---------------------------------------------------------------------
##  Precondition: Impossible paths

#   -> Check, if there are fewer edges in the empirical sample graph than in
#      the questionnaire graph by comparing the number of edges of both graphs.
gsize(quest_graph); gsize(graph_sample)
#   -> There is one edge less in the empirical sample graph than in the 
#      questionnaire graph; identify this edge.

##  E1: MISSING EDGES
(sample_miss_edges <- E(quest_graph)[!as_ids(E(quest_graph)) %in% 
                                       as_ids(E(graph_sample))])
#   -> There are two missing edges 
#      (Note: In sum only one edge less is shown because there are two missing
#      edges due to the missing vertex smodev but there is also one additional 
#      edge smoker->smofreq due to the missing vertex smodev, see E2: 
#      additional edges)
#   -> Ignore edges containing the vertices that were not traversed (see above)
(quest_miss_edges <- c(incident(quest_graph, V(quest_graph)["smodev"], "all")))
#   -> There are two missing edges due to the missing vertex smodev: 
#      smodev->smofreq and smoker->smodev
(rest <- sample_miss_edges[which(!(sample_miss_edges %in% quest_miss_edges))])
#   -> There are no missing edges left, i.e., all missing edges are due to the
#      missing vertex
#   -> IMPLICATION: There are missing edges.
#   -> NEXT STEPS:  Stop testing procedure & correct questionnaire programming!

##  E2: ADDITIONAL EDGES
(add_edges <- E(graph_sample)[!as_ids(E(graph_sample)) %in% as_ids(E(quest_graph))])
#   -> There is one edge resp. filter instruction, which does not occur in the
#      theoretical graph. This additional edge is due to the missing vertex.
#   -> IMPLICATION: There is one additional edge.
#   -> NEXT STEPS:  Stop testing procedure & correct questionnaire programming!

# Note: There are interdependencies of errors!
