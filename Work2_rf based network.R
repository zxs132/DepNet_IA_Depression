########## Uploading packages ##########

library(reshape)
library(dplyr)
library(ranger)
library(igraph)
library(visNetwork)
library(ztable)


########## Uploading & reshaping data set ##########
setwd("~/Desktop/Yonsei/Semester_2/Probabilistic Graphical Model/Final")

iadata <- read.csv("iadata.csv")

vars_timedep <-c("mdep", "IA", "smart", "happy", "sleep", "health", 
                     "CBCL", "family", "income", "empstatus")

iadata2 <- reshape(iadata, timevar = "year", idvar = "N_ID", 
                   v.names = vars_timedep, direction = "wide")


## Subsetting data from the 10th wave survey
d10 <- iadata2 %>% select(peridep, gender, ends_with("2017")) %>%
  
  transmute_all(., as.factor)


## Subsetting data from the 10th and 12th wave survey
d12 <- iadata2 %>% select(-N_ID) %>%
  
  transmute_all(., as.factor)



########## Tuning RFs ##########

tuning <- function(data, target) { # requires (dataframe, character))
  
  if(!require(dplyr)) { install.packages("dplyr"); library(dplyr) }
  if(!require(ranger)) { install.packages("ranger"); library(ranger) }
  
  
  p <- dim(data)[2] - 1 # number of predictors
  
  f = as.formula(paste(target, '~.'))
  
  
  ## creating a hyper parameter grid
  hyper_grid <- expand.grid(mvar = seq(1, floor(sqrt(p))*2, by = 1),
                            ntree = seq(100, 1000, by = 50), 
                            OOB_error = 0)
  
  
  for (i in 1:nrow(hyper_grid)) {
    
    rf <- ranger(formula = f, data = data,
                 num.trees = hyper_grid$ntree[i],
                 mtry = hyper_grid$mvar[i],
                 respect.unordered.factors = "partition",
                 seed = 123)
    
    hyper_grid$OOB_error[i] <- rf$prediction.error
    
  }
  
  
  ## finding optimal values for mtry and min.node.size
  hyper_grid.ordered <- hyper_grid %>% arrange(OOB_error)
  
  mtry.best <- hyper_grid.ordered[1,]$mvar
  
  ntree.best <- hyper_grid.ordered[1,]$ntree
  
  
  ## computing impurity importance scores
  rf_mdg <- ranger(formula = f, data = data,
                   num.trees = ntree.best,
                   mtry = mtry.best, 
                   respect.unordered.factors = "partition",
                   seed = 123,
                   importance = "impurity")
  
  
  #computing Altmann's p-values based on the impurity importance scores
  
  set.seed(123)
  mdg_df <- importance_pvalues(rf_mdg, method = "altmann",
                               num.permutations = 200,
                               formula = f, data = data) 
  
  # can used importance = 'impurity_corrected' option for the actual impurity reduction (AIR)
  
  results <- list("mtry.best" = mtry.best, 
                  "ntree.best" = ntree.best,
                  "mdg" = mdg_df[,1], 
                  "alt" = mdg_df[,2])
  
  return(results)
  
}



########## Executing the iteration for d10 and d12 ##########

varnames <- d12 %>% colnames

n <- length(varnames)

## creating an empty array to save the p-values
alt_p <- array(dim = c(n, n), dimnames = list(varnames, varnames))


targets17 <- d10 %>% select(ends_with("2017")) %>% colnames 
targets19 <- d12 %>% select(ends_with("2019")) %>% colnames 


for (name in targets17) {
  
  results <- tuning(d10, name)
  
  alt_p[names(results$alt), name] <- results$alt
  
}

for (name in targets19) {
  
  results <- tuning(d12, name)
  
  alt_p[names(results$alt), name] <- results$alt
  
}

## saving p-values in an empty array
write.table(alt_p, "altmann_pvalues.csv", na = "", sep = ",", 
            qmethod = "double", col.names = NA)

########## Extracting data for cytoscape generation ###########
setwd("~/Desktop/Yonsei/Semester_2/Probabilistic Graphical Model/Final")
p <- read.csv("altmann_pvalues.csv", sep = ",", row.names = 1, header = T)
labels <- rownames(p)
new_labels <- c("Peripartum depression", "Gender", "Mother's current depression", "Internet addiction", "Having a smartphone", "Happiness", "Sleep duration", "Health status", 
                "CBCL (psychiatric symptoms)", "Family composition", "Household income", "Parents' employment status")
node.name.labels <- c(new_labels, new_labels[3:length(new_labels)])
new_labels <- c(new_labels, paste0(new_labels[3:length(new_labels)], 2019))


### Node names for classification
node.name1 <- c()
node.name2 <- c()
pval <- c()

for(i in 1:nrow(p)){
  for(j in 1:ncol(p)){
    if(i < j){
      if(!is.na(p[i,j])){
         if(p[i,j] < 0.05){
            node.name1 <- c(node.name1, labels[i])
            node.name2 <- c(node.name2, labels[j])
            pval <- c(pval, p[i, j])
         } 
      }
    }
  }
} 

### Generating node names in cytoscape
new.node.name1 <- c()
new.node.name2 <- c()
node.name.labels1 <- c()
node.name.labels2 <- c()
for(i in 1:length(labels)){
  if(labels[i] %in% node.name1){
    ind <- which(node.name1 == labels[i])
    new.node.name1[ind] <- new_labels[i]
    node.name.labels1[ind] <- node.name.labels[i]
  }
  if(labels[i] %in% node.name2){
    ind <- which(node.name2 == labels[i])
    new.node.name2[ind] <- new_labels[i]
    node.name.labels2[ind] <- node.name.labels[i]
  }
}

data_for_cytoscape <- data.frame(node1 = new.node.name1, node2 = new.node.name2, pval = pval, node_name1 = node.name.labels1, node_name2 = node.name.labels2)
year1 <- ifelse(substr(node.name1, nchar(node.name1), nchar(node.name1) + 1) == 9, 2019, ifelse(substr(node.name1, nchar(node.name1), nchar(node.name1) + 1) == 7, 2017, 2008))
year2 <- ifelse(substr(node.name2, nchar(node.name2), nchar(node.name2) + 1) == 9, 2019, ifelse(substr(node.name2, nchar(node.name2), nchar(node.name2) + 1) == 7, 2017, 2008))
data_for_cytoscape$year1 <- year1
data_for_cytoscape$year2 <- year2

ind1 <- c()
ind2 <- c()
for(i in 1:nrow(data_for_cytoscape)){
  ind1[i] <- which(data_for_cytoscape$node1[i] == node.name.labels)
  ind2[i] <- which(data_for_cytoscape$node2[i] == node.name.labels)
}
data_for_cytoscape$node.ind1 <- ind1
data_for_cytoscape$node.ind2 <- ind2

write.table(data_for_cytoscape, "~/Desktop/Yonsei/Semester_2/Probabilistic Graphical Model/Final/data_for_cytoscape.txt", quote = FALSE, sep ="\t", row.names = FALSE)

########## Visualizing the test results ##########

p <- read.csv("altmann_pvalues.csv", sep = ",", row.names = 1, header = T)

n <- dim(p); psig <- matrix(nrow = n, ncol = n)

psig[p <= 0.05] <- 1
psig[(p > 0.05 & p <= 1) | is.na(p)] <- 0

vars <- names(p); dimnames(psig) <- list(vars, vars)


## generate a network object from the adjacency matrix
net <- igraph::graph.adjacency(psig)

vid <- igraph::get.vertex.attribute(net, "name")

vid1 <- vid[grepl("2017", vid)]
vid2 <- vid[grepl("2019", vid)]

vlab1 <- unlist(strsplit(vid1, ".2017"))
vlab2 <- unlist(strsplit(vid2, ".2019"))

V(net)[1:2]$label <- vid[1:2]
V(net)[3:12]$label <- vlab1
V(net)[13:22]$label <- vlab2

V(net)$group <- c(rep("1", 2), rep("2", 10), rep("3", 10))

E(net)$width <- 2


## set layout coordinates
x <- c(3, 7, 
       4.25, 2.5, 4.25, 3, 7.5,  6.75, 5.5, 2, 1, 1.5, 
       4.25, 2.5, 4.25, 3, 7.5, 6.75, 5.5, 2, 1, 1.5)

y <- c(2, 2, 
       5, 5, 4, 4, 5, 4, 4, 4, 4, 5,
       9, 9, 8, 8, 9, 8, 8, 8, 8, 9)

layoutmatrix <- cbind(x, y)


## plot the network
lgd <- data.frame(label = c("2008 (Wave8)", "2017 (Wave10)", "2019 (Wave12)"),
                  shape = "dot",
                  size = 12,
                  color = c("darkcyan", "darkgoldenrod", "mediumpurple"),
                  font.size = 12,
                  shadow = c(T, T, T))

rfplot <- visNetwork::visIgraph(net, idToLabel = F) %>%
  
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layoutmatrix) %>%
  
  visEdges(shadow = T) %>%
  
  visNodes(shadow = T, font = list(size = 24)) %>%
  
  visGroups(groupname = "1", color = "darkcyan") %>%
  visGroups(groupname = "2", color = "darkgoldenrod") %>%
  visGroups(groupname = "3", color = "mediumpurple") %>%
  
  visLegend(useGroups = F, addNodes = lgd, stepY = 185, position = "right", width = .335)

png("IA_network.png")
rfplot; dev.off()


## Creating a table to save the raw p-values

# time-invariant variables
x.1 <- c("Peripartum depression", "Gender")

# time-dependent variables
x.2 <- c("Mother's current depression", "IA", "Having a smartphone", 
                 "Happiness", "Sleep duration", "Health", "CBCL", 
                 "Family composition", "Income", "Parents' employment status")

x <- c(x.1, x.2, paste0(x.2, "2"))

p <- as.matrix(p); dimnames(p) <- list(x, x)


ptable <- p[,-c(1, 2)] %>% ztable(colnames.bold = T, align = "ccccccccccccccccccccc") %>%
  
  addSigColor %>%
  
  addrgroup(rgroup = c("", "2017 (Wave 10)", "2019 (Wave 12)"), n.rgroup = c(2, 10, 10)) %>%
  
  addcgroup(cgroup = c("Target variable"), n.cgroup = 20) %>%
  
  addcgroup(cgroup = c("2017 (Wave 10)", "2019 (Wave 12)"), n.cgroup = c(10, 10)) %>%
  
  vlines(type = 1) %>%
  
  hlines(type = 1)

x <- ztable2flextable(ptable); x
