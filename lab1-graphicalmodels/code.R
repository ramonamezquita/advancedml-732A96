library(bnlearn)
library(gRain)

# bnlearn leverages the modularity of BNs by:
# 1. learning the structure of the network, or creating one manually, gives an object of class bn that encodes G;
# 2. learning the parameters for a given structure starts from a bn object and gives an object of class bn.fit that encodes (G, Theta);
# 3. inference takes an object of class bn.fit


# 1. Non equivalent Bayesian networks
data(asia)

iss      <- c(5, 10)
bic_dag  <- hc(asia, score = "bic")
bde_dags <- list()
for (i in 1:length(iss)) {
  bde_dag       <- hc(asia, score = "bde", iss = iss[i])
  bde_dags[[i]] <- bde_dag
}

compare(bic_dag,  bde_dags[[1]])

# compare bic_dag vs each bde_dag
cmp1 <- compare(bic_dag, bde_dags[[1]])
cmp2 <- compare(bic_dag, bde_dags[[2]])
cmp3 <- compare(bde_dags[[1]], bde_dags[[2]])

# results in a single data.frame
comparison_table <- data.frame(
  Comparison = c("BIC vs BDeu(iss=5)", "BIC vs BDeu(iss=10)", "BDeu(iss=5) vs BDeu(iss=10)"),
  TP = c(cmp1[["tp"]], cmp2[["tp"]], cmp3[["tp"]]),
  FP = c(cmp1[["fp"]], cmp2[["fp"]], cmp3[["fp"]]),
  FN = c(cmp1[["fn"]], cmp2[["fn"]], cmp3[["fn"]])
)

knitr::kable(comparison_table, caption = "Comparison of Bayesian networks using `compare()`")

par(mfrow = c(1, 2))
graphviz.compare(bic_dag, bde_dags[[1]])



# 2. Posterior probability distribution

# train/test indices.
n_samples <- nrow(asia)
tr_size   <- n_samples * .8
all_index <- seq(1, n_samples) 
tr_index  <- sample(all_index, size = tr_size)
te_index  <- setdiff(all_index, tr_index)

# train/test data.
tr <- asia[tr_index, ]
te <- asia[te_index,]

# learn structure and fit parameters.
fitted <- bn.fit(hc(asia), tr)

# convert to grain object.
grain_bn <- as.grain(fitted)

# Function to classify a single test case.
# 
# Args
# ----
# sample:   A single sample to classify.
# bn:       The gRain network object.
# target:   The target node.
# evidence: The evidence nodes.
classify <- function(sample, bn, target, evidence) {
  evidence <- as.list(sample[evidence])
  query <- bn |> setEvidence(evidence = evidence) |> querygrain(nodes=target)
  query <- query[[target]]
  ifelse(query["yes"] >= query["no"], "yes", "no")
}

target   <- "S"
evidence <- setdiff(names(te), c(target))  # all except target.
y_pred   <- apply(te, 1, classify, bn = grain_bn, target = target, evidence = evidence)

y_true  <- te$S
cmatrix <- table(Predicted = y_pred, True = y_true)
accuracy <-  sum(diag(cmatrix)) / sum(cmatrix)
knitr::kable(cmatrix)


# 3. Classifying using the Markov blanket

target   <- "S"
evidence <- mb(fitted, "S")  # the Markov blanket of S.
y_pred   <- apply(te, 1, classify, bn = grain_bn, target = target, evidence = evidence)
y_true  <- te$S

cmatrix <- table(Predicted = y_pred, True = y_true)
knitr::kable(cmatrix)


# 4. Naive Bayes classifier

target  <- "S"
arc_set <- cbind("S", setdiff(names(tr), c(target))) 
nb       <- empty.graph(names(tr))  # initialize empty graph.
arcs(nb) <- arc_set
graphviz.plot(nb)

fitted_nb <- bn.fit(nb, tr)

grain_bn <- as.grain(fitted_nb)
target   <- "S"
evidence <- setdiff(names(te), c(target))  # all except target.
y_pred   <- apply(te, 1, classify, bn = grain_bn, target = target, evidence = evidence)

y_true  <- te$S
cmatrix <- table(Predicted = y_pred, True = y_true)
knitr::kable(cmatrix)




