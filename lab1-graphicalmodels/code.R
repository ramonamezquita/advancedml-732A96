library(bnlearn)
library(gRain)

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
# sample: A single sample to classify.
# bn: The gRain network object.
classify <- function(sample, bn, target_node) {
  evidence <- as.list(sample)
  evidence[[target_node]] <- NULL  # remove the target variable
  query <- querygrain(setEvidence(bn, evidence = evidence), nodes = target_node)[[target_node]]
  predicted <- ifelse(query["yes"] >= query["no"], "yes", "no")
  return(predicted)
}

y_pred <- apply(te, 1, classify, bn = grain_bn, target_node = "S")
y_true <- te$S
cmatrix <- table(Predicted = y_pred, True = y_true)




