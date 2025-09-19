library(bnlearn)
library(gRain)

data(asia)

# train/test indices.
n_samples <- nrow(asia)
tr_size   <- n_samples * .8
all_index <- seq(1, n_samples) 
tr_index  <- sample(all_index, size = tr_size)
te_index  <- setdiff(all_index, tr_index)

# train/test data.
tr <- asia[tr_index, ]
te <- asia[te_index,]

# set structure
dag <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

# fit parameters
fitted_bnlearn <- bn.fit(dag, data = tr)

# inference is done using gRain
fitted_grain <- as.grain(fitted)

# why are these different?
fitted_grain |> qgrain(nodes="S", type="conditional")
fitted_bnlearn$S