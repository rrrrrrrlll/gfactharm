set.seed(0)

# Set sample size
N <- 3000

# Means and variances of G for datasets A and B
mean_G_A <- 0
var_G_A  <- 1
mean_G_B <- 1
var_G_B  <- 0.8

# Factor loadings from G to first-order factors
lambda_G_f <- c(f1 = 0.8, f2 = 0.7, f3 = 0.9)
lambda_f1A_Y <- c(1, 2, 3)
lambda_f1B_Y <- c(2, 2, 2)
lambda_f2_Y <- c(1, 2, 3)
lambda_f3_Y <- c(1, 2, 3)
lambda_f3_Y_dif <- c(2, 4, 6)

# Function to compute residual variances
compute_residual_variance <- function(lambda, var_factor) {
    1 - lambda^2 * var_factor
}

## --- Data Generation: Reference data(A) --- ##

# Generate dataset A
## Suppose marginally each first order factor has variance 1
G_A <- rnorm(N, mean = mean_G_A, sd = sqrt(var_G_A))

# Generate first-order factors for A
var_e_f_A <- compute_residual_variance(lambda_G_f, var_G_A)
f1_A <- lambda_G_f["f1"] * G_A + rnorm(N, 0, sqrt(var_e_f_A["f1"]))
f2_A <- lambda_G_f["f2"] * G_A + rnorm(N, 0, sqrt(var_e_f_A["f2"]))
f3_A <- lambda_G_f["f3"] * G_A + rnorm(N, 0, sqrt(var_e_f_A["f3"]))

# Generate observed variables for f1 in A
var_e_Y_f1A <- c(1,2,3)
Y1_A <- sapply(1:3, function(i) {
    lambda_f1A_Y[i] * f1_A + rnorm(N, 0, sqrt(var_e_Y_f1A[i]))
})

# Generate observed variables for f2 in A
var_e_Y_f2 <- c(1,2,3)
Y2 <- sapply(1:3, function(i) {
    lambda_f2_Y[i] * f2_A + rnorm(N, 0, sqrt(var_e_Y_f2[i]))
})

# Generate observed variables for f3 in A
var_e_Y_f3 <- c(1,2,3)
Y3 <- sapply(1:3, function(i) {
    lambda_f3_Y[i] * f3_A + rnorm(N, 0, sqrt(var_e_Y_f3[i]))
})

# Combine observed variables into dataset A
data_A <- data.frame(ya1_1 = Y1_A[,1], ya1_2 = Y1_A[,2], ya1_3 = Y1_A[,3],
                     y2_1 = Y2[,1], y2_2 = Y2[,2], y2_3 = Y2[,3],
                     y3_1 = Y3[,1], y3_2 = Y3[,2], y3_3 = Y3[,3],
                     f1 = f1_A, f2 = f2_A, f3 = f3_A, G = G_A)


## --- Data Generation: B NO DIF --- ##

# Generate dataset B
G_B <- rnorm(N, mean = mean_G_B, sd = sqrt(var_G_B))

# Generate first-order factors for B
var_e_f_B <- c(f1 = 0.4, f2 = 0.49, f3 = 0.16)
f1_B <- lambda_G_f["f1"] * G_B + rnorm(N, 0, sqrt(var_e_f_B["f1"]))
f2_B <- lambda_G_f["f2"] * G_B + rnorm(N, 0, sqrt(var_e_f_B["f2"]))
f3_B <- lambda_G_f["f3"] * G_B + rnorm(N, 0, sqrt(var_e_f_B["f3"]))

# Generate observed variables for f1 in B
var_e_Y_f1B <- c(1,2,3)
Y1_B <- sapply(1:3, function(i) {
    lambda_f1B_Y[i] * f1_B + rnorm(N, 0, sqrt(var_e_Y_f1B[i]))
})

# Generate observed variables for f2 in B (same as in A)
Y2_B <- sapply(1:3, function(i) {
    lambda_f2_Y[i] * f2_B + rnorm(N, 0, sqrt(var_e_Y_f2[i]))
})

# Generate observed variables for f3 in B (same as in A)
Y3_B_no <- sapply(1:3, function(i) {
    lambda_f3_Y[i] * f3_B + rnorm(N, 0, sqrt(var_e_Y_f3[i]))
})

# Combine observed variables into dataset B
data_B_no <- data.frame(yb1_1 = Y1_B[,1], yb1_2 = Y1_B[,2], yb1_3 = Y1_B[,3],
                     y2_1 = Y2_B[,1], y2_2 = Y2_B[,2], y2_3 = Y2_B[,3],
                     y3_1 = Y3_B_no[,1], y3_2 = Y3_B_no[,2], y3_3 = Y3_B_no[,3],
                     f1 = f1_B, f2 = f2_B, f3 = f3_B, G = G_B)


## --- Data Generation: B UNI DIF and UNUNI DIF --- ##

# Generate observed variables for f3 in B with intersection 1
Y3_B_uni <- sapply(1:3, function(i) {
    lambda_f3_Y[i] * f3_B + rnorm(N, 3, sqrt(var_e_Y_f3[i]))
})

# Combine observed variables into dataset B
data_B_uni <- data.frame(yb1_1 = Y1_B[,1], yb1_2 = Y1_B[,2], yb1_3 = Y1_B[,3],
                        y2_1 = Y2_B[,1], y2_2 = Y2_B[,2], y2_3 = Y2_B[,3],
                        y3_1 = Y3_B_uni[,1], y3_2 = Y3_B_uni[,2], y3_3 = Y3_B_uni[,3],
                        f1 = f1_B, f2 = f2_B, f3 = f3_B, G = G_B)

# Generate observed variables for f3 in B with intersection 1 and different loadings
Y3_B_nonuni <- sapply(1:3, function(i) {
    lambda_f3_Y_dif[i] * f3_B + rnorm(N, 3, sqrt(var_e_Y_f3[i]))
})

# Combine observed variables into dataset B
data_B_nonuni <- data.frame(yb1_1 = Y1_B[,1], yb1_2 = Y1_B[,2], yb1_3 = Y1_B[,3],
                         y2_1 = Y2_B[,1], y2_2 = Y2_B[,2], y2_3 = Y2_B[,3],
                         y3_1 = Y3_B_nonuni[,1], y3_2 = Y3_B_nonuni[,2], y3_3 = Y3_B_nonuni[,3],
                         f1 = f1_B, f2 = f2_B, f3 = f3_B, G = G_B)

data_a = data_A[,1:9]
data_b_no = data_B_no[,1:9]
data_b_uni = data_B_uni[,1:9]
data_b_nonuni = data_B_nonuni[,1:9]

write.csv(data_a,        'dev/A.csv',       row.names = FALSE)
write.csv(data_b_no,     'dev/BNO.csv',     row.names = FALSE)
write.csv(data_b_uni,    'dev/BUNI.csv',    row.names = FALSE)
write.csv(data_b_nonuni, 'dev/BNONUNI.csv', row.names = FALSE)
