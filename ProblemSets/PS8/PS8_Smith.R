#Question 4

#set seed
set.seed(100)

#create matrix X
N <- 100000
K <- 10

X <- matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1)
X <- cbind(1, X)

#error term
eps <- rnorm(N, mean = 0, sd = 0.5)

#beta term
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)


#y vector
Y <- X %*% beta + eps

#Question 5 - generate OLS estimate of beta
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y
beta_ols
#beta_ols prints values are that very close to original beta

#Question 6 - generate OLS estimate of beta using gradient descent
# Initialize beta_hat with zeros
beta_hat <- rep(0, K)

# Set the learning rate (step size)
learning_rate <- 0.0000003

# Set the number of iterations
num_iterations <- 100000

# Perform gradient descent
for (i in 1:num_iterations) {
  gradient <- (2/N) * t(X) %*% (X %*% beta_hat - Y)
  
  # Update beta_hat using the gradient and learning rate
  beta_hat <- beta_hat - learning_rate * gradient
}

print(beta_hat)

#Question 7 - 
#compute OLS using nloptr L-BFGS
# Define the objective function (sum of squared residuals)
objective_function <- function(beta) {
  residuals <- Y - X %*% beta
  return(sum(residuals^2))
}

# Define the gradient function
gradient_function <- function(beta) {
  residuals <- Y - X %*% beta
  return(-2 * t(X) %*% residuals)
}

# Set the initial values for beta
beta_init <- rep(0, K)

# Compute OLS estimate using L-BFGS algorithm
result_lbfgs <- nloptr(x0 = beta_init, eval_f = objective_function, eval_grad_f = gradient_function, opts = list("algorithm" = "NLOPT_LD_LBFGS"))
beta_ols_lbfgs <- result_lbfgs$solution

# Compute OLS estimate using Nelder-Mead algorithm
result_nm <- nloptr(x0 = beta_init, eval_f = objective_function, opts = list("algorithm" = "NLOPT_LN_NELDERMEAD"))
beta_ols_nm <- result_nm$solution

# Print the results
print(beta_ols_lbfgs)
print(beta_ols_nm)


 #Question 8 - computer Maximum Likelihood Estimate (MLE)
# Define the negative log-likelihood function
negative_log_likelihood <- function(theta, Y, X) {
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  residuals <- Y - X %*% beta
  ll <- -sum(dnorm(residuals, mean = 0, sd = sig, log = TRUE))
  return(ll)
}

# Define the gradient function
gradient <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

# Set the initial values for theta (beta and sigma)
theta_init <- c(rep(0, K), 1)

# Compute MLE using L-BFGS algorithm
result_mle <- nloptr(x0 = theta_init, eval_f = negative_log_likelihood, eval_grad_f = gradient, opts = list("algorithm" = "NLOPT_LD_LBFGS"), Y = Y, X = X)
beta_mle <- result_mle$solution[1:K]
sigma_mle <- result_mle$solution[K + 1]

# Print the results
print(beta_mle)
print(sigma_mle)

#Question 9 - OLS using LM
library(modelsummary)
# Compute OLS estimate using lm()
model <- lm(Y ~ X - 1)

# Export the regression output to a .tex file
modelsummary(model, output = "regression_output.tex")


