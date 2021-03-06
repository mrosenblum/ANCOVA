# generate data for demonstration -----
# outcome y is generated by an ANCOVA model. The true average treatment effect is 0.
set.seed(1)
n <- 500
p <- 9
cov_w <- matrix(c(1.0, 0.0, 0.1, 0.0, 0.0, 0.1, 0.0, 0.1, 0.0,
                  0.0, 1.0, 0.2, 0.1, 0.0, 0.3, 0.1, 0.0, 0.0,
                  0.1, 0.2, 1.0, 0.5, -0.2, 0.4, 0.3, 0.1, 0.1,
                  0.0, 0.1, 0.5, 1.0, -0.2, 0.3, 0.1, 0.1, 0.1,
                  0.0, 0.0, -0.2, -0.2, 1.0, -0.1, -0.1, 0.0, 0.0,
                  0.1, 0.3, 0.4, 0.3, -0.1, 1.0, 0.3, 0.0, 0.0,
                  0.0, 0.1, 0.3, 0.1, -0.1, 0.3, 1.0, 0.0, 0.1,
                  0.1, 0.0, 0.1, 0.1, 0.0, 0.0, 0.0, 1.0, 0.1,
                  0.0, 0.0, 0.1, 0.1, 0.0, 0.0, 0.1, 0.1, 1.0), nrow = p, ncol = p)
w <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = cov_w)
a <- rbinom(n, 1, 0.5)
beta <- c(1.43, -0.10, -8.05, -0.52, 0.04, -1.00, -1.56, 0.80, -0.41)
y <- w %*% beta + rnorm(n, 0, 10)

# estimating average treatment effect ------
source("Data_Preprocessing_and_Analysis/adjust_estimator.R")
# unadjusted estimator with standard error (unadj_s.d.) and C.I. (unadj_c.i._upper and unadj_c.i._lower)
unadj_est <- adjust_estimator(y, a)
unadj_s.d. <- adjust_estimator(y, a, sd.cal = T)[2]
unadj_c.i._upper <- unadj_est + abs(qnorm(0.025)) * unadj_s.d.
unadj_c.i._lower <- unadj_est - abs(qnorm(0.025)) * unadj_s.d.
# ANCOVA estimator with standard error (ancova_s.d.) and C.I. (ancova_c.i._upper and ancova_c.i._lower)
ancova_est <- adjust_estimator(y, a, w, method = "ANCOVA")
ancova_s.d. <- adjust_estimator(y, a, w, method = "ANCOVA", sd.cal = T)[2]
ancova_c.i._upper <- ancova_est + abs(qnorm(0.025)) * ancova_s.d.
ancova_c.i._lower <- ancova_est - abs(qnorm(0.025)) * ancova_s.d.
# summary of results
data.frame(unadj_est = round(unadj_est, 2),
           unadj_s.d. = round(unadj_s.d., 2),
           unadj_c.i. = paste0("(", round(unadj_c.i._lower, 2), ", ", round(unadj_c.i._upper, 2), ")"),
           ancova_est = round(ancova_est, 2), 
           ancova_s.d. = round(ancova_s.d., 2),
           ancova_c.i. =  paste0("(", round(ancova_c.i._lower, 2), ", ", round(ancova_c.i._upper, 2), ")"),
           variance_reduction = round(1 - ancova_s.d.^2/unadj_s.d.^2, 2),
           RE = round(unadj_s.d.^2/ancova_s.d.^2, 2)
)

