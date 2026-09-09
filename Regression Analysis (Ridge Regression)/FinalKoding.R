library(readxl)
library(lmtest)
library(nortest)
library(MASS)
library(glmnet)
library(car)

# ============================================================
# DATA PREPARATION
# ============================================================
data <- read_xlsx("dataset reganal.xlsx")

var <- c("Rasio_LabIPA_Baik", 
         "Persentase_guru_SMA_bersertifikat_2024",
         "Rasio_Guru_mengajar_diatas_5tahun",
         "Rasio_SMA_fasilitas_INTERNET_untuk_belajar",
         "Rasio_SMA_fasilitas_komputer_untuk_belajar",
         "Persentase_SMA_Akreditasi_A",
         "Rasio_Guru_lebih_dari_sama_S1",
         "Rasio_Angka_Partisipasi_Murni_SMA",
         "Rasio_Perpustakaan_Baik")

y <- as.vector(data$Rata_Rata_Nilai_TKA_SMA_2025)
X <- as.matrix(data[, var])
n <- length(y)

# ============================================================
# 1. OLS
# ============================================================
model3 <- lm(Rata_Rata_Nilai_TKA_SMA_2025 ~ Rasio_LabIPA_Baik + 
               Persentase_guru_SMA_bersertifikat_2024 + Rasio_Guru_mengajar_diatas_5tahun + 
               Rasio_SMA_fasilitas_INTERNET_untuk_belajar + Rasio_SMA_fasilitas_komputer_untuk_belajar + 
               Persentase_SMA_Akreditasi_A + Rasio_Guru_lebih_dari_sama_S1 + 
               Rasio_Angka_Partisipasi_Murni_SMA + Rasio_Perpustakaan_Baik, 
             data = data)
summary(model3)
vif(model3)

e3 <- resid(model3)

dwtest(model3)
shapiro.test(e3)
bptest(model3)

# ============================================================
# 2. RIDGE (global lambda)
# ============================================================
ridge_model <- lm.ridge(
  Rata_Rata_Nilai_TKA_SMA_2025 ~ Rasio_LabIPA_Baik + 
    Persentase_guru_SMA_bersertifikat_2024 + Rasio_Guru_mengajar_diatas_5tahun + 
    Rasio_SMA_fasilitas_INTERNET_untuk_belajar + Rasio_SMA_fasilitas_komputer_untuk_belajar + 
    Persentase_SMA_Akreditasi_A + Rasio_Guru_lebih_dari_sama_S1 + 
    Rasio_Angka_Partisipasi_Murni_SMA + Rasio_Perpustakaan_Baik,
  data = as.data.frame(data),
  lambda = seq(0, 1, 0.01)
)

best_lambda_ridge <- as.numeric(names(which.min(ridge_model$GCV)))
cat("Best lambda (Ridge):", best_lambda_ridge, "\n")

best_coef_ridge <- coef(ridge_model)[which.min(ridge_model$GCV), ]
X_mat_ridge     <- as.matrix(cbind(1, as.data.frame(data)[, var]))
fitted_ridge    <- X_mat_ridge %*% best_coef_ridge
resid_ridge     <- y - fitted_ridge

best_coef_ridge

ss_res_ridge <- sum(resid_ridge^2)
ss_tot_ridge <- sum((y - mean(y))^2)
ridge_r2     <- 1 - ss_res_ridge / ss_tot_ridge
ridge_rmse   <- sqrt(mean(resid_ridge^2))

k_ridge   <- ncol(X_mat_ridge)
ridge_aic <- n * log(ss_res_ridge / n) + 2 * k_ridge

# ============================================================
# 3. PARTIAL GENERALIZED RIDGE REGRESSION (PGRR)
# ============================================================
generalized_ridge <- function(X, y, lambda_vec) {
  X_int <- cbind(1, X)
  XtX <- t(X_int) %*% X_int
  Xty <- t(X_int) %*% y
  Lambda <- diag(c(0, lambda_vec))
  beta <- solve(XtX + Lambda) %*% Xty
  return(beta)
}

# Grid search for best lambda_X1, lambda_X9 (by R²)
lambda_grid <- seq(0.1, 1, by = 0.1)
results_2d  <- expand.grid(lambda_X1 = lambda_grid, lambda_X9 = lambda_grid)
results_2d$r2   <- NA
results_2d$rmse <- NA

X_int <- cbind(1, X)

for (i in 1:nrow(results_2d)) {
  l1 <- results_2d$lambda_X1[i]
  l9 <- results_2d$lambda_X9[i]
  
  lambda_test <- c(l1, 0, 0, 0, 0, 0, 0, 0, l9)
  m     <- generalized_ridge(X, y, lambda_test)
  y_hat <- X_int %*% m
  e     <- y - y_hat
  
  results_2d$r2[i]   <- 1 - sum(e^2) / sum((y - mean(y))^2)
  results_2d$rmse[i] <- sqrt(mean(e^2))
}

best <- results_2d[which.max(results_2d$r2), ]
cat("Best combo (by R²):\n")
print(best)

# Final PGRR with chosen lambda
lambda_vec <- c(0.1, 0, 0, 0, 0, 0, 0, 0, 0.3)
gen_ridge  <- generalized_ridge(X, y, lambda_vec)

y_hat_gen_ridge <- X_int %*% gen_ridge
e_gen_ridge     <- y - y_hat_gen_ridge

ss_res <- sum(e_gen_ridge^2)
ss_tot <- sum((y - mean(y))^2)
r2_gen_ridge   <- 1 - ss_res / ss_tot
gen_ridge_rmse <- sqrt(mean(e_gen_ridge^2))

k_gen_ridge   <- ncol(X_int)
gen_ridge_aic <- n * log(ss_res / n) + 2 * k_gen_ridge

shapiro.test(e_gen_ridge)

# ============================================================
# 4. STANDARDIZE X (for Lasso & Elastic Net)
# ============================================================
X_scaled <- scale(X)

# ============================================================
# 5. LASSO
# ============================================================
set.seed(42)
cv_lasso <- cv.glmnet(X_scaled, y, alpha = 1, standardize = FALSE)
lasso_model <- glmnet(X_scaled, y, alpha = 1, lambda = cv_lasso$lambda.min)
cat("Best lambda (Lasso):", cv_lasso$lambda.min, "\n")

y_hat_lasso <- predict(lasso_model, X_scaled)
e_lasso     <- y - y_hat_lasso

lasso_rmse <- sqrt(mean(e_lasso^2))
lasso_r2   <- 1 - sum(e_lasso^2) / sum((y - mean(y))^2)

k_lasso   <- sum(coef(lasso_model)[-1] != 0)
lasso_rss <- sum(e_lasso^2)
lasso_aic <- n * log(lasso_rss / n) + 2 * k_lasso

shapiro.test(e_lasso)

# ============================================================
# 6. ELASTIC NET
# ============================================================
set.seed(42)
cv_enet <- cv.glmnet(X_scaled, y, alpha = 0.5, standardize = FALSE)
enet_model <- glmnet(X_scaled, y, alpha = 0.5, lambda = cv_enet$lambda.min)
cat("Best lambda (Elastic Net):", cv_enet$lambda.min, "\n")

y_hat_enet <- predict(enet_model, X_scaled)
e_enet     <- y - y_hat_enet

enet_rmse <- sqrt(mean(e_enet^2))
enet_r2   <- 1 - sum(e_enet^2) / sum((y - mean(y))^2)

k_enet   <- sum(coef(enet_model)[-1] != 0)
enet_rss <- sum(e_enet^2)
enet_aic <- n * log(enet_rss / n) + 2 * k_enet

shapiro.test(e_enet)

# ============================================================
# 7. COMPARE MODELS
# ============================================================
comparison1 <- data.frame(
  Model = c("Ridge", "Partial Generalized Ridge", "Lasso", "Elastic Net"),
  R2    = c(ridge_r2, r2_gen_ridge, lasso_r2, enet_r2),
  RMSE  = c(ridge_rmse, gen_ridge_rmse, lasso_rmse, enet_rmse),
  AIC   = c(ridge_aic, gen_ridge_aic, lasso_aic, enet_aic)
)
print(comparison1)

# ============================================================
# 8. MULTICOLLINEARITY DIAGNOSTICS
# ============================================================

# --- Ridge (global lambda): Ridge variance ratio ---
ridge_vif <- function(X, residuals, lambda_vec) {
  n <- nrow(X); p <- ncol(X)
  sigma2 <- sum(residuals^2) / (n - p)
  XtX <- t(X) %*% X
  Lambda <- diag(lambda_vec)
  A <- solve(XtX + Lambda)
  var_beta_ridge <- sigma2 * A %*% XtX %*% A
  var_beta_ols   <- sigma2 * solve(XtX)
  return(diag(var_beta_ridge) / diag(var_beta_ols))
}

ridge_global_vif_values <- ridge_vif(X, resid_ridge, rep(best_lambda_ridge, ncol(X)))
print(round(ridge_global_vif_values, 4))

# --- PGRR: Ridge variance ratio ---
ridge_vif_values <- ridge_vif(X, e_gen_ridge, lambda_vec)
print(round(ridge_vif_values, 4))

# --- Lasso: VIF on retained variables ---
lasso_coef     <- as.matrix(coef(lasso_model))
selected_lasso <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
selected_lasso <- selected_lasso[selected_lasso != "(Intercept)"]
print(selected_lasso)

lasso_data <- data[, selected_lasso, drop = FALSE]
lasso_temp <- lm(y ~ ., data = lasso_data)
vif_lasso  <- vif(lasso_temp)
print(round(vif_lasso, 4))

# --- Elastic Net: VIF on retained variables ---
enet_coef     <- as.matrix(coef(enet_model))
selected_enet <- rownames(enet_coef)[enet_coef[, 1] != 0]
selected_enet <- selected_enet[selected_enet != "(Intercept)"]
print(selected_enet)

enet_data <- data[, selected_enet, drop = FALSE]
enet_temp <- lm(y ~ ., data = enet_data)
vif_enet  <- vif(enet_temp)
print(round(vif_enet, 4))

multicollinearity_comparison <- data.frame(
  Variable = colnames(X),
  Ridge_Global    = round(ridge_global_vif_values, 4),
  PGRR            = round(ridge_vif_values, 4),
  Lasso_VIF       = c(round(vif_lasso, 4), rep(NA, length(colnames(X)) - length(vif_lasso)))[match(colnames(X), c(names(vif_lasso), rep(NA, length(colnames(X)) - length(vif_lasso))))],
  Elastic_Net_VIF = c(round(vif_enet, 4), rep(NA, length(colnames(X)) - length(vif_enet)))[match(colnames(X), c(names(vif_enet), rep(NA, length(colnames(X)) - length(vif_enet))))]
)

print(multicollinearity_comparison)


# ============================================================
# 9. FINAL COEFFICIENTS
# ============================================================
coef(model3)
print(best_coef_ridge)
print(gen_ridge)
coef(lasso_model)
coef(enet_model)

cor(X)
