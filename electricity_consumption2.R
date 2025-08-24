# ===============================
# پروژه: تحلیل مصرف برق (Regression Analysis)
# دیتاست: Electricity_Consumption10
# ===============================

# نصب و بارگذاری پکیج
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# --- بخش 1: بررسی اولیه دیتاست ---
cat("\n--- بررسی اولیه دیتاست ---\n")
str(Electricity_Consumption10)
head(Electricity_Consumption10)
summary(Electricity_Consumption10)

# --- بخش 2: تبدیل متغیرهای کیفی به فاکتور ---
Electricity_Consumption10 <- data.frame(lapply(Electricity_Consumption10, function(x) {
  if(is.character(x)) as.factor(x) else x
}))
str(Electricity_Consumption10)

# --- بخش 3: ساخت ماتریس طراحی و بردار هدف ---
X <- model.matrix(Target ~ ., data = Electricity_Consumption10)
Y <- as.numeric(Electricity_Consumption10$Target)

# ✅ تعریف تعداد نمونه‌ها و متغیرها
n <- nrow(X)   # تعداد مشاهدات
k <- ncol(X)   # تعداد متغیرها (با Dummy)

cat("ابعاد ماتریس X:", dim(X), "\n")  # چک کردن

# --- بخش 4: برآورد مدل رگرسیون (OLS) ---
beta_hat <- solve(crossprod(X), crossprod(X, Y))
residuals <- Y - X %*% beta_hat
sigma2_hat <- sum(residuals^2) / (n - k)
var_beta_hat <- sigma2_hat * solve(t(X) %*% X)
se_beta_hat <- sqrt(diag(var_beta_hat))
t_stats <- beta_hat / se_beta_hat
p_values <- 2 * (1 - pt(abs(t_stats), df = n - k))

# --- بخش 5: جدول نتایج ---
results <- data.frame(
  Coefficient = as.vector(beta_hat),
  Std_Error   = se_beta_hat,
  t_value     = as.vector(t_stats),
  p_value     = p_values
)
cat("\n--- نتایج رگرسیون ---\n")
print(round(results, 4))

# --- بخش 6: نمودار ضرایب ---
coef_data <- data.frame(
  Variable    = rownames(results),
  Coefficient = results$Coefficient,
  Std_Error   = results$Std_Error
)
coef_data$CI_Lower <- coef_data$Coefficient - 1.96 * coef_data$Std_Error
coef_data$CI_Upper <- coef_data$Coefficient + 1.96 * coef_data$Std_Error

ggplot(coef_data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Coefficient Plot", x = "Variables", y = "Coefficient (Effect Size)")

# --- بخش 7: ارزیابی مدل ---
SST <- sum((Y - mean(Y))^2)
SSE <- sum(residuals^2)
R2 <- 1 - (SSE/SST)
MSE <- mean(residuals^2)

cat("\n--- ارزیابی مدل ---\n")
cat("R-squared:", round(R2, 4), "\n")
cat("MSE:", round(MSE, 4), "\n")
