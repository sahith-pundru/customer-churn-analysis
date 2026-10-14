
# Open the CSV file and save it as "data".
data <- read.csv("C:/Users/SAHITHREDDY/Desktop/customer_Attrition_project/Telco-Customer-Churn.csv")
head(data)
dim(data)
names(data)
str(data)

# Counting missing values 
colSums(is.na(data))

# Replace the 11 missing values with 0.
# (These customers are brand new, so they have paid nothing.)
data$TotalCharges[is.na(data$TotalCharges)] <- 0

# Checking again. 
colSums(is.na(data))

# Counting duplicate rows. 
sum(duplicated(data))
data$Churn <- factor(data$Churn, levels = c("No", "Yes"))
table(data$Churn)
prop.table(table(data$Churn)) * 100

#  CHI-SQUARE TESTS
# Checks if each attribute is linked to Churn.
# p-value below 0.05 = useful attribute.
# p-value above 0.05 = not useful.

cols <- c("gender", "SeniorCitizen", "Partner", "Dependents",
          "PhoneService", "MultipleLines", "InternetService",
          "OnlineSecurity", "OnlineBackup", "DeviceProtection",
          "TechSupport", "StreamingTV", "StreamingMovies",
          "Contract", "PaperlessBilling", "PaymentMethod")

for (col in cols) {
  p <- chisq.test(table(data[[col]], data$Churn))$p.value
  print(paste(col, "- p-value:", signif(p, 3)))
}

t.test(tenure ~ Churn, data = data)
t.test(MonthlyCharges ~ Churn, data = data)

# ---------- Correlation of each number column with Churn ----------
# 1 = left and 0 = stayed. (The original Churn column is not changed.)
# ifelse() if Churn is "Yes" then 1, otherwise 0.
data$Churn01 <- ifelse(data$Churn == "Yes", 1, 0)

#   p-value: below 0.05 means the link with churn is real.
#   cor:     strength from -1 to 1.
#            Positive = higher values go with leaving.
#            Negative = higher values go with staying.
#            Near 0 = no link.

cor.test(data$tenure, data$Churn01)
cor.test(data$MonthlyCharges, data$Churn01)
cor.test(data$TotalCharges, data$Churn01)

# RESULT: most attributes are useful (p below 0.05).
# gender, PhoneService, MultipleLines are NOT useful (p above 0.05),
# so we do not plot them.


# Two colours: blue = stayed, red = left.
colours <- c("steelblue", "tomato")


# ---------- Graph 1: Overall churn ----------

# barplot() draws bars from the counts.
# main = title, ylab = vertical label, col = colours.
barplot(table(data$Churn),
        main = "How Many Customers Left?",
        ylab = "Number of customers",
        col = colours)


# ---------- Graph 2: Contract type vs churn ----------

# Percentages per row: each contract type adds up to 100%.

contract_pct <- prop.table(table(data$Contract, data$Churn), 1) * 100
contract_pct

# beside = TRUE puts bars side by side.
# legend = TRUE adds the colour key.
barplot(t(contract_pct),
        beside = TRUE,
        col = colours,
        main = "Churn Rate by Contract Type",
        ylab = "% of customers",
        legend = TRUE)


# ---------- Graph 3: Internet service vs churn ----------

# Same steps again with a different column.
internet_pct <- prop.table(table(data$InternetService, data$Churn), 1) * 100

internet_pct

barplot(t(internet_pct),
        beside = TRUE,
        col = colours,
        main = "Churn Rate by Internet Service",
        ylab = "% of customers",
        legend = TRUE)


# ---------- Graph 4: Payment method vs churn ----------

payment_pct <- prop.table(table(data$PaymentMethod, data$Churn), 1) * 100

payment_pct

# par(mar = ...) makes the bottom margin bigger so long names fit.
par(mar = c(10, 4, 4, 2))

barplot(t(payment_pct),
        beside = TRUE,
        col = colours,
        main = "Churn Rate by Payment Method",
        ylab = "% of customers",
        legend = TRUE,
        las = 2,
        cex.names = 0.7)

par(mar = c(5, 4, 4, 2) + 0.1)


# ---------- Graph 5: Tech support vs churn ----------

support_pct <- prop.table(table(data$TechSupport, data$Churn), 1) * 100

support_pct

barplot(t(support_pct),
        beside = TRUE,
        col = colours,
        main = "Churn Rate by Tech Support",
        ylab = "% of customers",
        legend = TRUE)


# ---------- Graph 6: Online security vs churn ----------

security_pct <- prop.table(table(data$OnlineSecurity, data$Churn), 1) * 100

security_pct

barplot(t(security_pct),
        beside = TRUE,
        col = colours,
        main = "Churn Rate by Online Security",
        ylab = "% of customers",
        legend = TRUE)


# ---------- Graph 7: Tenure vs churn ----------

# boxplot() shows the spread of a number for each group.
# "tenure ~ Churn" reads as "tenure split by Churn".
# The thick line in each box is the median (middle value).
boxplot(tenure ~ Churn,
        data = data,
        col = colours,
        main = "Tenure vs Churn",
        xlab = "Churn (No = stayed, Yes = left)",
        ylab = "Tenure (months)")


# ---------- Graph 8: Monthly charges vs churn ----------

boxplot(MonthlyCharges ~ Churn,
        data = data,
        col = colours,
        main = "Monthly Charges vs Churn",
        xlab = "Churn (No = stayed, Yes = left)",
        ylab = "Monthly charges")


# ---------- Graph 9: Tenure vs monthly charges ----------

# plot(x, y) draws one dot per customer.
# pch = 16 makes solid dots.
plot(data$tenure, data$MonthlyCharges,
     main = "Tenure vs Monthly Charges",
     xlab = "Tenure (months)",
     ylab = "Monthly charges",
     pch = 16,
     col = "steelblue")


# Close to 0 = no relationship. Close to 1 = strong link.
cor(data$tenure, data$MonthlyCharges)
# LOGISTIC REGRESSION
model_data <- data
model_data$TechSupport[model_data$TechSupport == "No internet service"] <- "No"
model_data$OnlineSecurity[model_data$OnlineSecurity == "No internet service"] <- "No"
for (v in c("Contract", "InternetService", "PaymentMethod", "TechSupport", "OnlineSecurity")) {
  model_data[[v]] <- as.factor(model_data[[v]])
}

# Split: 80% to train the model, 20% kept hidden to test it.
set.seed(123)                                   # same random split every time
rows  <- sample(nrow(model_data), 0.8 * nrow(model_data))
train <- model_data[rows, ]
test  <- model_data[-rows, ]
dim(train)
dim(test)

model <- glm(Churn01 ~ tenure + MonthlyCharges + Contract +
               InternetService + TechSupport + OnlineSecurity + PaymentMethod,
             data = train, family = "binomial")
summary(model)

# Odds ratios with 95% range: above 1 raises churn, below 1 lowers it.
round(exp(cbind(OddsRatio = coef(model), confint.default(model))), 3)
odds <- exp(coef(model))[-1]
par(mar = c(5, 16, 4, 2))
barplot(sort(odds), horiz = TRUE, las = 1, cex.names = 0.7, col = "steelblue",
        main = "Odds Ratios (above 1 = more likely to leave)",
        xlab = "Odds ratio")
abline(v = 1, col = "red", lwd = 2)
par(mar = c(5, 4, 4, 2) + 0.1)

# Predicted chance of leaving (0 to 1) for each test customer.
prob <- predict(model, newdata = test, type = "response")

# Confusion matrix at the usual cutoff of 0.5:
predicted <- factor(ifelse(prob > 0.5, 1, 0), levels = c(0, 1))
table(Actual = factor(test$Churn01, levels = c(0, 1)), Predicted = predicted)

# A function that calculates the main measures for any cutoff.
evaluate <- function(prob, actual, cutoff) {
  pred <- factor(ifelse(prob > cutoff, 1, 0), levels = c(0, 1))
  cm   <- table(Actual = factor(actual, levels = c(0, 1)), Predicted = pred)
  TN <- cm["0", "0"]; FP <- cm["0", "1"]
  FN <- cm["1", "0"]; TP <- cm["1", "1"]
  c(Cutoff      = cutoff,
    Accuracy    = (TP + TN) / sum(cm),   # all correct / all customers
    Sensitivity = TP / (TP + FN),        # real leavers we caught
    Specificity = TN / (TN + FP),        # real stayers we got right
    Precision   = TP / (TP + FP))        # when we say "leave", how often right
}

# Compare three cutoffs. A lower cutoff catches more leavers
# (higher sensitivity) but gives more false alarms.
round(t(sapply(c(0.5, 0.4, 0.3), function(k) evaluate(prob, test$Churn01, k))), 3)

# Baseline: accuracy if we just guessed "everyone stays". The model should beat this.
round(mean(test$Churn01 == 0), 3)

# Predicted chance by what really happened (leavers should score higher).
boxplot(prob ~ test$Churn, col = colours,
        names = c("Stayed", "Left"),
        main = "Predicted Chance of Leaving",
        xlab = "What really happened", ylab = "Predicted probability")

# ROC curve: try every cutoff from 0 to 1.
cutoffs <- seq(0, 1, by = 0.01)
tpr <- sapply(cutoffs, function(k) sum(prob > k & test$Churn01 == 1) / sum(test$Churn01 == 1))
fpr <- sapply(cutoffs, function(k) sum(prob > k & test$Churn01 == 0) / sum(test$Churn01 == 0))

plot(fpr, tpr, type = "l", lwd = 2, col = "tomato",
     main = "ROC Curve",
     xlab = "False positive rate", ylab = "True positive rate")
abline(0, 1, lty = 2)        # dashed line = pure guessing

# AUC = area under the ROC curve. 0.5 = guessing, 1 = perfect, above 0.8 = good.
auc <- sum(diff(rev(fpr)) * (head(rev(tpr), -1) + tail(rev(tpr), -1)) / 2)
round(auc, 3)

# SAVE THE FINAL MODEL FOR FUTURE CUSTOMER DATA
saveRDS(model, "churn_model.rds")
file.exists("churn_model.rds")
