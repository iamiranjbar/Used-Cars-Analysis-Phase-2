usedCar <- read.csv("~/Desktop/Term 8/SI/CA/Phase 2/usedcar.csv")
print(sapply(usedCar, function(x) sum(is.na(x))))
usedCars <- usedCar[!is.na(usedCar$price),]
usedCars$economy <- ifelse(is.na(usedCars$economy), mean(usedCars$economy, na.rm=TRUE), usedCars$economy)
usedCars$odometer <- ifelse(is.na(usedCars$odometer), mean(usedCars$odometer, na.rm=TRUE), usedCars$odometer)
usedCars$litres <- ifelse(is.na(usedCars$litres), median(usedCars$litres, na.rm=TRUE), usedCars$litres)
usedCars$cylinders <- ifelse(is.na(usedCars$cylinders), median(usedCars$cylinders, na.rm=TRUE), usedCars$cylinders)
print(sapply(usedCars, function(x) sum(is.na(x))))
drops <- c("rownum")
usedCars <- usedCars[ , !(names(usedCars) %in% drops)]
# 1 #
usedCars$body_type <- as.factor(usedCars$body_type)
print(levels(usedCars$body_type))
print(levels(usedCars$make))
print(nrow(usedCars[usedCars$body_type == "",]))
usedCars <- usedCars[usedCars$body_type != "",]
print(nrow(usedCars[usedCars$body_type == "",]))
## a ##
subaru_count <- nrow(usedCars[usedCars$make == "Subaru",])
subaru_pe <-  subaru_count / nrow(usedCars)
sedan_count <- nrow(usedCars[usedCars$body_type == "Sedan",])
sedan_pe <- sedan_count / nrow(usedCars)
point_estimate <- subaru_pe - sedan_pe
standard_error <- sqrt((subaru_pe * (1-subaru_pe)/subaru_count)+(sedan_pe * (1-sedan_pe)/sedan_count))
z_limit <- 0.05 / 2
z_star <- -qnorm(z_limit)
margin_of_error <- standard_error * z_star
lower_bound <- point_estimate - margin_of_error
upper_bound <- point_estimate + margin_of_error
print(paste("95% CI: (", lower_bound, ",",upper_bound,")"))
## b ##
usedCars$body_type <- droplevels(usedCars$body_type)
observation_table <- table(usedCars$make, usedCars$body_type)
expected_table <- as.array(margin.table(observation_table,1)) %*% t(as.array(margin.table(observation_table,2))) / margin.table(observation_table)
print(observation_table)
print(expected_table)
chi_stat <- sum((expected_table - as.array(observation_table))^2/expected_table)
df <- (length(levels(usedCars$make))-1) * (length(levels(usedCars$body_type))-1)
p_value <- pchisq(chi_stat, df = df, lower.tail = FALSE)
print(paste("P-value: ", p_value))
# 2 #
print(nrow(usedCars[usedCars$transmission=="",]))
usedCars <- usedCars[usedCars$transmission != "",]
print(nrow(usedCars[usedCars$transmission=="",]))
first_sample <- sample(usedCars$transmission, size = 10)
manual_count <- length(first_sample[first_sample == "Manual"])
p_hat <- manual_count / 10
proportions <- vector()
for (n in 1:1000) {
  success_count <- 0
  for (i in 1:10) {
    random_number <- runif(1)
    if (random_number >= 0.5) {
      success_count <- success_count + 1
    }
  }
  new_prop <- success_count / 10
  proportions <- c(proportions, new_prop)
}
extreme_result <- 0
for (n in 1:1000) {
  if (proportions[n] >= p_hat) {
    extreme_result <- extreme_result + 1
  }
}
p_value <- extreme_result / 1000
print(paste("P-value:", p_value))
# 3 #
## a ##
probs <- vector()
for (body_type in levels(usedCars$body_type)) {
  body_type_count <- nrow(usedCars[usedCars$body_type == body_type,])
  body_type_prob <- body_type_count / nrow(usedCars)
  print(paste(body_type, ":", body_type_prob))
  probs <- c(probs, body_type_prob)
}
random_sample <- usedCars[sample(nrow(usedCars), size = 100),]
sedan_suv_cars <- usedCars[usedCars$body_type == "SUV" | usedCars$body_type == "Sedan",]
biased_sample <- sedan_suv_cars[sample(nrow(sedan_suv_cars), size = 100),]
random_count <- vector()
biased_count <- vector()
for (body_type in levels(usedCars$body_type)) {
  body_type_count <- nrow(random_sample[random_sample$body_type == body_type,])
  random_count <- c(random_count, body_type_count)
  body_type_count <- nrow(biased_sample[biased_sample$body_type == body_type,])
  biased_count <- c(biased_count, body_type_count)
}
random_expected <- probs * 100
biased_expected <- probs * 100
random_chi <- sum((random_count - random_expected)^2 / random_expected)
biased_chi <- sum((biased_count - biased_expected)^2 / biased_expected)
df <- length(random_count)
random_p_value <- pchisq(random_chi, df = df, lower.tail = FALSE)
biased_p_value <- pchisq(biased_chi, df = df, lower.tail = FALSE)
print(paste("Random P-value: ", random_p_value))
print(paste("Biased P-value:", biased_p_value))
## b ##
observation_table <- table(usedCars$make, usedCars$body_type)
expected_table <- as.array(margin.table(observation_table,1)) %*% t(as.array(margin.table(observation_table,2))) / margin.table(observation_table)
chi_stat <- sum((expected_table - as.array(observation_table))^2/expected_table)
df <- (length(levels(usedCars$make))-1) * (length(levels(usedCars$body_type))-1)
p_value <- pchisq(chi_stat, df = df, lower.tail = FALSE)
print(paste("P-value: ", p_value))
# 4 #
## a ##
price_lr <- lm(price ~ odometer, data = usedCars)
print(summary(price_lr))
## c ##
plot(usedCars$odometer, usedCars$price, main="Price vs. Odometer", xlab = "Odometer", ylab = "Price", pch = 20)
abline(price_lr, lty = 2)
## d ##
random_sample <- usedCars[sample(nrow(usedCars), size = 27),]
### 1 ###
price_little_lr <- lm(price ~ odometer, data = random_sample)
print(summary(price_little_lr))
### 2 ###
lr_summary <- summary(price_little_lr)
lr_coeficients <- lr_summary$coefficients
point_estimate <- lr_coeficients["odometer", "Estimate"]
df <- 25
t_star <- -qt(0.025, df=df)
standard_error <- lr_coeficients["odometer", "Std. Error"]
margin_of_error <- t_star * standard_error
lower_bound <- point_estimate - margin_of_error
upper_bound <- point_estimate + margin_of_error
print(paste("95% CI: (", lower_bound, ",", upper_bound, ")"))
### 3 ###
price_bigger_lr <- lm(price ~ odometer + transmission, data=random_sample)
print(summary(price_bigger_lr))
print(anova(price_little_lr))
print(anova(price_bigger_lr))
# 5 #
## a ##
first_step_model <- lm(price ~ body_type + category + colour + cylinders + economy + fuel + litres + location + make + model + odometer+ transmission + year, data = usedCars)
print(summary(first_step_model))
second_step_model <- lm(price ~ body_type + category + colour + cylinders + economy + fuel + litres + location + model + odometer + transmission + year, data = usedCars)
print(summary(second_step_model))
third_step_model <- lm(price ~ body_type + category + colour + cylinders + economy + fuel + litres + location + odometer + transmission + year, data = usedCars)
print(summary(third_step_model))
fourth_step_model <- lm(price ~ category + colour + cylinders + economy + fuel + litres + location + odometer + transmission + year, data = usedCars)
print(summary(fourth_step_model))
fifth_step_model <- lm(price ~ category + colour + cylinders + economy + fuel + location + odometer + transmission + year, data = usedCars)
print(summary(fifth_step_model))
sixth_step_model <- lm(price ~  category + colour + cylinders + economy + location + odometer + transmission + year, data = usedCars)
print(summary(sixth_step_model))
seventh_step_model <- lm(price ~  category + colour + economy + location + odometer + transmission + year, data = usedCars)
print(summary(seventh_step_model))
eighth_step_model <- lm(price ~  category + colour + economy + odometer + transmission + year, data = usedCars)
print(summary(eighth_step_model))
ninth_step_model <- lm(price ~ category + colour + economy + odometer + year, data = usedCars)
print(summary(ninth_step_model))
## b ##
library(caret)
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
final_model <- train(price ~ category + colour + economy + odometer + year, data = usedCars, method = "lm",
               trControl = train.control)
# Summarize the results
print(final_model)
## c ##
### 1 ###
final_model <- ninth_step_model
plot(final_model$residuals ~ usedCars$economy, main= "Residuals VS. economy", pch=20)
plot(final_model$residuals ~ usedCars$odometer, main= "Residuals VS. odometer", pch = 20)
plot(final_model$residuals ~ usedCars$year, main= "Residuals VS. year", pch = 20)
### 2 ###
hist(final_model$residuals)
qqnorm(final_model$residuals)
qqline(final_model$residuals)
### 3 ###
plot(final_model$residuals ~ final_model$fitted)
## d ##
library(corrplot)
model_mat <- usedCars[ , c(1, 8, 15, 17)]
cor_mat <- cor(model_mat)
corrplot(cor_mat, method="circle")
## e ##
print(summary(final_model))
# 6 #
## a ##
usedCars$make <- as.factor(usedCars$make)
usedCars$cylinders <- as.factor(usedCars$cylinders)
make_glm <- glm(make ~ price + cylinders, family = binomial, data = usedCars)
print(summary(make_glm))
## c ##
prediction <- predict.glm(make_glm, usedCars)
predicted <- 1/ (1 + exp(-prediction))
library(pROC)
roc_obj <- roc(usedCars$make, predicted)
plot(roc_obj)
print(auc(roc_obj))
## d ##
odd_ratio_98_ci <- function(point_estimate, standard_error) {
  z_star <- -qnorm(0.01)
  margin_of_error <- z_star * standard_error
  lower_bound <- exp(point_estimate - margin_of_error)
  upper_bound <- exp(point_estimate + margin_of_error)
  print(paste("98% CI: (", lower_bound, ",", upper_bound, ")"))
}
coefs <- summary(make_glm)$coefficients
print("Intercept:")
odd_ratio_98_ci(coefs["(Intercept)", "Estimate"], coefs["(Intercept)", "Std. Error"])
print("Price slope:")
odd_ratio_98_ci(coefs["price", "Estimate"], coefs["price", "Std. Error"])
print("Cylinder slope:")
odd_ratio_98_ci(coefs["cylinders6", "Estimate"], coefs["cylinders6", "Std. Error"])
# 7 #
## b ##
odds_ratio <- exp(coefs["cylinders6", "Estimate"])
p_toyota_4cylinder <- seq(0.01, 1, 0.01)
p_toyota_6cylinder <- vector()
for (p in p_toyota_4cylinder) {
  x <- (p / (1-p)) * odds_ratio
  new_prob <- x / (x+1)
  p_toyota_6cylinder <- c(p_toyota_6cylinder, new_prob)
}
plot(p_toyota_4cylinder, p_toyota_6cylinder, pch=16)
lines(p_toyota_4cylinder[order(p_toyota_4cylinder)], p_toyota_6cylinder[order(p_toyota_4cylinder)], xlim=range(p_toyota_4cylinder), ylim=range(p_toyota_6cylinder), pch=16)
## c ##
make_glm <- glm(make ~ price + body_type + model, family = binomial, data = usedCars)
print(summary(make_glm))
make_glm <- glm(make ~ body_type + model, family = binomial, data = usedCars)
print(summary(make_glm))
make_glm <- glm(make ~ price + model, family = binomial, data = usedCars)
print(summary(make_glm))
make_glm <- glm(make ~ price + body_type, family = binomial, data = usedCars)
print(summary(make_glm))
make_glm <- glm(make ~ price, family = binomial, data = usedCars)
print(summary(make_glm))
make_glm <- glm(make ~ model, family = binomial, data = usedCars)
print(summary(make_glm))
## d ##
treshold <- seq(0.01, 0.99, 0.01)
actual <- usedCars$make
prediction <- predict.glm(make_glm, usedCars)
predicted <- 1/ (1 + exp(-prediction))
max_tresh <- 0
max_score <- 0
for (tresh in treshold) {
  class_prediction <-
    ifelse(predicted > tresh,
           "Toyota",
           "Subaru"
    )
  class_prediction <- as.factor(class_prediction)
  conf_mat <- confusionMatrix(class_prediction, actual)
  score <- conf_mat$byClass[["Sensitivity"]] + 1 - conf_mat$byClass[["Specificity"]]
  if (score > max_score) {
    max_score <- score
    max_tresh <- tresh
  }
}
print(paste("Best treshold:", max_tresh))
## e ##
treshold <- seq(0.01, 0.99, 0.01)
actual <- usedCars$make
prediction <- predict.glm(make_glm, usedCars)
predicted <- 1/ (1 + exp(-prediction))
max_tresh <- 0
max_score <- 0
utilities <- vector()
for (tresh in treshold) {
  class_prediction <-
    ifelse(predicted > tresh,
           "Toyota",
           "Subaru"
    )
  class_prediction <- as.factor(class_prediction)
  conf_mat <- confusionMatrix(class_prediction, actual)
  tp <- conf_mat$table["Toyota", "Toyota"]
  tn <- conf_mat$table["Subaru", "Subaru"]
  fp <- conf_mat$table["Toyota", "Subaru"]
  fn <- conf_mat$table["Subaru", "Toyota"]
  utility <- tp + tn - 5 * fp - 10 * fn
  utilities <- c(utilities, utility)
  if (utility > max_score) {
    max_score <- utility
    max_tresh <- tresh
  }
}
print(paste("Best treshold:", max_tresh))
plot(treshold, utilities, pch=16)
lines(treshold[order(treshold)], utilities[order(treshold)], xlim=range(treshold), ylim=range(utilities), pch=16)
# 8 #
usedCars <- usedCars[usedCars$fuel != "",]
usedCars$fuel <- as.factor(usedCars$fuel)
droplevels(usedCars$fuel)
usedCars$transmission <- as.factor(usedCars$transmission)
first_step_lm <- lm(economy ~ cylinders + fuel + litres + odometer + transmission + year, data = usedCars)
print(summary(first_step_lm))
second_step_lm <- lm(economy ~ cylinders + fuel + litres + transmission + year, data = usedCars)
print(summary(second_step_lm))