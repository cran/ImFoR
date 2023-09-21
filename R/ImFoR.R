#'@title Non-Linear Height Diameter Models for Forestry
#' @param data Datasets
#' @param train_frac Train-Test fraction
#' @import stats minpack.lm Metrics caret tidyverse nlme ggpubr ggplot2
#' @return
#' \itemize{
#'   \item metrics: Metrics of all applied models
#'   \item plot:  Plot
#' }
#' @export
#'
#' @examples
#' library("ImFoR")
#' data <- system.file("extdata", "data_test.csv", package = "ImFoR")
#' data_test <- read.csv(data)
#' Model<-ImFoR(data =data_test)
#' @references
#' \itemize{
#'\item Jeelani, M.I., Tabassum, A., Rather, K and Gul,M.2023. Neural Network Modeling of Height Diameter Relationships for Himalayan Pine through Back Propagation Approach. Journal of The Indian Society of Agricultural Statistics. 76(3): 169–178
#'\item Tabassum, A., Jeelani, M.I., Sharma,M., Rather, K R ., Rashid, I and Gul,M.2022.  Predictive Modelling of Height and Diameter Relationships of Himalayan Chir Pine . Agricultural Science Digest - A Research Journal. DOI:10.18805/ag.D-5555
#'\item	Huang, S., Titus, S.J., and Wiens, D.P. 1992. Comparison of nonlinear height-diameter functiond for major Alberta tree species. Can J. For. Res. 22: 1297-1304. DOI : 10.1139/x92-172
#'\item -	Zeide, B. 1993. Analysis of growth equations. Forest Science 39(3):594-616. doi:10.1093/forestscience/39.3.594
#' }
ImFoR <- function(data, train_frac = 0.8) {
  # Split the data into training and test sets
  Model<-NULL
  Data<-NULL
  PER<-NULL
  training.samples <- data$Height %>%
    createDataPartition(p = train_frac, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  # Here 80% of data is uesd as training and 20% for testing
  # Define a list of model formulas and their starting values
  model_list <- list(
    Naslund1 = list(formula = Height ~ 1.37 + (Diameter^2 / (a + b * Diameter)^2), start = list(a = 1, b = 0.21)),
    Curtis = list(formula = Height ~ 1.37 + (a * (Diameter / (1 + Diameter))^b), start = list(a = 3.9, b = 0.02)),
    Michailoff = list(formula = Height ~ 1.37 + (a * exp(-b * Diameter^(-1))), start = list(a = 3, b = 1)),
    Meyer=list(formula=Height~1.37+(a*(1-exp(b*Diameter))),start=list(a=2,b=-0.21)),
    Power = list(formula = Height ~ 1.37 + (a * (Diameter)^b), start = list(a = 1, b = 1)),
    Michail_menten = list(formula = Height ~ 1.37 + ((a * Diameter) / (b + Diameter)), start = list(a = 1.82, b = -13.08)),
    Michail_menten2 = list(formula = Height ~ 1.37 + (Diameter / (a + (b * Diameter))), start = list(a = 1, b = 1)),
    Wykoff = list(formula = Height ~ 1.37 + (exp(a + (b / (Diameter + 1)))), start = list(a = 1, b = 1))
  )

  # Initialize data frames to store results
  training_results <- data.frame(Model = character(0), R2 = numeric(0), RMSE = numeric(0), MAE = numeric(0), AIC = numeric(0), BIC = numeric(0), PER = numeric(0), Data = character(0))
  test_results <- data.frame(Model = character(0), R2 = numeric(0), RMSE = numeric(0), MAE = numeric(0), AIC = numeric(0), BIC = numeric(0), PER = numeric(0), Data = character(0))

  for (model_name in names(model_list)) {
    model_info <- model_list[[model_name]]

    # Fit the model on the original data
    model_orig <- nls(formula = model_info$formula, start = model_info$start, data = data)




    # Fit the model on the training data
    model_train <- nls(formula = model_info$formula, start = model_info$start, data = train.data)

    # Predictions on training data
    predictions_train <- predict(model_train, train.data)

    # Calculate metrics for training data
    metrics_train <- data.frame(
      Model = model_name,
      R2 = R2(predictions_train, train.data$Height),
      RMSE = RMSE(predictions_train, train.data$Height),
      MAE = MAE(predictions_train, train.data$Height),
      AIC = AIC(model_train),
      BIC = BIC(model_train),
      PER = RMSE(predictions_train, train.data$Height) / mean(train.data$Height),
      Data = "Training"
    )

    training_results <- rbind(training_results, metrics_train)

    # Predictions on test data
    predictions_test <- predict(model_train, test.data)

    # Calculate metrics for test data
    metrics_test <- data.frame(
      Model = model_name,
      R2 = R2(predictions_test, test.data$Height),
      RMSE = RMSE(predictions_test, test.data$Height),
      MAE = MAE(predictions_test, test.data$Height),
      AIC = AIC(model_train),
      BIC = BIC(model_train),
      PER = RMSE(predictions_test, test.data$Height) / mean(test.data$Height),
      Data = "Test"
    )

    test_results <- rbind(test_results, metrics_test)
  }

  # Combine results for all data sets
  all_results <- rbind( training_results, test_results)

  # Create a ggplot to visualize the selection metrics
  plot1 <- ggplot(all_results, aes(x = Model, y = R2, fill = Data)) +
    geom_bar(stat = "identity", position = "dodge") + labs(title = "Coefficient of Determination", x = "Model",
         y = "R2") + facet_grid(. ~ Data) +  theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot2 <- ggplot(all_results, aes(x = Model, y = RMSE, fill = Data)) +
    geom_bar(stat = "identity", position = "dodge") + labs(title = "Root Mean Square Error",
         x = "Model", y = "RMSE") + facet_grid(. ~ Data) +theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot3 <- ggplot(all_results, aes(x = Model, y = MAE, fill = Data)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Mean Absolute Error",
         x = "Model",
         y = "MAE") +
    facet_grid(. ~ Data) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot4 <- ggplot(all_results, aes(x = Model, y = AIC, fill = Data)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Akaike information criterion",
         x = "Model",
         y = "AIC") +
    facet_grid(. ~ Data) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plot5 <- ggplot(all_results, aes(x = Model, y =BIC, fill = Data)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Bayesian information criterion",
         x = "Model",
         y = "BIC") +
    facet_grid(. ~ Data) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot6<- ggplot(all_results, aes(x = Model, y =PER, fill = Data)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Prediction Error Rate",
         x = "Model",
         y = "PER") +
    facet_grid(. ~ Data) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  final.plot= ggarrange(plot1,plot2,plot3,plot4,plot5,plot6)
 Result<- list(metrics = all_results, plot = final.plot)
  return(Result)
}

