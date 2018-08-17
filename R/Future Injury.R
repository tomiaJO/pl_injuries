##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

## set parallel processing
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in prepared training & test sets
data_train    <- readRDS(paste(path_Data, "data_train.RDS",    sep = "/"))
data_train_7  <- readRDS(paste(path_Data, "data_train_7.RDS",  sep = "/"))
data_train_14 <- readRDS(paste(path_Data, "data_train_14.RDS", sep = "/"))
data_train_21 <- readRDS(paste(path_Data, "data_train_21.RDS", sep = "/"))
data_train_28 <- readRDS(paste(path_Data, "data_train_28.RDS", sep = "/"))


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Control function for caret
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tuning grid for caret
tg_rf <- expand.grid(.mtry = c(2:7),
                     .splitrule = "gini",
                     .min.node.size = c(25, 50, 100))

Sys.time()

## injured
## RF
set.seed(93)
m_rf_0 <- train(injured ~ .,
                data = data_train,       
                method     = "ranger",
                metric     = "ROC",
                trControl  = ctrl,
                tuneGrid   = tg_rf,
                num.trees = 1000,
                importance = "impurity")

saveRDS(object = m_rf_0, file = paste(path_Models, "m_rf_0.RDS", sep = "/"))
rm(m_rf_0)
Sys.time()

## 7 days
set.seed(93)
m_rf_7 <- train(injured_7days ~ .,
                data = data_train_7,       
                method     = "ranger",
                metric     = "ROC",
                trControl  = ctrl,
                tuneGrid   = tg_rf,
                num.trees = 1000,
                importance = "impurity")

saveRDS(object = m_rf_7, file = paste(path_Models, "m_rf_7.RDS", sep = "/"))
rm(m_rf_7)
Sys.time()

## 14 days
set.seed(93)
m_rf_14 <- train(injured_14days ~ .,
                 data = data_train_14,       
                 method     = "ranger",
                 metric     = "ROC",
                 trControl  = ctrl,
                 tuneGrid   = tg_rf,
                 num.trees = 1000,
                 importance = "impurity")

saveRDS(object = m_rf_14, file = paste(path_Models, "m_rf_14.RDS", sep = "/"))
rm(m_rf_14)
Sys.time()

## 21 days
set.seed(93)
m_rf_21 <- train(injured_21days ~ .,
                 data = data_train_21,       
                 method     = "ranger",
                 metric     = "ROC",
                 trControl  = ctrl,
                 tuneGrid   = tg_rf,
                 num.trees = 1000,
                 importance = "impurity")

saveRDS(object = m_rf_21, file = paste(path_Models, "m_rf_21.RDS", sep = "/"))
rm(m_rf_21)
Sys.time()

## 28 days
set.seed(93)
m_rf_28 <- train(injured_28days ~ .,
                 data = data_train_28,       
                 method     = "ranger",
                 metric     = "ROC",
                 trControl  = ctrl,
                 tuneGrid   = tg_rf,
                 num.trees = 1000,
                 importance = "impurity")

saveRDS(object = m_rf_28, file = paste(path_Models, "m_rf_28.RDS", sep = "/"))
rm(m_rf_28)
Sys.time()

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MODEL EVALUATION
m_rf_0  <- readRDS(file = paste(path_Models, "m_rf_0.RDS",     sep = "/"))
m_rf_7  <- readRDS(file = paste(path_Models, "m_rf_7.RDS",     sep = "/"))
m_rf_14  <- readRDS(file = paste(path_Models, "m_rf_14.RDS",     sep = "/"))
m_rf_21  <- readRDS(file = paste(path_Models, "m_rf_21.RDS",     sep = "/"))
m_rf_28  <- readRDS(file = paste(path_Models, "m_rf_28.RDS",     sep = "/"))

model_list <- list("Game Day"     = m_rf_0,
                   "7 days"       = m_rf_7,
                   "14 days"      = m_rf_14,
                   "21 days"      = m_rf_21,
                   "28 days"      = m_rf_28
)

resamples_object <- resamples(model_list)

p_resampling <- resamples_object$values %>%
                  tidyr::gather(key= "Resample", factor_key = F) %>%
                  data.table::setnames(c("Model~Metric", "Value")) %>%
                  mutate(model  = stringr::str_split(`Model~Metric`, "~", simplify = T)[,1],
                         metric = stringr::str_split(`Model~Metric`, "~", simplify = T)[,2]) %>%
                  mutate(metric = factor(metric,
                                         levels = c("ROC",       "Sens",              "Spec"),
                                         labels = c("AUC (ROC)", "Sensitivity (TPR)", "Specificy (TNR)"))) %>%
                  mutate(model = factor(model, levels = names(model_list))) %>%
                  ggplot(aes(x = model, y = Value, fill = model)) +
                  geom_boxplot() +
                  facet_wrap(~metric, ncol = 1) +
                  labs(title    = "Comparison of model performances",
                       subtitle = "Logit, Probit, GLMNet, Random Forest and XGBoost",
                       x        = "Model",
                       y        = "",
                       caption  = "Note: Results are based on 50 resamples of training data") +
                  theme_minimal() +
                  theme(plot.title  =  element_text(size = 20, face = "italic"),
                        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                        axis.text.y = element_text(size = 12))

f_calc_auc(pp, truth)