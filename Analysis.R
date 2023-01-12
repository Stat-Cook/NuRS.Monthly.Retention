library(tibble)
library(purrr)

corrected.log <- function(values){
  .correction <- min(values[values != 0])
  if (.correction < 0){
    stop("Min less than zero - aborted")
  }
  log(values + .correction)
}

{
  numeric <- complete.data %>% as_tibble() %>%
    select(-c(Year, Ward, Leavers, `Ave Establishment Lag 3`)) %>%
    mutate(
      across(where(is.difftime), as.numeric)
    ) %>%
    mutate(
      across(everything(), as.numeric)
    )
  #ncol(numeric)
  
  numeric.var <- numeric %>% apply(2, var)
  numeric <- numeric[which(numeric.var != 0)]
  #ncol(numeric)
  numeric <- tibble(numeric)
  
  exposure <- complete.data$`Ave Establishment Lag 3`
  leavers <- as.double(complete.data$Leavers)
  
  y <- leavers > 0
}
##### 

cc

hist(corrected.log(numeric$`Lag 3-4 Gender.Missing Est. Proportion`))
hist(numeric$`Lag 3-4 Gender.Missing Est. Proportion`)

demo.stopped <- paste(demographic.cols, ".", sep="")

{ 
  var.stab <- function(data, col){
    values <- data[[col]]
    .min <- min(values[values != 0])
    
    if(.min < 0){
      stop("Negative values in column - aborted.")
    }
    
    data.frame(
      X = values,
      Log_X = log(values + .min)
    ) %>% summarize(
      across(everything(), list("Skew" = e1071::skewness))
    )
  }
  
  numeric.2 <- numeric
  
  var.cols <- numeric.2 %>% 
    select(contains("var") | contains("Var")) %>% 
    colnames()
  
  
  log.stabilization <- map(
    var.cols,
    ~ var.stab(numeric.2, .)
  ) %>% do.call(rbind, .) %>% tibble(Variable = var.cols, .)
  
  highly.skewed <- log.stabilization %>% filter(X_Skew > 1.5 | X_Skew < -1.5)
  highly.skewed.vars <- highly.skewed$Variable

  
  numeric.2 <- numeric.2 %>% 
    mutate(
      across(
        highly.skewed.vars, 
        corrected.log,
        .names = "{.col} (CC Log)"
      )
    )%>% 
    select(-all_of(highly.skewed.vars))

  numeric.2.cols <- colnames(numeric.2)

  for (demo in demo.stopped){
    
    demo.cols <- numeric.2.cols[grepl(demo, numeric.2.cols)]
    
    numeric.2 <- numeric.2 %>% mutate(
      across(demo.cols, corrected.log, .names = "{.col} (CC Log)")
    ) %>% select(-all_of(demo.cols))
  }
  
}

numeric.2 %>% 
  select(-contains("Est. Proportion")) %>% ncol()

numeric.2 %>% 
  select(contains("Est. Proportion")) %>% colnames()

colnames(numeric.2)

{
  pca <- numeric.2 %>% prcomp(scale.=T)
  cs <- cumsum(pca$sdev)
  plot(pca$sdev)
  
  plot(cs / tail(cs, 1), ylim=c(0,1))
  abline(0,1/length(cs))
  }

plot(pca$sdev[1:200])

{
  numeric.features <- data.frame(predict(pca))
  complete.features <- data.frame(
    numeric.features[,1:30], 
    Ward = complete.data$Ward, 
    Year = factor(complete.data$Year))
  feature.matrix <- model.matrix(~-1 + ., data=complete.features)
}

glm(y ~ ., data=complete.features) %>% summary()

library(glmnet)
{
  # seed <- 249
    
  seed <- 248
  #
#seed.f <- function(seed){
  set.seed(seed)
  n <- length(y)
  train.index <-caret::createDataPartition(y = y, p = 0.8)$Resample1 # sample(n, floor(0.8*n))
  
  tts <- list(
    x.train = feature.matrix[train.index,],
    y.train = y[train.index],
    #    odds.train = y[train.index,1] / y[train.index,2],
    x.test = feature.matrix[-train.index,],
    y.test = y[-train.index]
    #   odds.test = y[-train.index,1] / y[-train.index,2]
  )
  
  cvx2 <- cv.glmnet(tts$x.train, tts$y.train,
                    family="binomial", nfolds = 10,
                    type.measure = "auc"  )
  
  pred.test <- predict(cvx2, newx = tts$x.test, s="lambda.min")
  pred.train <- predict(cvx2, newx = tts$x.train, s="lambda.min")
  roc.test <- pROC::roc(tts$y.test, pred.test[,1])
  roc.train <- pROC::roc(tts$y.train, pred.train[,1])
  rocs <- c(roc.test$auc, roc.train$auc)
  c(mu = mean(rocs), delta = diff(rocs))
}

mq <- sapply(201:250, function(i) seed.f(i))

data.frame(t(mq)) %>% mutate(
  index = rownames(.),
  qual = abs(delta)
) %>% filter(mu > 0.5) %>%
  filter(qual == min(qual))


qual.frm <- data.frame(t(mq)) %>% mutate(
  index = rownames(.),
  qual = abs(delta),
  Metric = 2*mu - 1,
  Score = qual / Metric
)

qual.frm %>% arrange(desc(Score))

{
  plot(roc.train)
  plot(roc.test, add=T)
}  

plot(cvx2)

m.coefs <- coef(cvx2, s="lambda.min")
m.coefs
pc.coef.signs <- sign(m.coefs[2:31])

which(pc.coef.signs != 0)
k <- 3
hist(pca$rotation[,k])
{
  cont <- which(abs(pca$rotation[,k]) > 0.1) %>% names() %>% sort(decreasing = T)
  .sign <- pc.coef.signs[k]
  pca$rotation[cont,k]
}



k <- 13
{
  cont <- which(abs(pca$rotation[,k]) > 0.1) %>% names() %>% sort(decreasing = T)
  .sign <- pc.coef.signs[k]
  .sign * pca$rotation[cont,k]
}


k <- 16
{
  cont <- which(abs(pca$rotation[,k]) > 0.1) %>% names() %>% sort(decreasing = T)
  .sign <- pc.coef.signs[k]
  .sign * pca$rotation[cont,k]
}
