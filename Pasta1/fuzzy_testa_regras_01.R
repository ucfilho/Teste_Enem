rm(list=ls());
#  https://rdrr.io/cran/frbs/man/WM.html
#  https://cran.r-project.org/web/packages/frbs/vignettes/lala2015frbs.pdf

library(frbs)
library(readxl)
library("writexl")

# read the dataset
path = "C:/Users/User/Documents/Atividades_andamento/Nayana/fuzzy anfis"
setwd(path)

fileName = 'custos_versao_jun_17_2023.xlsx'
print(getwd())
df = read_excel(fileName)
# head(df,3)
columnsNames = c('Area', 'Presença de focos erosivos ao final da operação?',
                 'Água subterrânea contaminada?',
                 'Material de superficie/solo de cobretura contaminado?',
                 'Abertura de brecha', 'Retaludamento', 'Construção de vertedouro',
                 'Outras ações de fechamento', 'Focos erosivos.1')
columnSelect = which(names(df) %in% columnsNames )
data = df[, columnSelect]
data$y = df[,'Custo_barragem']



find_range <- function(data) {
  ranges <- apply(data, 2, function(col) c(min(col), max(col)))
  return(matrix(ranges, nrow = 2))
}

## Set the method and its parameters,
## for example, we use Wang and Mendel's algorithm
method.type <- "FIR.DM"  # "WM","ANFIS","DENFIS","FS.HGD"
##    type.mf ="GAUSSIAN" "TRAPEZOID"
##    num.labels = 3, 15
control_WM <- list(num.labels = 5, type.mf = "TRAPEZOID", type.defuz = "WAM",
                type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH",
                name = "sim-0")

control_ANFIS <-list(num.labels=5, max.iter=50, step.size = 0.01,
                     type.tnorm = "MIN", type.snorm = "MAX",
                     type.implication.func = "ZADEH")
control_TSK = list(num.labels=5,  max.iter = 100, step.size = 0.01,
                   type.tnorm = "MIN", type.snorm = "MAX", 
                   type.implication.func = "ZADEH")


# Use the function to find the range for the dataframe
range_matrix <- find_range(data)

num = nrow(df)
resp = c()

    
    X_train = df[, columnSelect]
    y_train <- df[,'Custo_barragem']
    X_test = df[ ,columnSelect]
    y_test = df[ ,'Custo_barragem']
    data.train = X_train
    data.train$y= y_train
    ## Learning step: Generate an FRBS model
    #     control = control_WM
    #     control = control_ANFIS
    #     control = control_ANFIS    
    frbs_model <- frbs.learn(data.train, range.data = range_matrix ,
                             method.type = method.type, control = control_TSK)


    # Make predictions
    features = as.matrix(X_test)
    predictions = predict(frbs_model, newdata = features)
    resp = append(resp,predictions)
    
    


plot(x=as.matrix(resp) , y= as.matrix(df[,'Custo_barragem']),
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)

cat(' cross validation  R^2=', cor(as.matrix(resp), as.matrix(df[,'Custo_barragem'])))
#print(summary(frbs_model))
df_rules=data.frame(frbs_model$rule)
#write_xlsx(df_rules,"df_rules.xlsx")
