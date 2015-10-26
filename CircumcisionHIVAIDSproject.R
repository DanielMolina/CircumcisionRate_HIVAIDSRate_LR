library(XML)
library(car)

# African Circumsicion Rate Data Cleaning

unparsed = readHTMLTable(doc = "http://en.wikipedia.org/wiki/Prevalence_of_circumcision")
AfricaCR = unparsed[[1]]
AfricaCR[[3]] = NULL
AfricaCR = as.data.frame(apply(AfricaCR, 2, function(x) gsub('\\s+', '', x)))
AfricaCR = as.data.frame(apply(AfricaCR, 2, function(x) gsub('-', '', x)))
AfricaCR = as.data.frame(apply(AfricaCR, 2, function(x) gsub('ô', 'o', x)))
AfricaCR = as.data.frame(apply(AfricaCR, 2, function(x) gsub("'", '', x)))
colnames(AfricaCR) = c("Country", "CircumcisionRate")

# End

# Rate of HIV Infection Data Cleaning

HIVAIDS = read.table("~/Documents/STP429/STP429project2/hivrate.txt", header = FALSE)
HIVAIDS[[1]] = NULL
colnames(HIVAIDS) = c("Country", "HIVAIDSRate")

# End

# Merge data sets and make scatter plot and histograms

mergedset = merge(AfricaCR, HIVAIDS, by = "Country")
mergedset[2] = lapply(mergedset[2], as.numeric)
attach(mergedset)
plot(CircumcisionRate, HIVAIDSRate, xlab = "Circumcision Rate", ylab = "HIV/AIDS Rate", main = "Circumcision Rate vs HIV/AIDS Rate in Africa")
text(CircumcisionRate, HIVAIDSRate, labels = Country, cex = 0.5, pos = 1)
hist(CircumcisionRate)
hist(HIVAIDSRate)

#End

# Linear Model and Diagnostics
model = lm(HIVAIDSRate ~ CircumcisionRate)
summary(model)
yhat = fitted(model)
lines(CircumcisionRate, yhat, col = "red")

residuals = resid(model)
plot(yhat, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
qqnorm(residuals)
influenceIndexPlot(model, id.n=3, vars=c("Studentized","hat","Cook"), labels = Country)
detach(mergedset)
#End

# Remove outliers and run model and diagnostics again

mergedset.edit = mergedset[-c(21,31,37),]
attach(mergedset.edit)

model.edit = lm(HIVAIDSRate ~ CircumcisionRate)
summary(model.edit)
yhat.edit = fitted(model.edit)
plot(CircumcisionRate, HIVAIDSRate, xlab = "Circumcision Rate", ylab = "HIV/AIDS Rate", main = "Circumcision Rate vs HIV/AIDS Rate in Africa")
text(CircumcisionRate, HIVAIDSRate, labels = Country, cex = 0.5, pos = 1)
lines(CircumcisionRate, yhat.edit, col = "red")

residuals.edit = resid(model.edit)
plot(yhat.edit, residuals.edit, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
qqnorm(residuals.edit)
influenceIndexPlot(model.edit, id.n=3, vars=c("Studentized","hat","Cook"), labels = Country)
detach(mergedset.edit)

# End