# data analysis for Working Memory Sums task
library(plotrix)
# intialize grouping variables arrays
allConSubjAcc = c()
allConMedRT = c()
allConSumTenAcc = c()
allConSumNotTenAcc = c()
allConNumPers = c()
allConRTSumTen = c()
allConRTSumNotTen = c()

allCussSubjAcc = c()
allCussMedRT = c()
allCussSumTenAcc = c()
allCussSumNotTenAcc = c()
allCussNumPers = c()
allCussRTSumTen = c()
allCussRTSumNotTen = c()

######################  con group   ######################################
baseFolder = "/Users/rorden/Documents/MATLAB/Working-Memory-Sums-master (4)/data/con"
fileList = list.files(baseFolder, pattern = ".csv", full.names = TRUE)
numberOfFiles = length(fileList)

for (i in 1:numberOfFiles) {
  file = fileList[i]
  data = read.csv(file)
  
  subjAcc = round(mean(data$accuracy)*100, digits = 2)
  sumIsTenTrials = round(mean(data$accuracy[data$trialType == 1])*100,digits=2)
  sumNotTenTrials = round(mean(data$accuracy[data$trialType == 0])*100,digits=2)
  averageRT = round(mean(data$RT[data$RT<999]), digits =2)
  medianRT = round(median(data$RT[data$RT < 999]),digits=2)
  numPers = length(data$RT[data$RT<0.1]) # perseverations are RT less than 100ms
  RTSumTen = round(median(data$RT[data$trialType == 1 & data$RT < 999]),digits=2)
  RTSumNotTen = round(median(data$RT[data$trialType == 0 & data$RT < 999]),digits=2)
  
  allConSubjAcc = append(allConSubjAcc, subjAcc)
  allConMedRT = append(allConMedRT, medianRT)
  allConSumTenAcc = append(allConSumTenAcc, sumIsTenTrials)
  allConSumNotTenAcc = append(allConSumNotTenAcc, sumNotTenTrials)
  allConNumPers = append(allConNumPers, numPers)
  allConRTSumTen = append(allConRTSumTen, RTSumTen)
  allConRTSumNotTen = append(allConRTSumNotTen, RTSumNotTen)
  
  
}

dataForCard_zConWM = scale(allConSubjAcc)

######################  cuss group   ######################################
baseFolder = "/Users/rorden/Documents/MATLAB/Working-Memory-Sums-master (4)/data/cuss"
fileList = list.files(baseFolder, pattern = ".csv", full.names = TRUE)
numberOfFiles = length(fileList)

for (i in 1:numberOfFiles) {
  file = fileList[i]
  data = read.csv(file)
  
  subjAcc = round(mean(data$accuracy)*100, digits = 2)
  sumIsTenTrials = round(mean(data$accuracy[data$trialType == 1])*100,digits=2)
  sumNotTenTrials = round(mean(data$accuracy[data$trialType == 0])*100,digits=2)
  averageRT = round(mean(data$RT[data$RT<999]), digits =2)
  medianRT = round(median(data$RT[data$RT < 999]),digits=2)
  numPers = length(data$RT[data$RT<0.1]) # perseverations are RT less than 100ms
  RTSumTen = round(median(data$RT[data$trialType == 1 & data$RT < 999]),digits=2)
  RTSumNotTen = round(median(data$RT[data$trialType == 0 & data$RT < 999]),digits=2)
  
  allCussSubjAcc = append(allCussSubjAcc, subjAcc)
  allCussMedRT = append(allCussMedRT, medianRT)
  allCussSumTenAcc = append(allCussSumTenAcc, sumIsTenTrials)
  allCussSumNotTenAcc = append(allCussSumNotTenAcc, sumNotTenTrials)
  allCussNumPers = append(allCussNumPers, numPers)
  allCussRTSumTen = append(allCussRTSumTen, RTSumTen)
  allCussRTSumNotTen = append(allCussRTSumNotTen, RTSumNotTen)
  
}

dataForCard_zCussWM = scale(allCussSubjAcc)

t = t.test(allConSubjAcc, allCussSubjAcc, var.equal = TRUE)
t

t = t.test(allConMedRT, allCussMedRT, var.equal = TRUE)
t

t = t.test(allConSumTenAcc, allCussSumTenAcc, var.equal = TRUE)
t

t = t.test(allConSumNotTenAcc, allCussSumNotTenAcc, var.equal = TRUE)
t

t = t.test(allConNumPers, allCussNumPers, var.equal = TRUE)
t

t = t.test(allConRTSumTen, allCussRTSumTen, var.equal = TRUE)
t

t = t.test(allConRTSumNotTen, allCussRTSumNotTen, var.equal = TRUE)
t

t = t.test(allConRTSumTen, allConRTSumNotTen, paired = TRUE, var.equal = TRUE)
t

t = t.test(allCussRTSumTen, allCussRTSumNotTen, paired = TRUE, var.equal = TRUE)
t


myVarToPlotA = allConSumTenAcc
myVarToPlotB = allCussSumTenAcc

barCenters = barplot(c(mean(myVarToPlotA), mean(myVarToPlotB)),
                     main = "Subj Mean Acc Sum 10: Con vs. Cuss",
                     xlab = "Group",
                     ylim = c(0, 100))
# segments(barCenters, mean(myVarToPlotA) - std.error(myVarToPlotA) * 2, barCenters,
#          mean(myVarToPlotB) + std.error(myVarToPlotB) * 2, lwd = 1.5)
# arrows(barCenters, mean(myVarToPlotA) - std.error(myVarToPlotA) * 2, barCenters,
#        mean(myVarToPlotB) + std.error(myVarToPlotB) * 2, lwd = 1.5, angle = 90,
#        code = 3, length = 0.05)



