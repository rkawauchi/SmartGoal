####################### data cleaning and analysis #######################
data = read.csv("/home/oski/SmartGoal/notebooks/data/raw/indiegogo.csv")
require(stringr)
dim(data)[1]
# no. of projects retrieved:  10,764


######## data clearning

### filter only USD projects
data = subset(data, str_sub(data[,4],-3,-1) == 'USD')
dim(data)[1]
# no. of USD projects: 9,189


### celarn duration: remove negative duration
durationClean = function(data){
  du = data[,3]
  duCheck = nrow(data[as.numeric(du) < 0 ,])
  if(duCheck > 0){
    for(i in 1:duCheck){
      count = i
      badRow = which.min(data[,3])
      data = data[-badRow,,drop=FALSE]
    }
  }
  return(data)
}

data = durationClean(data)
dim(data)[1]
# no. of useful projects: 8,584

### clean amount_raised: remove 'USD' character to be read as numeric
data[,4] = sub('USD$', '', data[,4])

### clean amount_goal: remove '$' and ',' to be read as numeric
data[,5] = sub('\\$', '', data[,5])
data[,5] = sub('\\,', '', data[,5]) # do this twice
data[,5] = sub('\\,', '', data[,5])
data[,6] = sub('\\,', '', data[,6])
data = data.frame("location"=data[,1],"category"=data[,2],"duration"=data[,3],"amount_raised"=as.numeric(data[,4]),"amount_goal"=as.numeric(data[,5]),"percent"=as.numeric(data[,6]))

save(data,file='indiegogo.rda') # for later use, save the cleaned file.


######## data-analysis

# decide which cut-off of category
catAnalysis = function(data){
  indTable = table(data[,2])
  indTable = indTable[order(-indTable)]
  lenCat = length(indTable)
  indTable = cbind("index" = rep(1:lenCat), "No._of_project"=indTable)
  par(mfrow=c(1,1))
  plot(indTable, ylab = 'No. of Projects', xlab= 'Project Index')
  return(indTable)
}

catTable = catAnalysis(data)
# 24 industries and we will group them into 11; top 10, plus other
catName = rownames(catTable)
data[,2] = factor(data[,2],levels=catName)

# mean of each category
numcols = sapply(data,class) %in% c('integer','numeric')
catMeanSummary = aggregate(data[,numcols],data['category'],mean)

# decide which duation to focus
duAnalysis = function(data){
  duTable = table(data[,3])
  duTable = duTable[order(-duTable)]
  lenDu = length(duTable)
  duTable = cbind(as.numeric(rownames(duTable)), "No._of_project"=duTable)
  plot(duTable, main="All Durations", ylab = 'No. of Projects', xlab= 'Duration Index')
  plot(duTable[1:5,], main="Top 5 Durations", ylab = 'No. of Projects', xlab= 'Duration Index')
  abline(v=c(duTable[1:5,1]),col='red')
  return(duTable)
}

duAnalysis(data)
# focus on top 4 durations of 30,40,45,60 (47 is adj of 45)

# comparison between industry's raised_amount (took log)
boxPlot = function(){
  category = data[,2]
  indNames = row.names(catTable)
  par(mfrow=c(1,5))
  for(i in 1:length(indNames)){
    filterInd = data[category == indNames[i],]
    if(class(data[category == indNames[i],]) == "character"){
      boxplot(log(as.numeric(filterInd[4])), xlab=indNames[i], ylim=c(0,20))
      msg1 = paste('mean:', ceiling(mean(as.numeric(filterInd[4]))))
      msg2 = paste('max:', ceiling(max(as.numeric(filterInd[4]))))
      legend('top', c(msg1,msg2))
    }else{
      boxplot(log(as.numeric(filterInd[,4])), xlab=indNames[i], ylim=c(0,20))
      msg1 = paste('mean:', ceiling(mean(as.numeric(filterInd[,4]))))
      msg2 = paste('max:', ceiling(max(as.numeric(filterInd[,4]))))
      legend('top', c(msg1,msg2))
    }
  }
}

boxPlot()


####################### Best Duration Advise #######################

# general function for given industry and target amount

# category to choose:
# [1] "Film"           "Community"      "Music"          "Education"     
# [5] "Health"         "Small Business" "Technology"     "Theater"       
# [9] "Art"            "Sports"         "Writing"        "Animals"       
# [13] "Video / Web"    "Food"           "Environment"    "Religion"      
# [17] "Dance"          "Gaming"         "Fashion"        "Photography"   
# [21] "Design"         "Politics"       "Comic"          "Transmedia"

# target fund-raising amount to choose (column number):
#      [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]   [,8]
# [1,]    0  5000 10000 20000 30000 40000 50000  100000
# [2,] 5000 10000 20000 30000 40000 50000 100000 400000

bestDuration = function(data, category, index){
  amountMat = matrix(0,2,8)
  amountMat[1,] = c(0,5000,10000,20000,30000,40000,50000,100000)
  amountMat[2,] = c(5000,10000,20000,30000,40000,50000,100000,400000)
  cat = data[,2]
  filterCat = data[cat == category,]
  goal = filterCat[,5]
  filterGoal = filterCat[goal>amountMat[1,index] & goal<=amountMat[2,index],]
  perc = filterGoal[,6]
  filterDone = filterGoal[perc >= 100,]
  # get duration that majority of projects pick
  duration = filterDone[,3]
  durationMat = table(duration)
  max.duration = names(durationMat[which.max(durationMat)])
  u = format(round(mean(duration),0), nsmall = 0)
  # plot the distribution
  par(mfrow=c(1,2))
  hist(duration, main="No. of projects for given durations", col="deepskyblue1", border=FALSE, breaks=20)
  abline(v=mean(duration),col="red", lwd = 2)
  abline(v=max.duration,col="darkorange1", lwd = 2)
  msg1 = paste("mean: ",u)
  msg2 = paste("most: ",max.duration)
  legend("topright",c(msg1,msg2), text.col = c("red","darkorange1"), bty='n', cex=1)
  den = density(duration)
  plot(den, main="Density of frequency",xlab = "duration", col="limegreen")
  polygon(den, col="limegreen", border=FALSE)
  abline(v=mean(duration),col="red", lwd = 2)
  abline(v=max.duration,col="darkorange1", lwd = 2)
  legend("topright",c(msg1,msg2), text.col = c("red","darkorange1"), bty='n', cex=1)
  ans = cbind("Best Duration"=u, "Most Picked Duration"=max.duration)
  return(ans)
}

bestDuration(data,"Film",3)



##################### probability calculation: logit model prep #####################
# 1.
# Make Logit model base matrix
intercep = 1
nCat = 24
nTarget = 9
nDuration = 6
nY = 1
nRow = dim(data)[1]
nTotal = intercep + nCat + nTarget + nDuration + nY

baseX = data.frame(data, matrix(0,nRow, nTotal))
baseX[,7]=1
colnames(baseX) = c('location','category','duration','amount_raised','amount_goal','percent',"a", c(rownames(catTable[1:24,]),"<5K","<10K","<20K","<30K","<40K","<50K","<100K","<400K",">400K","<30d","30d","40d","45d","60d",">60d","Y"))

# 2.
#################### assign category columns
catChange = function(baseX){
  names = rownames(catTable[1:24,])
  category = baseX[,2]
  for(i in 1:24){
    baseX[category == names[i],i+7] = 1
  }
  #for(i in 11:24){
  #  baseX[category == names[i],18] = 1
  #}
  return(baseX)
}

baseX = catChange(baseX)

#################### assign target amount columns
targetChange = function(baseX){
  targetList = c(0,5000,10000,20000,30000,40000,50000,100000,400000,10000000000000)
  target = baseX[,5]
  for(i in 1:9){
    baseX[target>targetList[i] & target<=targetList[i+1],i+31] = 1
  }
  return(baseX)
}

baseX = targetChange(baseX)

#################### assign duration columns
durationChange = function(baseX){
  durationList = c(30,40,45,60,61)
  duration = baseX[,3]
  baseX[duration<30,41] = 1
  baseX[duration>60,46] = 1
  for(i in 1:4){
    baseX[duration>=durationList[i] & duration<durationList[i+1],i+41] = 1
  }
  return(baseX)
}

baseX = durationChange(baseX)

#################### assign Y

yChange = function(baseX){
  percent = baseX[,6]
  baseX[percent>=100,47] = 1
  return(baseX)
}

baseX = yChange(baseX)
X = baseX[,8:46]
Y = baseX[,47]

# 2.
# Logit Regression
dataLogit = cbind(Y,X)

modelLogit = glm(Y ~ ., data = dataLogit, family = "binomial")
beta = summary(modelLogit)$coef[,1]

#####################  logit model #####################

# Generalized Function!

# category to choose: (Film = 1 ~ Comic = 23)
# [1] "Film"           "Community"      "Music"          "Education"     
# [5] "Health"         "Small Business" "Technology"     "Theater"       
# [9] "Art"            "Sports"         "Writing"        "Animals"       
# [13] "Video / Web"    "Food"           "Environment"    "Religion"      
# [17] "Dance"          "Gaming"         "Fashion"        "Photography"   
# [21] "Design"         "Politics"       "Comic"

# target fund-raising amount to choose (column number):
#      [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]   [,8]
# [1,]    0  5000 10000 20000 30000 40000 50000  100000
# [2,] 5000 10000 20000 30000 40000 50000 100000 400000

# fundraising campaign duration to choose (column number):
#       [,1] [,2] [,3] [,4] [,5]
# [1,]  <30   30   40   45   60 


predict = function(cat, target, duration){
  predicX = c(1,rep(0,36))
  catName = catName # 1:24
  beta = beta
  # assign industry category
  catN = match(cat,catName) +1
  predicX[catN]=1
  # assign target amount
  targetN = target + 24
  predicX[targetN] = 1
  # assign duration
  durationN = duration + 32
  predicX[durationN] = 1
  predicY = predicX %*% beta
  successP = plogis(predicY)
  return(successP)
}

predict('Technology',1,2)



