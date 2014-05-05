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

# 1.
# check number of projects in each range
amount = data[,4]

amt2K = dim(data[amount<=2000,])[1]
amt2.5K = dim(data[amount>2000 & amount<=5000,])[1]
amt5.10K = dim(data[amount>5000 & amount <=10000,])[1]
amt10.20K = dim(data[amount>10000 & amount <=20000,])[1]
amt20.30K = dim(data[amount>20000 & amount <=30000,])[1]
amt30.40K = dim(data[amount>30000 & amount <=40000,])[1]
amt40.50K = dim(data[amount>40000 & amount <=50000,])[1]
amt50.100K = dim(data[amount>50000 & amount <=100000,])[1]
amt100.400K = dim(data[amount>100000 & amount <=400000,])[1]

# general function for given industry and target amount
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




##################### probability calculation: logit model #####################
################### DRAFT *Does not work. Just copied* DRAFT ###################

nllX = matrix(0,row,(1+9+8+4))
colnames(nllX)  = c("a", c(rownames(indTable[1:10,]),"Other","<5K","<10K","<20K",
                           "<30K","<40K","<50K","<100K","<400K","<30","30","40","45","60",">60"))
# dX[,1] = 1

# Change age columns
for(i in 1:row){
  if(pac[i,1] >= 20 & pac[i,1] <= 39){
    dX[i,2] = 1
  }
  else {
    if(pac[i,1] >= 40 & pac[i,1] <= 64){
      dX[i,3] = 1
    }
    else {
      if(pac[i,1] >= 65){
        dX[i,4] = 1
      }
    }
  }
}

# Change sex colmun
for(i in 1:row){
  if(pac[i,2] == 1){
    dX[i,5] = 1
  }
}

# change race column
for(i in 1:row){
  if(pac[i,3] == 1){
    dX[i,6] = 1
  }
}

# Change the education
for(i in 1:row){
  if(pac[i,8] == 39){
    dX[i,7] = 1
  }
  else {
    if(pac[i,8] >= 40){
      dX[i,8] = 1
    }
  }
}

# make Y col vector
Y = matrix(0, row, 1)
for(i in 1:row){
  if(pac[i,9] == 1){
    Y[i,1] = 1
  }
}

# 2.
nll <- function(beta){
  beta = matrix(beta, ncol= 1, nrow = 8)
  return(-(-sum(log(1+exp(dX %*% beta)))+ sum(t(Y) %*% (dX %*% beta))))
}

# test function
betaVec = c(1:8) 
nll(betaVec) #returns a scalar

# minimize and find the optimal beta vector
betaHat <- optim(c(rep(1,8)), f = nll, method = "BFGS")
betaHat$par












