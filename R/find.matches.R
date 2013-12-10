###############################################################################
#
# Copyright (C) 2013  Drew Griffith
#
# For more information please visit my blog at http://drewgriffith15.tumblr.com/
###############################################################################
#' Set up a dataframe for regression
#'
#' This function, when supplied a vector, looks for matches based on correlation
#' or coefficient determination and returns a data frame preparing for a regression model.
#' The function also outputs "newdata" to be used in the forecast horizon if needed, and
#' the location in the data set where the match begins.
#'
#' @param data vector or times series
#' @param n.hist number of data points used for Y in a regression model
#' @param n.fore number of data points in the forecast horizon
#' @param n.match number of matches requested from data set
#' @param model linear, ves - variable elasticity,  or ces - constant elasticity
#' @param use.cd whether to use the coefficient determination or correlation
#' @export
###############################################################################

find.matches <- function(data, n.hist = 24, n.fore = 6, n.match=NULL,
 	model = c("linear","ves","ces"), use.cd = FALSE)
{
require(xts)
origdata = coredata(data)
n.data = NROW(origdata)
model = match.arg(model)
if (model =="ces") {
	Y = round(log(origdata[((n.data-n.hist)+1):n.data]),4)
} else { Y = origdata[((n.data-n.hist)+1):n.data]
}
if (is.null(n.match)) {
	n.match = floor(n.hist*.25)
}
if (model=="ves") {
	n.match = floor(n.match/2)
}
# calc correlation
matches = rep(NA, n.data)
for(i in 1:(n.data-(n.hist+n.fore))) {
  window = origdata[i:(n.hist+(i-1))]
  matches[i] = cor(Y, window)
}
# use CD or correlation
if (use.cd==TRUE){matches = round(matches^2,6)
} else {matches = round(matches,6)}
# find matches
max.index = c()
max.cor = c()
temp = matches
	temp[ temp < mean(matches, na.rm=TRUE) ] = NA
for(i in 1:n.match) {
	if(any(!is.na(temp))) {
		index = which.max(temp)
    correl = temp[index]
		max.index[i] = index
    max.cor[i] = correl # or CD...
		temp[max(0,index):min(n.data,(index +
			(n.fore+n.hist)))] = NA
	}
}
# Get CD for output only
if (use.cd==TRUE){max.cd = max.cor
} else {max.cd = round(max.cor^2,6)}
# model
n.match = NROW(max.index)
X = matrix(NA, nr=(n.match), nc=(n.hist))
temp = origdata
for(i in 1:n.match) {
	X[i,] = temp[max.index[i]:(max.index[i]+(n.hist-1))]
}
if (model=="ves") {
	Z = log(X)
	X = data.frame(t(rbind(X,Z)))
	df = cbind(data.frame(Y=Y),as.data.frame(X))
} else if (model=="ces") {
	X = t(log(X))
	df = cbind(data.frame(Y=Y),data.frame(X))
} else { X = t(X)
	df = cbind(data.frame(Y=Y),data.frame(X))
}
# newdata formation
X = matrix(NA, nr=(n.match), nc=(n.fore))
temp = origdata
for(i in 1:n.match) {
	X[i,] = temp[(max.index[i]+n.hist):((max.index[i]+
		n.hist+n.fore)-1)]
}
if (model=="ves") {
	Z = log(X)
	newdf = data.frame(t(rbind(X,Z)))
} else if (model=="ces") {
	X = t(log(X))
	newdf = data.frame(X)
} else { X = t(X)
	newdf = data.frame(X)
}
out = list(df,newdf,max.index,max.cd)
names(out)[1] = "rmodel"
names(out)[2] = "fmodel"
names(out)[3] = "matchindx"
names(out)[4] = "matchcd"
return(out)
}