###############################################################################
#
# Copyright (C) 2014  Drew Griffith
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
find.matches <- function(data, n.hist = 35, n.fore = 15, n.match=NULL,
 	model = c("linear","ves","ces"), use.cd = TRUE)
{
require(xts)
origdata = coredata(data)
n.data = NROW(origdata)
model = match.arg(model)
if (model =="ces") {
	Y = round(log10(origdata[((n.data-n.hist)+1):n.data]),4)
} else { Y = origdata[((n.data-n.hist)+1):n.data]
}
if (is.null(n.match)) {
	n.match = floor(n.hist*.25)
}
if (model=="ves") {
	n.match = floor(n.match/2)
}

# correlation table
correlation.table = rep(NA, n.data)
for(i in 1:(n.data-(n.hist+n.fore))) {
  window = origdata[i:(n.hist+(i-1))]
  correlation.table[i] = cor(Y, window)
}

# CD table
cd.table = round(abs(correlation.table)^2,6)

# find matches
max.index = c()
max.cor = c()

if (use.cd==TRUE){temp = cd.table
} else {temp = correlation.table}

if (use.cd==TRUE){
for(i in 1:n.match) {
	index = which.max(temp)
  c = temp[index]
	max.index[i] = index
  max.cor[i] = c
	#temp[max(0,index):min(n.data,(index +
	#	(n.fore+n.hist)))] = NA 12.10.13
  temp[max(0,index)] = NA
}} else {for(i in 1:n.match) {
  index = which.max(temp)
  c = temp[index]
  max.index[i] = index
  max.cor[i] = c
  temp[max(0,index):min(n.data,(index +
  	(n.fore+n.hist)))] = NA}}

# model
n.match = NROW(max.index)
X = matrix(NA, nr=(n.match), nc=(n.hist))
temp = origdata
for(i in 1:n.match) {
	X[i,] = temp[max.index[i]:(max.index[i]+(n.hist-1))]
}
if (model=="ves") {
	Z = log10(X)
	X = data.frame(t(rbind(X,Z)))
	df = cbind(data.frame(Y=Y),as.data.frame(X))
} else if (model=="ces") {
	X = t(log10(X))
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
	Z = log10(X)
	newdf = data.frame(t(rbind(X,Z)))
} else if (model=="ces") {
	X = t(log10(X))
	newdf = data.frame(X)
} else { X = t(X)
	newdf = data.frame(X)
}

out = list(df,newdf,max.index)
names(out)[1] = "rmodel"
names(out)[2] = "fmodel"
names(out)[3] = "matchindx"
return(out)
}