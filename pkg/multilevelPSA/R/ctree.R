#' @include package.R
NA

#' Performs a recursive partitioning in a conditional inference framework for each level 2.
#' 
#' TODO: Need more details
#' 
#' @return a list of BinaryTree-class classes for each level 2
#' @export multilevelCtree
multilevelCtree <- function(vars, formula, level2) {
	partyPlyr <- function(x) {
		excludeVars = names(x) %in% c(level2)
		x = x[,!excludeVars]
		tmp.party = ctree(formula, data=x)
		return(tmp.party)
	}
	party.results = dlply(vars, level2, partyPlyr, .progress='text')
	return(party.results)
}

#' Returns a data frame with two columns corresponding to the level 2 variable
#' and the leaves from the conditional inference trees.
#' 
#' @return a data frame
#' @export getStrata
getStrata <- function(party.results) {
	df = data.frame(level2 = character(), strata=numeric)
	for(i in names(party.results)) {
		strata = where(party.results[i][[1]])
		df = rbind(df, data.frame(level2 = rep(i, length(strata)), strata=strata))
	}
	return(df)
}
