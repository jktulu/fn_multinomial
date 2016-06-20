**Description**

These functions modify functions in R packages _MASS_ [Venables & Ripley 2012] and _mlogit_ [Croissant 2013] in order to implement **backward-step procedure** for multinomial logit models and estimate **point elasticities** in objects of class _mlogit_. An additional function is provided to perform **Dominance Analysis** on _mlogit_ models following the procedure described in Luchman [2014]. The modification for point elasticities was informed by this discussion: http://stackoverflow.com/questions/25831729/marginal-effects-of-mlogit-in-r

**Functions**

`fitStepMultinom` implements backward-step multinomial logit estimation, whereby all covariates are included in the model and then iteratively removed or retained based on whether their marginal contribution to the variance of the response variable is statistically significant at p < .95. 
`decompMultinom` implements variance decomposition for mlogit objects. 
`effectsMultinom` estimates marginal effects of a given covariate on probability of the response variable. This functions a modified version of `effects.mlogit` found in package _mlogit_. 
`effectsMultinomWrap` is a function wrapper for `effectsMultinom`, calculating marginal effects for all covariates in the model.

	fitstepMultinom(dx, y, x, stepw = TRUE, reflevel = 1, clean = TRUE)
	decompMultinom(m)
	effectsMultinom(m, covariate = NULL, type = c("aa", "ar", "rr", "ra"), data = NULL)
	effectsMultinomWrap(m, type = "rr")
	
* `dx`: data.frame that contains vector of response variable and covariates in columns.
* `y`: name of response variable (character vector of length 1)
* `x`: right hand side string of formula without `~` listing names of all covariates separated by `+` or `|`
* `stepw`: boolean `TRUE` stepwise mode is implemented `FALSE` all covariates are included (default `TRUE`)
* `reflevel`:  integer indicating reference level of response variable, default `1`
* `clean`: boolean `TRUE`(default) data.frame is stripped of incomplete cases and ununsed factor levels
* `m`: an object of class `mlogit`
* `covariate`: character vector of length 1 providing the name of the covariate to estimate the marginal effect of
* `type`: absolute `"a"` versus `"r"` representation of the marginal effect. The first lettre refers to the response variable, the second to the covariate. The type for elasticities is `"rr"`.
* `data`: the original data for the model, which needs to be of class `mlogit.data`.

**References**

Croissant (2013). _mlogit: multinomial logit model_. R package version 0.2-4. http://CRAN.R-project.org/package=mlogit, accessed 1 June 2016.

Luchman, J. N. (2014). Relative Importance Analysis With Multicategory Dependent Variables:: An Extension and Review of Best Practices. _Organizational Research Methods_, 17(4), 452–471. http://doi.org/10.1177/1094428114544509

Venables, W. N. & Ripley, B. D. (2002). _Modern Applied Statistics with S_. Fourth Edition. Springer, New York.
