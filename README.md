These functions modify functions in R packages _MASS_ [Venables & Ripley 2012] and _mlogit_ [Croissant 2013] in order to implement **backward-step procedure** for multinomial logit models and estimate **point elasticities** in objects of class _mlogit_. An additional function is provided to perform **Dominance Analysis** on _mlogit_ models following the procedure described in Luchman [2014]. The modification for point elasticities was informed by this discussion: http://stackoverflow.com/questions/25831729/marginal-effects-of-mlogit-in-r

**Functions:**

`fitStepMultinom` implements backward-step multinomial logit estimation, whereby all covariates are included in the model and then iteratively removed or retained based on whether their marginal contribution to the variance of the dependent variable is statistically significant at p < .95. Arguments:

  `dx`        data.frame that contains vector of dependent and independent variables in columns.
  
  `y`         name of dependent variable (character vector of length 1)
  
  `x`         right hand side string of formula without `~` listing names of all indepenent variables separated by `+` or `|`
  
  `stepw`     boolean `TRUE` stepwise mode is implemented `FALSE` all independent variables are included (default `TRUE`)
  
  `reflevel`  integer indicating reference level of dependent variable
  
  `clean`     boolean `TRUE`(default) data.frame is stripped of incomplete cases and ununsed factor levels

`decompMultinom` implements variance decomposition for mlogit objects 

**References:**

Croissant (2013). mlogit: multinomial logit model. R package version 0.2-4. http://CRAN.R-project.org/package=mlogit, accessed 1 June 2016.

Luchman, J. N. (2014). Relative Importance Analysis With Multicategory Dependent Variables:: An Extension and Review of Best Practices. Organizational Research Methods, 17(4), 452–471. http://doi.org/10.1177/1094428114544509

Venables, W. N. & Ripley, B. D. (2002). Modern Applied Statistics with S. Fourth Edition. Springer, New York.
