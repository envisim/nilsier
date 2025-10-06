# nilsier -- design-based estimators for NILS
This repo contains an R-package which provides design-based estimators and variance estimators used
in the [NILS-programs](https://www.slu.se/om-slu/organisation/institutioner/skoglig-resurshushallning/miljoanalys/nils/).
NILS is an acronym for the National Inventories of Landscapes in Sweden, a group of environmental
monitoring programs governed by the [Swedish University of Agricultural Sciences](https://slu.se)
(SLU).

## Installation
### From CRAN
```{r}
install.packages("nilsier");
```

### Using `devtools` or `remotes`
Installing e.g. the development branch of `nilsier`:

```{r}
# Using devtools
# install.packages("devtools");
devtools::install_github("envisim/nilsier", ref = "develop");
# or for the

# Using remotes
# install.packages("remotes");
remotes::install_github("envisim/nilsier", ref = "develop");
```

### Cloning the repo
```{bash}
git clone git@github.com:envisim/nilsier.git
git checkout develop
R CMD BUILD nilsier
R CMD INSTALL nilsier
```

