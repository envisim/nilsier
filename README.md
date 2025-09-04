# nilsier -- design-based estimators for NILS
This repo contains an R-package which provides design-based estimators and variance estimators used
in the NILS-programs.
NILS is an acronym for the National Inventories of Landscapes in Sweden, a group of environmental
monitoring programs governed by the [Swedish University of Agricultural Sciences](https://slu.se)
(SLU).

## Installation
### Using `devtools` or `remotes` (recommended)
```{R}
# Using devtools
# install.packages("devtools");
devtools::install_github("envisim/nilsier");

# Using remotes
# install.packages("remotes");
remotes::install_github("envisim/nilsier");
```

### Cloning the repo
```{bash}
git clone git@github.com:envisim/nilsier.git
R CMD BUILD nilsier
R CMD INSTALL nilsier
```

