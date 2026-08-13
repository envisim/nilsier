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

### Using `pak`
Installing e.g. the development branch of `nilsier` requires a working installation of [`rust`](https://rust-lang.org/) (1.84.1).


```{r}
# Using devtools
# install.packages("pak");
pak::pkg_install("nilsier=github::envisim/nilsier/nilsier_rpkg@develop");
```

### Cloning the repo
Installing e.g. the development branch of `nilsier` requires a working installation of [`rust`](https://rust-lang.org/) (1.84.1).

```{bash}
git clone git@github.com:envisim/nilsier.git
git checkout develop
R CMD BUILD nilsier
R CMD INSTALL nilsier
```

