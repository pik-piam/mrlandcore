# One-line description of this awesome package

R package **mrlandcore**, version **0.0.1.9001**

[![CRAN status](https://www.r-pkg.org/badges/version/mrlandcore)](https://cran.r-project.org/package=mrlandcore)  [![R build status](https://github.com/pik-piam/mrlandcore/workflows/check/badge.svg)](https://github.com/pik-piam/mrlandcore/actions) [![codecov](https://codecov.io/gh/pik-piam/mrlandcore/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrlandcore) 

## Purpose and Functionality

One-paragraph description of this awesome package.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrlandcore")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Kristine Karstens <karstens@pik-potsdam.de>.

## Citation

To cite package **mrlandcore** in publications use:

Karstens K (2024). _mrlandcore: One-line description of this awesome package_. R package version 0.0.1.9001, <https://github.com/pik-piam/mrlandcore>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrlandcore: One-line description of this awesome package},
  author = {Kristine Karstens},
  year = {2024},
  note = {R package version 0.0.1.9001},
  url = {https://github.com/pik-piam/mrlandcore},
}
```
