# One-line description of this awesome package

R package **mrlandcore**, version **1.1.9**

[![CRAN status](https://www.r-pkg.org/badges/version/mrlandcore)](https://cran.r-project.org/package=mrlandcore)  [![R build status](https://github.com/pik-piam/mrlandcore/workflows/check/badge.svg)](https://github.com/pik-piam/mrlandcore/actions) [![codecov](https://codecov.io/gh/pik-piam/mrlandcore/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrlandcore) [![r-universe](https://pik-piam.r-universe.dev/badges/mrlandcore)](https://pik-piam.r-universe.dev/builds)

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

In case of questions / problems please contact Felicitas Beier <beier@pik-potsdam.de>.

## Citation

To cite package **mrlandcore** in publications use:

Beier F, Karstens K, Alves M, Philipp Dietrich J, Leon Bodirsky B, Hoetten D, Humpenoeder F, Heinke J, v. Jeetze P, Mishra A, Beier F, Wirth S, Chen D, Kreidenweis U (2024). _mrlandcore: One-line description of this awesome package_. R package version 1.1.9, <https://github.com/pik-piam/mrlandcore>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrlandcore: One-line description of this awesome package},
  author = {Felicitas Beier and Kristine Karstens and Marcos Alves and Jan {Philipp Dietrich} and Benjamin {Leon Bodirsky} and David Hoetten and Florian Humpenoeder and Jens Heinke and Patrick {v. Jeetze} and Abhijeet Mishra and Felcitas Beier and Stephen Wirth and David Chen and Ulrich Kreidenweis},
  year = {2024},
  note = {R package version 1.1.9},
  url = {https://github.com/pik-piam/mrlandcore},
}
```
