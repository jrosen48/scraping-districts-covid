# scraping-districts-covid

This project uses [drake](https://github.com/ropensci/drake).

See:

 - [/_drake.R](/_drake.R) (runs the plan in a new session with `drake::r_make()`)
 - [/R/plan.R](R/plan.R) (the analysis plan)
 - [/R/functions.R](/R/functions.R) (functions used in the analysis plan)
 - [/R/packages.R](/R/packages.R) (functions used in the analysis plan)

The data used is from the [ELSI Table Generator](https://nces.ed.gov/ccd/elsi/tableGenerator.aspx) and is in this repository:

- [/district-data.csv](/district-data.csv)
