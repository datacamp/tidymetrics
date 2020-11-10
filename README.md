# tidymetrics

**Authors:** Ramnath Vaidyanathan, [David Robinson](http://varianceexplained.org/)<br/>

<!-- badges: start -->
  [![R build status](https://github.com/datacamp/tidymetrics/workflows/R-CMD-check/badge.svg)](https://github.com/datacamp/tidymetrics/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test coverage](https://codecov.io/gh/datacamp/tidymetrics/branch/master/graph/badge.svg)](https://codecov.io/gh/datacamp/tidymetrics?branch=master)
<!-- badges: end -->

<!--

[![Travis build status](https://travis-ci.org/ramnathv/tidymetrics.svg?branch=master)](https://travis-ci.org/ramnathv/tidymetrics)
[![Codecov test coverage](https://codecov.io/gh/ramnathv/tidymetrics/branch/master/graph/badge.svg)](https://codecov.io/gh/ramnathv/tidymetrics?branch=master) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

-->

Dimensional modeling done the tidy way!

## What the package contains

The "cross by" family of functions, which prepare data to be aggregated in ways useful for dimensional modeling:

* `cross_by_periods`, which prepares data with a `date` column to be aggregated by calendar periods (day/week/month), rolling windows, or "X weeks ago"
* `cross_by_dimensions`, which adds an `All` level to each segment

Methods for annotating aggregated metrics with useful metadata:

* `create_metrics`, which gathers a table of metrics into a list of `tbl_metric` objects and attaches metadata to it
* `create_metric_group`, which annotates a group of metrics with the same dimensions as a `tbl_metric_group`

Verbs for working with dimensions in metric tables, including:

* `discard_dimensions` (and its inverse `keep_dimensions`), which filters for only the `All` segment of dimensions and removes those columns
* `condense_metric`, which retains only observations with one non-All dimension (in order to store a compact version that can still be explored one dimension at a time) 

## Code of Conduct

Please note that the 'tidymetrics' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
