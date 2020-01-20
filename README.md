# gantt

[![Build Status](https://travis-ci.org/yourgithub/gantt.png?branch=master)](https://travis-ci.org/yourgithub/gantt)  [![codecov](https://codecov.io/gh/yourgithub/gantt/branch/master/graph/badge.svg)](https://codecov.io/gh/yourgithub/gantt)

<img src = "man/images/logo.png" align = "right" width="120"/>

### 2. Travis

Now you can go to [Travis](https://travis-ci.org/profile/yourgithub) and turn on continuous integration for your new package. You may need to click the "Sync account" button to get your new package to show up in the list.

If you have a codecov account, running your tests on Travis will trigger the code coverage job. No additional configuration is necessary

### 3. Appveyor

Go to [Appveyor's new project page](https://ci.appveyor.com/projects/new) and select your new repository from the list. Then you can go to the [badges](https://ci.appveyor.com/project/yourgithub/gantt/settings/badges) page, copy the markdown code it provides, and paste it up with the other badges above. (Their badge API has a random token in it, so `skeletor` can't include it in the template for you.)


## Installing

    # install.packages("devtools")
    devtools::install_github("piotr-ole/gantt", build_vignettes=TRUE)

### Running tests

`$ make test`. Requires the [testthat](https://github.com/hadley/testthat) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=logging`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

### Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.
