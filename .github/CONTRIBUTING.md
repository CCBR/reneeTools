# Contributing to reneeTools

## Proposing changes with issues

If you’ve found a bug, please file an issue that illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
If you want to make a change, it's a good idea to first [open an issue](https://code-review.tidyverse.org/issues/)
and make sure someone from the team agrees that it’s needed.

If you've decided to work on an issue,
[assign yourself to the issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/assigning-issues-and-pull-requests-to-other-github-users#assigning-an-individual-issue-or-pull-request)
so others will know you're working on it.

### Pull request process

- If you are a member of CCBR, you can clone this repository to your computer or development environment.
  Otherwise, you will first need to fork the package and clone your fork.

- Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`.
  If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.

- Create a Git branch for your pull request (PR). Give the branch a descriptive name for the changes you will make, such as `iss-99` if it is for a specific issue.

- Make your changes, write unit tests, and update the documentation as needed.

  Most changes to the code will also need unit tests to demonstrate that the changes work as intended.
  Use [`testthat`](https://testthat.r-lib.org/) to create your unit tests and test the code.
  Test files are organized as described in <https://style.tidyverse.org/tests.html>.
  Take a look at the existing code in this package for examples.

  If you have written a new function or changed the API of an existing function, you will need to update the function's roxygen2 comment.
  See instructions on writing roxygen2 comments here: <https://r-pkgs.org/man.html>.
  If the function is used in a vignette, you may also need to update vignette.

  Run `devtools::check()` to make sure the package still passes R CMD check.

- Commit your changes to git and push your changes to GitHub.
  Your commit messages should follow the [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/) format.
  You can use [`pre-commit`](https://ccbr.github.io/HowTos/GitHub/howto_precommit/)
  to enforce that your commits follow this format, as well as other code style checks.

- Once your branch is ready, create a PR on GitHub.
  The title of your PR should briefly describe the change.
  Follow the guide in the [PR template](/.github/CONTRIBUTING.md) to fill out the body of the PR.

- Wait for a maintainer to review your PR.
  We will do our best to follow the tidyverse code review principles: <https://code-review.tidyverse.org/>.
  The reviewer may suggest that you make changes before accepting your PR in order to improve the code quality or style.
  Once they approve the PR, they will merge it and the issue(s) it links will close automatically.

### Code style

- New code should follow the tidyverse [style guide](https://style.tidyverse.org).
  You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles,
  but please don't restyle code that has nothing to do with your PR.

- We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://roxygen2.r-lib.org/articles/rd-formatting.html), for documentation.

- We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.
  Contributions with test cases included are easier to accept.

## Helpful links for more information

- This contributing guide was adapted from the [tidyverse contributing guide](https://github.com/tidyverse/tidyverse/blob/main/.github/CONTRIBUTING.md)
- [tidyverse style guide](https://style.tidyverse.org)
- [tidyverse code review principles](https://code-review.tidyverse.org)
- [reproducible examples](https://www.tidyverse.org/help/#reprex)
- [R packages book](https://r-pkgs.org/)
- packages:
  - [usethis](https://usethis.r-lib.org/)
  - [devtools](https://devtools.r-lib.org/)
  - [testthat](https://testthat.r-lib.org/)
  - [styler](https://styler.r-lib.org/)
  - [roxygen2](https://roxygen2.r-lib.org)
