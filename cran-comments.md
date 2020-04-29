## Resubmission
This is a resubmission. Suggestions by CRAN and my responses are listed below.

- Please always write TRUE and FALSE instead of T and F.
  Also never name your variables T or F.

T and F were replaced by TRUE and FALSE.

- Please don't use install.packages() in your vignettes.

Removed install.packages() from vignettes.

- Please add \value to .Rd files regarding exported methods and explain
  the functions results in the documentation. Please write about the
  structure of the output (class) and also what the output means.
  (If a function does not return a value, please document that too, e.g.
  \value{No return value, called for side effects} or similar)
  F.i: im_gray.Rd

Documents for function output were added.

## Test environments
local: darwin15.6.0, R 3.6.1
R-hub fedora-clang-devel (r-devel)
R-hub windows-x86_64-devel (r-devel)
R-hub ubuntu-gcc-release (r-release)

## R CMD check results
0 errors | 0 warnings | 1 notes
1 note should be the first time submission note.

Also, r-hub reports a note, saying "Possibly mis-spelled words in DESCRIPTION: Tsuda (9:34)."
But the spelling is OK.

## Downstream dependencies
There are currently no downstream dependencies for this package.
