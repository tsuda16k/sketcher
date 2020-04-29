## Resubmission
This is a resubmission.

CRAN maintainers suggestion:

- Please only write/save files if the user has specified a directory in the function themselves.
- Therefore please omit any default path = getwd() in writing functions.

So I removed any getwd() from default function parameters.

## Test environments
- Local: darwin15.6.0, R 3.6.1
- R-hub fedora-clang-devel (r-devel)
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)

## R CMD check results
0 errors | 0 warnings | 1 notes
1 note should be the first time submission note.

Also, r-hub reports a note, saying "Possibly mis-spelled words in DESCRIPTION: Tsuda (9:34)."  
But the spelling is OK.

## Downstream dependencies
There are currently no downstream dependencies for this package.
