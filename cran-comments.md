## Test Environments

* Local Windows 10 x64, R 4.4.2

## R CMD check results

0 errors | 0 warnings | 2 notes

## Submission Notes

This is a new submission.

MERIDIAN is a Shiny application packaged with golem. It imports several packages
for the interactive UI and statistical workflows. Report/export-related packages
are listed in Suggests and are used conditionally.

The remaining local notes are:

* New submission.
* The local Windows check environment was unable to verify current time.

Pandoc was available during the final local check.

The local check machine has TinyTeX installed, but Windows group policy blocks
execution of `pdflatex.exe` and `tlmgr.bat`. Because of that local policy, the
PDF manual could not be built in this environment when `manual = TRUE` was
requested. The HTML manual and all Rd checks passed.
