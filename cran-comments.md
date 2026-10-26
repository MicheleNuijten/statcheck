## Test environments

* local Windows install, R 4.6.1
* win-builder (R-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is about the maintainer change: my email address changed from
  m.b.nuijten@uvt.nl to m.b.nuijten@tilburguniversity.edu. I am the same
  maintainer; Tilburg University moved to a new email domain. Mail to the old
  address still reaches me, so I can confirm the change from either address.

## Changes in this version

* The package logo is now a PNG file, as requested by CRAN.
* PDFs are now converted with pdftools instead of the external Xpdf program,
  so users no longer need a separate system installation.
* See NEWS.md for all changes.

## Reverse dependencies

I checked the 1 reverse dependency (effectcheck, which lists statcheck in 
Suggests) and found no new problems.