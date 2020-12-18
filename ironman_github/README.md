IRONMAN data processing and export procedures
=====

Things that currently need updating (based on old data/knowledge)
- `summary.Rmd`: This one renders a nice `skimr` html across all of the SAS data sets, but got broken with a recent update in which one (or more) of the data sets no longer includes the primary key variable. A little digging will resolve this. *Note: the rendered html file should not be uploaded to GitHub because it outputs some raw PHI data*
- `summarize-proms.Rmd`: This isn't outdated, but if we want to make the html useful, we should spend some time ordering the factor responses so that they print in ordinal fashion.

