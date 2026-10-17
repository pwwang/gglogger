## 0.1.9
- fix: preserve logged expressions and update S7 (v0.2.2.9000) package hooks (#2)
- ci: add missing 'any::remotes' package to workflow dependencies (0.1.8)

## 0.1.8
- feat: add support for functions not returning ggplot in register
- ci: bump R version to 4.3.3 and update documentation

## 0.1.7
- chore: update RoxygenNote to 7.3.3
- chore: use is_theme instead of deprecated is.theme from ggplot2
- chore: bump version to 0.1.6
- fix: adopt ggplot2 with version > 3.5.2 (#1)
- style: fix typos in DESCRIPTION

## 0.1.5
- style: fix cran issues
- feat: add argument name for the function to be registered
- style: fix cran submission
- chore: update git_rev in conda/meta.yaml
- style: update .gitignore to exclude CRAN-SUBMISSION file

## 0.1.4
- style: fix cran submission

## 0.1.3
- fix: add error handling for adding different object types

## 0.1.2
- feat: add warning for piping data using `%>%`
- docs: update README.md
- give up cran submission

## 0.1.1
- style: fix cran checks
- release: submit to cran (0.1.0)
- docs: update README.md
- docs: update README.md
- docs: add logo
- feat: add `register` function to export and document
- fix: fix function call to add_theme in gglogger.R
- feat: add gen_code method to generate the code for the plot regeneration
- chore: update README with limitations of gglogger
- chore: add error handling directly in `+.gg` function
- fix: keep the logs for evaluated plots
- ci: add GitHub Actions workflow for building and deploying the project
