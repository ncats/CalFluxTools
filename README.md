# CalFluxTools

CalFluxTools is an R package for parsing, quality-controlling, visualizing,
transforming, and analyzing calcium flux plate-reader data. It is designed for
ScreenWorks-normalized data in `statAll` format and supports both paired and
unpaired study designs.

For a worked example, see the
[CalFluxTools example-data vignette](https://ncats.github.io/CalFluxTools/index.html).

## Installation

Install the Bioconductor dependency and then install CalFluxTools from GitHub:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("ComplexHeatmap")

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ncats/CalFluxTools")
```

## Quick start

Once a correctly formatted parameter manifest and its referenced data files are
in place, the complete workflow is run with one line of code:

```r
calflux_data <- CalFluxTools::processCalFluxData("path/to/parameter_manifest.xlsx")
```

The manifest controls the analysis. It identifies the input files, plate maps,
plate layout, output organization, parameters to retain, processing rules, and
analyses to run. `processCalFluxData()` uses those settings to load and organize
the data, run the configured workflow, write reports and figures, and return a
`CalFluxData` object containing the processed data and analysis results.

## Input files

A typical analysis uses:

- an Excel parameter manifest containing `Analysis_Settings` and
  `Parameter_Info` worksheets;
- one or more ScreenWorks-normalized `statAll` files; and
- CSV plate maps containing well-level sample annotations.

The filenames and locations of the data and plate maps are specified in the
manifest. Example manifests, plate maps, and `statAll` files are provided under
[`inst/extdata`](inst/extdata).

## Workflow and outputs

Depending on the manifest settings, CalFluxTools can:

- parse plate-reader data and attach well annotations;
- mask unused wells and handle blank or missing values;
- filter parameters and encode categorical measurements;
- transform paired plate reads relative to baseline;
- assess data completeness and reference-plate quality;
- generate plate views, heatmaps, bar charts, and z-prime summaries;
- run t-tests and principal component analysis; and
- run random-forest classification and export prediction summaries.

Reports, plots, spreadsheets, and logs are written beneath the configured root
directory. The returned `CalFluxData` object can also be used with exported
lower-level functions for custom exploration and analysis.

## Example workflow

The [example-data vignette](https://ncats.github.io/CalFluxTools/index.html)
demonstrates the package with the data included in this repository. It covers
the manifest-driven wrapper as well as lower-level parsing, quality-control,
transformation, and visualization functions.

## License

CalFluxTools is distributed under the GPL-2 license. See [LICENSE](LICENSE) for
details.
