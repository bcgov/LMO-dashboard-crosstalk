[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

# LMO-dashboard-crosstalk

### Usage

To run you first need to download 4 files from 4castviewer:

1.  demand_industry.xlsx: the demand file for ALL occupations and 8 regions, by industry.

2.  demand_occupation.xlsx: the demand for ALL industries and 8 regions, by occupation.

3.  employment_industry.xlsx: the employment file for ALL occupations and 8 regions, by industry.

4.  employment_occupation.xlsx: the employment file for ALL industries and 8 regions, by occupation.

-   "ALL" indicates aggregated across all values, and the 8 regions are B.C. plus the 7 LMO regions (no sub-aggregates)

-   For the employment files the dates need to be current year till the end. (for current year + cagr calculation).

-   For the demand files the dates need to be next year till the end year (for 10 year sum of demand components).

There are two scripts that are required for the analysis, they need to be run in order:

-   01_preprocess.R
-   02_lmo_dull.qmd

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/LMO-dashboard-crosstalk/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```         
Copyright 2024 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```

------------------------------------------------------------------------

*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.*
