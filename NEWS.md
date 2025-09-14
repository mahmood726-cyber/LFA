# Create a simple NEWS.md file
cat("# cbamm 0.1.0

* Initial CRAN release
* Implements cumulative meta-analysis
* Provides transport weights for generalizability
* Includes visualization functions
", file = "NEWS.md")

# Then restart the release process
devtools::release()
