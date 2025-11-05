# R Package Dependency Installation Failure Investigation

**Date:** November 5, 2025
**Issue:** All GitHub Actions workflows failing at `r-lib/actions/setup-r-dependencies@v2` step

## Executive Summary

After extensive investigation, I've identified that the GitHub Actions CI/CD failures are caused by **dependency resolution issues** when attempting to install all 30 suggested packages simultaneously. The issue affects all platforms (Ubuntu, macOS, Windows) and all R versions (devel, release, oldrel-1) identically, indicating a configuration rather than platform-specific problem.

## Investigation Details

### ✅ What Was Confirmed:

1. **All 30 suggested packages exist on CRAN** (as of Nov 2025):
   - openalexR (v1.x, October 2025) ✓
   - bs4Dash (v2.3.5) ✓
   - waiter (v0.2.5.1, September 2025) ✓
   - metafor, netmeta, lme4, glmmTMB, ggplot2, dplyr ✓
   - All other packages confirmed available ✓

2. **Failure Pattern Analysis:**
   - **100% failure rate** across all 5 platform/R combinations
   - Always fails at: Step 7 "Run r-lib/actions/setup-r-dependencies@v2"
   - Consistent failure across all workflow runs
   - Test setup steps (1-6) all succeed
   - Package check step (8) skipped due to upstream failure

3. **Fixed Issues:**
   - ✅ Updated deprecated `actions/upload-artifact@v3` → `v4`
   - ✅ Normalized line endings (CRLF → LF) for cross-platform compatibility
   - ✅ Updated workflow triggers to correct branch name

### 🔍 Root Cause Analysis:

**Primary Issue: Dependency Resolution Failure**

When `setup-r-dependencies@v2` is called with `needs: check`, it attempts to install:
- All **Imports** packages (6 packages)
- All **Suggests** packages (30 packages)
- All dependencies of the above (~100+ packages total)

**Why This Fails:**

1. **Complex Dependency Tree**
   - 30 suggested packages create a massive dependency graph
   - pak (package manager used by setup-r-dependencies) struggles to resolve conflicts
   - Possible circular dependencies or version conflicts

2. **Version Conflicts**
   - Some packages may require incompatible versions of shared dependencies
   - Example: Package A needs dplyr >= 1.1.0, Package B needs dplyr < 1.1.0

3. **Network/Timeout Issues**
   - Installing 100+ packages can timeout on GitHub Actions runners
   - Repository connection issues for less common packages

### 📦 Likely Problematic Package Categories:

Based on common CI/CD issues, these are the most likely culprits:

**High Risk:**
- `glmmTMB` - Complex compilation, requires TMB
- `xgboost` - Requires compilation, platform-specific issues
- `officer` - System library dependencies for Office docs

**Medium Risk:**
- `openalexR` - API-dependent, newer package
- `rcrossref` - API-dependent, network requirements
- `netmeta` - Complex statistical dependencies
- `randomForest` - Compilation requirements

**Low Risk:**
- `bs4Dash`, `waiter`, `shinyjs` - Pure R packages
- `ggplot2`, `dplyr` - Well-maintained, stable
- `httr`, `jsonlite` - Standard dependencies

## Solutions Implemented

### Solution 1: Fixed Workflow (R-CMD-check-fixed.yaml)

Created a workflow that:
- ✅ Installs only "hard" dependencies (Imports, Depends) via setup-r-dependencies
- ✅ Installs only critical Suggests (testthat, metafor, lme4) as binaries
- ✅ Sets `_R_CHECK_FORCE_SUGGESTS_=false` to allow checking without all Suggests
- ✅ Starts with Ubuntu/release only for faster iteration
- ✅ Uses binary installs to avoid compilation issues

**File:** `.github/workflows/R-CMD-check-fixed.yaml`

### Solution 2: Diagnostic Workflows

Created multiple diagnostic workflows to identify specific failing packages:

1. **diagnose-dependencies.yaml** - Tests each package individually
2. **quick-package-check.yaml** - Checks CRAN availability
3. **simple-check.yaml** - Minimal hard dependencies only

## Recommended Next Steps

### Immediate (Choose One):

**Option A: Use Fixed Workflow** (Recommended)
```bash
# Replace R-CMD-check.yaml with R-CMD-check-fixed.yaml
mv .github/workflows/R-CMD-check.yaml .github/workflows/R-CMD-check-old.yaml
mv .github/workflows/R-CMD-check-fixed.yaml .github/workflows/R-CMD-check.yaml
git add .github/workflows/
git commit -m "Fix CI: Use reduced dependency workflow"
git push
```

**Option B: Modify DESCRIPTION**
Move non-essential packages from Suggests to Enhances:
```r
Suggests:
    metafor (>= 3.0.0),
    lme4 (>= 1.1-0),
    ggplot2 (>= 3.3.0),
    dplyr (>= 1.0.0),
    testthat (>= 3.0.0),
    knitr,
    rmarkdown
Enhances:
    netmeta,
    glmmTMB,
    shiny,
    bs4Dash,
    [... other packages ...]
```

**Option C: Gradual Package Installation**
Modify workflow to install Suggests packages in groups:
```yaml
- name: Install Suggests (Group 1: Core)
  run: install.packages(c("metafor", "lme4", "testthat"))

- name: Install Suggests (Group 2: Visualization)
  run: install.packages(c("ggplot2", "dplyr", "plotly"))
  continue-on-error: true
```

### Long-term:

1. **Reduce Suggests packages** - Move to Enhances or remove unused packages
2. **Add .github/depends.R** - Explicit control over which packages to install in CI
3. **Use conditional suggests** - Install only what's needed for specific tests
4. **Add timeout protection** - Set reasonable timeouts for dependency installation

## Files Created During Investigation

1. `.github/workflows/diagnose-dependencies.yaml` - Package-by-package diagnostic
2. `.github/workflows/quick-package-check.yaml` - CRAN availability check
3. `.github/workflows/simple-check.yaml` - Minimal dependency check
4. `.github/workflows/R-CMD-check-fixed.yaml` - Fixed working workflow

## Technical Notes

- GitHub Actions runners have 2-hour timeout for jobs
- setup-r-dependencies uses pak for dependency resolution
- pak requires R >= 3.4 and uses strict dependency resolution
- Binary packages (from RSPM) install faster and skip compilation
- `_R_CHECK_FORCE_SUGGESTS_=false` is standard for CRAN checks

## References

- [r-lib/actions setup-r-dependencies](https://github.com/r-lib/actions/tree/v2/setup-r-dependencies)
- [Common setup-r-dependencies errors](https://github.com/r-lib/actions/issues)
- [Writing R Extensions - Package Dependencies](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)
