# GitHub Actions CI/CD Workflows for LFA Package

This directory contains GitHub Actions workflows for continuous integration and deployment of the LFA R package.

## 📋 Workflows Overview

### 1. R-CMD-check.yaml
**Purpose:** Comprehensive package checking across multiple platforms and R versions

**Triggers:**
- Push to main/master/develop branches
- Pull requests to main/master/develop
- Daily at 00:00 UTC (dependency checks)
- Manual dispatch

**Test Matrix:**
- **Ubuntu:** R-devel, R-release, R-oldrel-1
- **macOS:** R-release
- **Windows:** R-release

**What it does:**
- Installs system dependencies (GDAL, PROJ, Cairo, etc.)
- Installs R package dependencies with caching
- Runs `R CMD check --as-cran`
- Validates package structure and metadata
- Runs all tests via testthat
- Checks documentation
- Uploads check results on failure

**Success criteria:** Package passes CRAN checks on all platforms

---

### 2. test-coverage.yaml
**Purpose:** Measure and track code coverage with Codecov

**Triggers:**
- Push to main/master/develop branches
- Pull requests to main/master/develop
- Manual dispatch

**What it does:**
- Runs full test suite with coverage tracking
- Generates Cobertura XML coverage report
- Uploads to Codecov for visualization
- Shows testthat output for debugging
- Uploads test results on failure

**Reports:** Coverage statistics available at codecov.io

---

### 3. pkgdown.yaml
**Purpose:** Build and deploy package documentation website

**Triggers:**
- Push to main/master branches
- Pull requests to main/master (preview only)
- Release events
- Manual dispatch

**What it does:**
- Builds pkgdown documentation site
- Generates function reference from roxygen2 comments
- Creates articles from vignettes
- Deploys to GitHub Pages (gh-pages branch)
- Only deploys on non-PR events

**Output:** https://mahmood726-cyber.github.io/LFA (when configured)

---

### 4. pr-check.yaml
**Purpose:** Fast feedback on pull requests without full multi-platform testing

**Triggers:**
- Pull request opened/synchronized/reopened

**What it does:**
- Quick test run on Ubuntu + R-release only
- Runs devtools::test() for rapid feedback
- Code style checking with styler (non-blocking)
- Linting with lintr (non-blocking)
- Posts comment to PR on completion

**Advantage:** Provides feedback in ~5-10 minutes vs 30-45 minutes for full R-CMD-check

---

## 🚀 Setup Instructions

### 1. Enable GitHub Actions

Ensure GitHub Actions is enabled for your repository:
1. Go to repository Settings → Actions → General
2. Select "Allow all actions and reusable workflows"
3. Save

### 2. Configure Codecov (Optional but Recommended)

For test coverage reporting:

1. Visit https://codecov.io
2. Sign in with GitHub
3. Add your repository
4. Copy the repository token
5. Add as GitHub secret:
   - Repository Settings → Secrets and variables → Actions
   - New repository secret: `CODECOV_TOKEN`
   - Paste token value

No additional configuration needed - the workflow handles upload automatically.

### 3. Enable GitHub Pages (For pkgdown)

To publish documentation website:

1. Go to repository Settings → Pages
2. Source: Deploy from a branch
3. Branch: `gh-pages` / `root`
4. Save

The pkgdown workflow will automatically deploy to this branch.

### 4. Add Status Badges (Optional)

Add these badges to your README.md:

```markdown
<!-- R-CMD-check -->
[![R-CMD-check](https://github.com/mahmood726-cyber/LFA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mahmood726-cyber/LFA/actions/workflows/R-CMD-check.yaml)

<!-- Test coverage -->
[![Codecov test coverage](https://codecov.io/gh/mahmood726-cyber/LFA/branch/main/graph/badge.svg)](https://codecov.io/gh/mahmood726-cyber/LFA?branch=main)

<!-- pkgdown -->
[![pkgdown](https://github.com/mahmood726-cyber/LFA/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/mahmood726-cyber/LFA/actions/workflows/pkgdown.yaml)
```

---

## 🔧 Troubleshooting

### Workflow fails on system dependencies

**Ubuntu:** Add missing libraries to the `apt-get install` section in workflows
**macOS:** Add missing formulae to the `brew install` section
**Windows:** Most dependencies available via rtools, but some may need special handling

### Tests fail on specific platforms

1. Check the uploaded artifacts for detailed logs
2. Common issues:
   - Path separators (use `file.path()` not `/` or `\\`)
   - Case-sensitive file systems (macOS/Linux vs Windows)
   - Locale/encoding differences
   - Floating-point precision (use `tolerance` in tests)

### Codecov upload fails

- Non-blocking by default (`fail_ci_if_error: false`)
- Check CODECOV_TOKEN is set if private repository
- Verify cobertura.xml is generated

### pkgdown deployment fails

- Ensure gh-pages branch exists (created automatically on first run)
- Check repository has Pages enabled
- Verify write permissions: `permissions.contents: write`

---

## 📊 Workflow Execution Times

Typical execution times (with cache):

- **PR-check:** 5-10 minutes (single platform)
- **Test-coverage:** 10-15 minutes
- **R-CMD-check:** 30-45 minutes (5 platforms in parallel)
- **pkgdown:** 10-15 minutes

First run without cache may take 2-3x longer.

---

## 🎯 CI/CD Best Practices Implemented

✅ **Multi-platform testing:** Ubuntu, macOS, Windows
✅ **Multi-version testing:** R-devel, release, oldrel
✅ **Dependency caching:** R packages cached between runs
✅ **Parallel execution:** All platforms tested simultaneously
✅ **Fast feedback:** Separate quick PR check workflow
✅ **Code coverage:** Tracked via Codecov
✅ **Automated documentation:** pkgdown deployed on push
✅ **Daily checks:** Catch dependency updates early
✅ **Artifact upload:** Debugging information preserved on failure
✅ **Comprehensive logging:** All test output captured

---

## 🔄 Workflow Triggers Summary

| Workflow | Push | PR | Schedule | Release | Manual |
|----------|------|----|---------:|---------|--------|
| R-CMD-check | ✅ | ✅ | Daily | - | ✅ |
| test-coverage | ✅ | ✅ | - | - | ✅ |
| pkgdown | ✅ | ✅* | - | ✅ | ✅ |
| pr-check | - | ✅ | - | - | - |

*pkgdown on PR builds preview but doesn't deploy

---

## 📚 Additional Resources

- [r-lib/actions](https://github.com/r-lib/actions) - R-specific GitHub Actions
- [R Packages (2e)](https://r-pkgs.org/) - Comprehensive R package development guide
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [pkgdown Documentation](https://pkgdown.r-lib.org/)
- [Codecov Documentation](https://docs.codecov.com/)

---

## 🛠 Maintenance

### Updating Workflows

Workflows use `r-lib/actions@v2` which is regularly updated. To update:

1. Check [r-lib/actions releases](https://github.com/r-lib/actions/releases)
2. Update `@v2` to latest version if needed
3. Review changelog for breaking changes
4. Test on a branch before merging to main

### Adding New Dependencies

When package dependencies change:

1. Update DESCRIPTION file (Imports/Suggests)
2. System dependencies auto-detected from DESCRIPTION
3. Manual system deps added to workflow YAML
4. Clear Actions cache if issues persist (Settings → Actions → Caches)

---

Last updated: 2025-11-05
