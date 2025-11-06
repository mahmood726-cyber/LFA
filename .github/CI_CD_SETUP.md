# CI/CD Setup Guide for LFA Package

Complete guide for setting up and maintaining the CI/CD pipeline for the LFA meta-analysis package.

## 📑 Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Initial Setup](#initial-setup)
4. [Workflow Details](#workflow-details)
5. [Configuration](#configuration)
6. [Monitoring & Maintenance](#monitoring--maintenance)
7. [Troubleshooting](#troubleshooting)
8. [Best Practices](#best-practices)

---

## Overview

The LFA package uses GitHub Actions for continuous integration and deployment, implementing industry best practices:

### CI/CD Pipeline Components

```
┌─────────────────────────────────────────────────────────────┐
│                     Code Push / PR                          │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ├──────────────────┬─────────────────────┬────────────────┐
                     ▼                  ▼                     ▼                ▼
            ┌─────────────┐    ┌──────────────┐    ┌─────────────┐  ┌──────────┐
            │  PR Check   │    │ R-CMD-check  │    │  Coverage   │  │ pkgdown  │
            │  (5-10 min) │    │ (30-45 min)  │    │ (10-15 min) │  │(10-15min)│
            └─────────────┘    └──────────────┘    └─────────────┘  └──────────┘
                  │                    │                    │              │
                  │              ┌─────┴─────┐              │              │
                  │              ▼     ▼     ▼              │              │
                  │           Ubuntu macOS Windows          │              │
                  │              │     │     │              │              │
                  └──────────────┴─────┴─────┴──────────────┴──────────────┘
                                            │
                                            ▼
                                  ┌──────────────────┐
                                  │  Status Report   │
                                  │  ✅ All Passed   │
                                  └──────────────────┘
```

---

## Prerequisites

### Required

- [x] GitHub account with repository access
- [x] Repository with LFA package code
- [x] Valid DESCRIPTION file in R package format
- [x] Test suite in `tests/testthat/`

### Optional (for enhanced features)

- [ ] Codecov account (for coverage reporting)
- [ ] GitHub Pages enabled (for documentation)

---

## Initial Setup

### Step 1: Enable GitHub Actions

1. Navigate to your repository on GitHub
2. Go to **Settings** → **Actions** → **General**
3. Under "Actions permissions":
   - Select **Allow all actions and reusable workflows**
4. Under "Workflow permissions":
   - Select **Read and write permissions**
   - Check **Allow GitHub Actions to create and approve pull requests**
5. Click **Save**

### Step 2: Configure Branch Protection (Recommended)

Protect your main branch by requiring CI checks:

1. Go to **Settings** → **Branches**
2. Add rule for `main` or `master`
3. Check:
   - ✅ Require status checks to pass before merging
   - ✅ Require branches to be up to date before merging
4. Select status checks:
   - `R-CMD-check` (ubuntu-latest, r: release)
   - `test-coverage`
   - `quick-check` (from pr-check workflow)
5. Save changes

### Step 3: Set Up Codecov (Optional)

For test coverage tracking:

1. Visit https://codecov.io
2. Sign in with your GitHub account
3. Click **Add new repository**
4. Find and enable `mahmood726-cyber/LFA`
5. Copy the **Repository Upload Token**
6. In GitHub repository:
   - Go to **Settings** → **Secrets and variables** → **Actions**
   - Click **New repository secret**
   - Name: `CODECOV_TOKEN`
   - Value: [paste token]
   - Click **Add secret**

### Step 4: Enable GitHub Pages

For automated documentation:

1. Go to **Settings** → **Pages**
2. Under "Build and deployment":
   - Source: **Deploy from a branch**
   - Branch: **gh-pages** / **(root)**
3. Click **Save**
4. Note the URL (e.g., https://mahmood726-cyber.github.io/LFA)

### Step 5: Initial Workflow Run

1. The workflows are triggered automatically on push
2. Go to **Actions** tab to monitor first run
3. First run takes longer (15-30 min) due to cache building
4. Subsequent runs much faster (5-15 min)

---

## Workflow Details

### R-CMD-check.yaml

**Purpose:** Comprehensive multi-platform package validation

**Test Matrix:**
```yaml
Ubuntu + R-devel    # Catch upcoming R changes
Ubuntu + R-release  # Current CRAN version
Ubuntu + R-oldrel-1 # Support older R (1 version back)
macOS  + R-release  # Apple Silicon / Intel
Windows + R-release # Windows compatibility
```

**Checks Performed:**
- Package metadata validation
- Namespace consistency
- Documentation completeness
- Code syntax
- Example execution
- All tests via testthat
- CRAN policy compliance

**Duration:** ~30-45 minutes (parallel)

**Artifacts:** Check logs uploaded on failure

---

### test-coverage.yaml

**Purpose:** Measure code coverage and track over time

**Process:**
1. Run full test suite with `covr::package_coverage()`
2. Generate Cobertura XML report
3. Upload to Codecov
4. Calculate coverage metrics:
   - Line coverage
   - Branch coverage
   - Function coverage

**Target:** 80% coverage (configurable in `.codecov.yml`)

**Duration:** ~10-15 minutes

**Output:**
- Coverage badge
- Coverage reports on Codecov
- PR comments with coverage diff

---

### pkgdown.yaml

**Purpose:** Generate and deploy package website

**Content Generated:**
- Function reference (from roxygen2)
- Articles (from vignettes)
- News (from NEWS.md)
- README
- Search functionality

**Deployment:**
- Builds on every push to main
- Deploys to `gh-pages` branch
- Available at GitHub Pages URL

**Duration:** ~10-15 minutes

**Triggers:**
- Push to main/master
- Releases
- Manual dispatch

---

### pr-check.yaml

**Purpose:** Fast feedback on pull requests

**Benefits:**
- Results in 5-10 minutes
- Catches obvious issues early
- Runs in parallel with full R-CMD-check
- Non-blocking code style checks

**Checks:**
- Test suite execution
- Code style (styler)
- Linting (lintr)
- Automated PR comment

**Philosophy:** Fail fast, fix early

---

## Configuration

### Modifying Test Matrix

To add/remove platforms in `R-CMD-check.yaml`:

```yaml
strategy:
  matrix:
    config:
      # Add new configuration
      - {os: ubuntu-latest, r: '4.3.0'}  # Specific R version
      - {os: ubuntu-22.04, r: 'release'} # Specific Ubuntu
```

### Adjusting System Dependencies

For new R package dependencies requiring system libraries:

**Ubuntu (apt):**
```yaml
- name: Install system dependencies (Ubuntu)
  if: runner.os == 'Linux'
  run: |
    sudo apt-get install -y \
      libnewpackage-dev \
      another-package
```

**macOS (brew):**
```yaml
- name: Install system dependencies (macOS)
  if: runner.os == 'macOS'
  run: |
    brew install newpackage
```

### Customizing Coverage Thresholds

Edit `.codecov.yml`:

```yaml
coverage:
  status:
    project:
      default:
        target: 80%      # Overall target
        threshold: 2%    # Allowed decrease
    patch:
      default:
        target: 80%      # New code target
        threshold: 5%    # More lenient for patches
```

### Modifying Workflow Triggers

Edit trigger section in workflows:

```yaml
on:
  push:
    branches: [main, master, develop]  # Add branches
  pull_request:
    branches: [main]
    paths-ignore:                      # Ignore certain files
      - '**.md'
      - 'docs/**'
  schedule:
    - cron: '0 0 * * 0'               # Weekly instead of daily
```

---

## Monitoring & Maintenance

### Viewing Workflow Status

1. Go to repository **Actions** tab
2. Select workflow from left sidebar
3. View run history
4. Click run for detailed logs

### Status Badge

Add to README.md:

```markdown
[![R-CMD-check](https://github.com/mahmood726-cyber/LFA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mahmood726-cyber/LFA/actions/workflows/R-CMD-check.yaml)
```

### Email Notifications

GitHub sends emails on workflow failures to:
- Committer who triggered the run
- Repository watchers (if enabled)

Configure: GitHub Settings → Notifications → Actions

### Cache Management

GitHub caches R packages between runs. Clear cache if issues:

1. Go to **Settings** → **Actions** → **Caches**
2. Delete specific caches or all
3. Next run rebuilds cache

**Cache limits:**
- 10 GB per repository
- Automatically evict old caches
- Separate caches per workflow + OS + R version

---

## Troubleshooting

### Workflow Fails on System Dependencies

**Symptom:** Error installing R packages due to missing system libraries

**Solution:**
1. Check error log for missing library (e.g., `libxml2.so`)
2. Add to system dependencies section:
   ```yaml
   sudo apt-get install -y libxml2-dev
   ```
3. Commit and push

### Tests Pass Locally But Fail in CI

**Common causes:**

1. **Path issues:**
   - Use `file.path()` instead of `/` or `\\`
   - Use `tempdir()` for temporary files
   - Use `testthat::test_path()` for test fixtures

2. **Locale differences:**
   ```r
   # In tests, set explicit locale
   withr::local_locale(c(LC_COLLATE = "C"))
   ```

3. **Random failures:**
   ```r
   # Set seed for reproducibility
   set.seed(123)
   ```

4. **Timing issues:**
   ```r
   # Increase timeouts
   testthat::skip_on_ci()  # Skip flaky tests
   ```

### Codecov Upload Fails

**Check:**
1. Is `CODECOV_TOKEN` secret set?
2. Is repository public? (Token not needed for public repos)
3. Check Codecov service status

**Debug:**
```yaml
- uses: codecov/codecov-action@v3
  with:
    verbose: true  # Enable verbose logging
```

### pkgdown Deployment Fails

**Check:**
1. Does `gh-pages` branch exist?
2. Are Pages enabled in Settings?
3. Does workflow have write permissions?

**Fix permissions:**
```yaml
permissions:
  contents: write
  pages: write
```

### Windows-Specific Failures

**Common issues:**
1. **Backslashes in paths:** Use `file.path()`
2. **Line endings:** Ensure `.gitattributes` configured
3. **Case sensitivity:** Use exact case for file names
4. **System calls:** May need conditional logic:
   ```r
   if (Sys.info()["sysname"] == "Windows") {
     # Windows-specific code
   }
   ```

---

## Best Practices

### ✅ DO

- ✅ Test on multiple platforms before release
- ✅ Use caching to speed up workflows
- ✅ Write platform-independent code
- ✅ Keep workflows up to date (r-lib/actions@v2)
- ✅ Monitor workflow execution times
- ✅ Use `skip_on_cran()` for long-running tests
- ✅ Set explicit seeds for random tests
- ✅ Document system dependencies in DESCRIPTION
- ✅ Use meaningful commit messages (triggers workflows)
- ✅ Review workflow logs on failures

### ❌ DON'T

- ❌ Don't hardcode file paths
- ❌ Don't assume specific locales
- ❌ Don't write to package directory (read-only in check)
- ❌ Don't use `skip_on_ci()` unless necessary
- ❌ Don't ignore workflow failures
- ❌ Don't commit large files (slows cloning)
- ❌ Don't store secrets in code
- ❌ Don't test network-dependent code without mocking
- ❌ Don't use system-specific commands without checks

### Performance Optimization

1. **Reduce test time:**
   ```r
   # Mark slow tests
   test_that("slow test", {
     skip_on_cran()
     skip_if(as.logical(Sys.getenv("CI")))
     # Long-running test
   })
   ```

2. **Parallel testing:**
   ```yaml
   env:
     TESTTHAT_CPUS: 2  # Use 2 cores for tests
   ```

3. **Selective testing:**
   ```yaml
   on:
     pull_request:
       paths:
         - 'R/**'
         - 'tests/**'
         - 'DESCRIPTION'
   ```

### Security Considerations

1. **Never commit secrets**
   - Use GitHub Secrets for tokens
   - Use `.gitignore` for credentials

2. **Limit workflow permissions**
   ```yaml
   permissions:
     contents: read  # Minimal permissions
   ```

3. **Pin action versions**
   ```yaml
   uses: actions/checkout@v4  # Specific version, not @main
   ```

4. **Review workflow changes carefully**
   - Workflows can access secrets
   - Malicious PRs could expose secrets

---

## Workflow Update Schedule

### Regular Updates (Quarterly)

- [ ] Check r-lib/actions for updates
- [ ] Review deprecated R packages
- [ ] Update R version matrix (when new R releases)
- [ ] Check system dependency versions
- [ ] Review Codecov configuration

### After Major Changes

- [ ] Add new system dependencies if needed
- [ ] Update test configurations
- [ ] Check workflow execution times
- [ ] Verify all platforms still pass

---

## Support & Resources

### Documentation

- [r-lib/actions](https://github.com/r-lib/actions) - Official R Actions
- [GitHub Actions Docs](https://docs.github.com/en/actions)
- [R Packages Book](https://r-pkgs.org/)
- [testthat Documentation](https://testthat.r-lib.org/)
- [pkgdown Guide](https://pkgdown.r-lib.org/)

### Community

- [R-package-devel Mailing List](https://stat.ethz.ch/mailman/listinfo/r-package-devel)
- [rOpenSci Packages](https://ropensci.org/) - Examples of well-tested packages
- [GitHub Community Forums](https://github.community/)

### Getting Help

1. Check workflow logs (Actions tab)
2. Search GitHub Issues in r-lib/actions
3. Post on Stack Overflow with `[r] [github-actions]` tags
4. Open issue in LFA repository

---

## Changelog

### 2025-11-05 - Initial Setup

- ✅ Created 4 comprehensive workflows
- ✅ Configured multi-platform testing
- ✅ Integrated Codecov
- ✅ Set up pkgdown deployment
- ✅ Added PR quick checks
- ✅ Documented all processes

---

## Appendix: Workflow Files Summary

| File | Lines | Purpose |
|------|-------|---------|
| R-CMD-check.yaml | ~130 | Multi-platform package validation |
| test-coverage.yaml | ~80 | Code coverage tracking |
| pkgdown.yaml | ~75 | Documentation deployment |
| pr-check.yaml | ~100 | Fast PR feedback |
| README.md | ~350 | Workflow documentation |

**Total:** ~735 lines of CI/CD configuration

---

Last Updated: 2025-11-05
Version: 1.0.0
Maintainer: LFA Development Team
