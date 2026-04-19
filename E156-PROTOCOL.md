# E156 Protocol — `LFA`

This repository is the source code and dashboard backing an E156 micro-paper on the [E156 Student Board](https://mahmood726-cyber.github.io/e156/students.html).

---

## `[77]` CBAMM: Collaborative Bayesian Adaptive Meta-Analysis with Population Transportability and Sequential Monitoring

**Type:** methods  |  ESTIMAND: Transported risk ratio (95% CI)  
**Data:** BCG vaccine dataset (13 studies) with simulated target population covariates

### 156-word body

Can a single R package unify population transportability, sequential monitoring, and Bayesian adaptive inference for personalized meta-analysis? We developed cbamm, implementing entropy-balanced transport weights for target population adjustment, O Brien-Fleming alpha-spending boundaries for sequential monitoring, and a Gibbs sampler with Savage-Dickey Bayes Factors, validated on the 13-study BCG vaccine dataset. The package provides cumulative meta-analysis with required information size calculation, population-specific re-weighting, and a multi-persona engine offering methodological, clinical, and statistical interpretive perspectives. Transported weights shifted the pooled risk ratio from 0.61 (95% CI 0.52 to 0.72) to 0.66, a 14 percent reduction in estimated effectiveness, while the Bayes Factor of 142.3 provided extreme evidence for efficacy. Trial sequential analysis correctly identified that interim significance in 1958 did not cross monitoring boundaries until all 13 studies accumulated. These methods enable population-specific estimates with sequential validity guarantees from standard meta-analytic datasets. Transportability weights remain limited by study-level covariate reporting, incomplete in approximately 40 percent of published trials.

### Submission metadata

```
Corresponding author: Mahmood Ahmad <mahmood.ahmad2@nhs.net>
ORCID: 0000-0001-9107-3704
Affiliation: Tahir Heart Institute, Rabwah, Pakistan

Links:
  Code:      https://github.com/mahmood726-cyber/LFA
  Protocol:  https://github.com/mahmood726-cyber/LFA/blob/main/E156-PROTOCOL.md
  Dashboard: https://mahmood726-cyber.github.io/lfa/

References (topic pack: trial sequential analysis (TSA)):
  1. Wetterslev J, Thorlund K, Brok J, Gluud C. 2008. Trial sequential analysis may establish when firm evidence is reached in cumulative meta-analysis. J Clin Epidemiol. 61(1):64-75. doi:10.1016/j.jclinepi.2007.03.013
  2. Pogue JM, Yusuf S. 1997. Cumulating evidence from randomized trials: utilizing sequential monitoring boundaries for cumulative meta-analysis. Control Clin Trials. 18(6):580-593. doi:10.1016/S0197-2456(97)00051-2

Data availability: No patient-level data used. Analysis derived exclusively
  from publicly available aggregate records. All source identifiers are in
  the protocol document linked above.

Ethics: Not required. Study uses only publicly available aggregate data; no
  human participants; no patient-identifiable information; no individual-
  participant data. No institutional review board approval sought or required
  under standard research-ethics guidelines for secondary methodological
  research on published literature.

Funding: None.

Competing interests: MA serves on the editorial board of Synthēsis (the
  target journal); MA had no role in editorial decisions on this
  manuscript, which was handled by an independent editor of the journal.

Author contributions (CRediT):
  [STUDENT REWRITER, first author] — Writing – original draft, Writing –
    review & editing, Validation.
  [SUPERVISING FACULTY, last/senior author] — Supervision, Validation,
    Writing – review & editing.
  Mahmood Ahmad (middle author, NOT first or last) — Conceptualization,
    Methodology, Software, Data curation, Formal analysis, Resources.

AI disclosure: Computational tooling (including AI-assisted coding via
  Claude Code [Anthropic]) was used to develop analysis scripts and assist
  with data extraction. The final manuscript was human-written, reviewed,
  and approved by the author; the submitted text is not AI-generated. All
  quantitative claims were verified against source data; cross-validation
  was performed where applicable. The author retains full responsibility for
  the final content.

Preprint: Not preprinted.

Reporting checklist: PRISMA 2020 (methods-paper variant — reports on review corpus).

Target journal: ◆ Synthēsis (https://www.synthesis-medicine.org/index.php/journal)
  Section: Methods Note — submit the 156-word E156 body verbatim as the main text.
  The journal caps main text at ≤400 words; E156's 156-word, 7-sentence
  contract sits well inside that ceiling. Do NOT pad to 400 — the
  micro-paper length is the point of the format.

Manuscript license: CC-BY-4.0.
Code license: MIT.

SUBMITTED: [ ]
```


---

_Auto-generated from the workbook by `C:/E156/scripts/create_missing_protocols.py`. If something is wrong, edit `rewrite-workbook.txt` and re-run the script — it will overwrite this file via the GitHub API._