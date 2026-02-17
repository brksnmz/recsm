# Appendix — Data and build notes

## ESS subset

- File: `ess.csv`
- Countries: GB, DE, FR
- For variable descriptions, open the bundled HTML codebook: `ESS1e06_...subset codebook.html` in your browser.
- Common missing-value codes: 66/77/88/99 or blank strings. All chapters use `na_if()` to drop them before analysis.

## Extending the material

- Swap dependent variables: `pplhlp` (people helpful) or `pplfair` (people fair) can replace `ppltrst` without changing structure.
- Add country-level covariates by merging lookups (e.g., GDP or media freedom indices) before running multilevel models.
- Convert interaction plots to `ggplotly` for live demos if desired.


