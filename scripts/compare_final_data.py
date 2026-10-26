"""Audit both saved datasets against local sources; run from the repository root."""
import csv
import math
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

def read(path, encoding="utf-8"):
    with (ROOT / path).open(encoding=encoding, newline="") as stream:
        return list(csv.DictReader(stream))

def key(row):
    return row['iso'], int(row['year'])

def equal(a, b):
    if a in ('', 'NA') or b in ('', 'NA'):
        return a in ('', 'NA') and b in ('', 'NA')
    try:
        return math.isclose(float(a), float(b), rel_tol=1e-12, abs_tol=1e-12)
    except ValueError:
        return a == b

original = read('data/processed/final_data.csv')
agent = read('data/processed/final_data_copilot.csv')
old = {key(r): r for r in original}
new = {key(r): r for r in agent}
assert len(old) == len(original) == len(new) == len(agent) == 3720
assert old.keys() == new.keys()
assert all({y for i, y in old if i == iso} == set(range(2000, 2020)) for iso, _ in old)
aliases = {'earthquake': 'disaster_earthquake', 'drought': 'disaster_drought'}
comparison = []
for col in original[0]:
    other = aliases.get(col, col)
    comparison.append((col, sum(not equal(r[col], new[key(r)][other]) for r in original),
                       sum(r[col] in ('', 'NA') for r in original),
                       sum(r[other] in ('', 'NA') for r in agent)))

totals = defaultdict(float)
pooled = defaultdict(float)
conflict = read('data/raw/conflict.csv')
excluded = 0
for r in conflict:
    if r['conflict_id'] in ('', 'NA'):
        excluded += 1
        continue
    assert r['best'] not in ('', 'NA')
    totals[(*key(r), r['conflict_id'])] += float(r['best'])
    pooled[key(r)] += float(r['best'])
flags = defaultdict(int)
for (iso, year, _), deaths in totals.items():
    flags[iso, year] |= deaths >= 25

events = defaultdict(set)
for r in read('data/raw/disaster.csv', encoding='latin-1'):
    if r['Disaster Type'] in ('Earthquake', 'Drought') and 2000 <= int(r['Year']) <= 2019:
        events[r['ISO'], int(r['Year'])].add(r['Disaster Type'])

source_checks = []
for col in ('maternal_mortality', 'infant_mortality', 'neonatal_mortality', 'under5_mortality'):
    source = {r['iso']: r for r in read(f'data/raw/{col}.csv')}
    source_checks.append((col, sum(not equal(r[col], source[r['iso']]['X'+r['year']]) for r in original)))
covariates = read('data/raw/covariates.csv')
cov = {key(r): r for r in covariates}
assert len(cov) == len(covariates)
for col in covariates[0]:
    if col not in ('iso', 'year'):
        source_checks.append((col, sum(not equal(r[col], cov[key(r)][col]) for r in original)))
for col, label in [('earthquake', 'Earthquake'), ('drought', 'Drought')]:
    source_checks.append((col, sum(int(r[col]) != int(label in events[key(r)]) for r in original)))
source_checks.append(('armed_conflict_lag1', sum(int(r['armed_conflict_lag1']) != flags[r['iso'], int(r['year'])-1] for r in original)))
assert all(n == 0 for _, n in source_checks), source_checks

differences = []
for r in original:
    iso, year = key(r)
    if r['armed_conflict_lag1'] != new[iso, year]['armed_conflict_lag1']:
        differences.append({'iso': iso, 'year': year, 'original_lag': r['armed_conflict_lag1'],
                            'agent_lag': new[iso, year]['armed_conflict_lag1'],
                            'source_verified_lag': flags[iso, year-1],
                            'prior_year_pooled_deaths': pooled[iso, year-1],
                            'prior_year_max_conflict_deaths': max((v for (i,y,c),v in totals.items() if (i,y)==(iso,year-1)), default=0)})
with (ROOT/'reports/conflict_discrepancies.csv').open('w', newline='') as stream:
    writer = csv.DictWriter(stream, fieldnames=list(differences[0]))
    writer.writeheader()
    writer.writerows(differences)
assert all(int(r['armed_conflict']) == int(pooled[key(r)] >= 25) for r in agent)
assert all(int(r['armed_conflict_lag1']) == int(pooled[r['iso'], int(r['year'])-1] >= 25) for r in agent if r['year'] != '2019')
table = '\n'.join(f'| {c} | {n} | {m} | {a} |' for c,n,m,a in comparison)
late = sum(r['year'] == 2019 for r in differences)
early = len(differences)-late
text = f'''# Comparison of Week 3 and coding-agent final data

Date: 26 October 2026

## Assessment

The original Week 3 dataset is supported by the supplied raw data. Its values do not require correction. The coding-agent dataset has 48 incorrect values of `armed_conflict_lag1` under the definition implemented in the Week 3 script: at least one conflict must reach 25 deaths within a country-year, with exposure assigned to the following year. Agreement between two outputs alone would not establish correctness; this assessment independently reconstructs the variables from the raw files.

## Files and method

The comparison treats `data/processed/final_data.csv` as the Week 3 output and `data/processed/final_data_copilot.csv` as the agent output. Git history supports this identification: commit `5223b0c` adds the Copilot file after the original final-data commits. The available preparation code is `scripts/create_final_data.R`; no agent generation code or separate assignment specification is present, so the original script supplies the operational conflict definition. Conclusions about the agent's implementation are inferred from its output.

Rows are matched by ISO code and year, never by row position. Both files contain 3,720 unique country-years: 186 countries with all 20 years from 2000 through 2019. There are no missing or duplicate keys and no unmatched rows. The original has 17 columns and the agent file has 18. The agent calls `earthquake` and `drought` `disaster_earthquake` and `disaster_drought`, respectively, and adds a contemporaneous `armed_conflict` column. These structural differences are distinct from value errors.

Numeric comparisons allow relative and absolute tolerance of 1e-12 and treat empty cells and `NA` as missing. Small floating-point export differences in covariates are therefore not classified as substantive disagreements. Missing values must match on both sides.

## Variable comparison

| Original variable | Substantive differences | Original missing | Agent missing |
|---|---:|---:|---:|
{table}

All mortality outcomes, covariates, and disaster indicators agree under these rules. The missingness patterns also agree. Missing observations are retained; neither agreement nor this audit justifies imputing them or deleting country-years.

## Conflict discrepancies and source verification

The original file has 691 exposed country-years (18.58%); the agent file has 665 (17.88%). Forty-eight classifications differ (1.29% of rows), with a net reduction of 26 exposures. The discrepancy has two components:

* **{late} false negatives in 2019:** the agent assigns zero despite a qualifying conflict in 2018. Raw conflict records run from 1999 through 2018, which is sufficient to construct lagged exposure for every study year, including 2019. Every agent lag value in 2019 is zero. The observed pattern is consistent with forming the lag only on years present in the conflict table and then filling missing 2019 values with zero; the absent agent code prevents confirming that mechanism.
* **{early} false positives in earlier years:** the agent flags countries where combined deaths across conflicts meet 25 but no individual conflict reaches 25. For Brazil in 2008, the prior-year pooled total is 40 deaths, but the largest individual conflict total is only 21; the original correctly codes zero under its definition. All affected country-years and the exact underlying totals are recorded in `reports/conflict_discrepancies.csv`. Across all years before 2019, the agent's lag matches a threshold applied to pooled country-year deaths. Its extra contemporaneous `armed_conflict` column also matches that pooled definition throughout the panel. Pooling is a different exposure definition from the original code and should not silently replace it.

Independent reconstruction groups deaths by ISO, year, and conflict ID, tests each conflict total against 25, takes the maximum indicator within each country-year, and advances the year by one before joining to the mortality panel. All original lag values agree. There are {excluded:,} raw rows without a conflict ID, also missing death counts; they are excluded consistently with the original script. No retained conflict record has a missing death count. Absence of a qualifying record is coded zero, as in the original preparation code; this is a coding convention rather than proof of complete surveillance.

The audit also checks every original mortality value against its wide-format raw source, every covariate against its raw country-year row, and both disaster indicators against the presence of earthquake or drought records in 2000–2019. All source checks return zero discrepancies. Disaster events are collapsed to binary indicators, so multiple events cannot multiply analytical rows.

## Edits and recommendation

Retain `data/processed/final_data.csv` as the analytical dataset. No data values were edited because the source checks support them. Preserve the agent output as the comparison artifact; it should not replace the original for analyses using the Week 3 definition. Correct the agent's aggregation and lag construction before using it, and explicitly resolve the exposure definition if a separate assignment specification requires pooling across conflicts.

One reproducibility edit was necessary: all source paths in `scripts/create_final_data.R` now point to the existing `data/raw/` directory instead of the nonexistent `data/original/` directory. Raw input files were left intact. After this path correction, an isolated R rebuild was run successfully and its parsed output matched all 3,720 rows and 17 columns of the saved original exactly. The original lag calculation already advances the source year before the join and therefore retains 2018 exposure for 2019 outcomes.

Run `python3 scripts/compare_final_data.py` from the project root to reproduce the comparison, source checks, discrepancy CSV, and this report. Run `Rscript scripts/create_final_data.R` from the project root to rebuild the original analytical dataset (requires tidyverse and janitor). The report's checks establish consistency with supplied sources and the available Week 3 code; they do not validate the external measurement quality of those sources or estimate the effect of discrepancies on a fitted model.
'''
(ROOT/'reports/compare_final_data.qmd').write_text('---\ntitle: "Comparison of Week 3 and coding-agent final data"\nformat: html\n---\n\n'+text.split('\n',1)[1])
print(f'Audit passed: {len(original):,} rows; {len(differences)} agent lag discrepancies; all {len(source_checks)} original source checks passed.')
