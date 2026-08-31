# `saved_results/` — artifact policy

This directory holds frozen Phase-1 artifacts: the Yosys reports, tool-version
record, and OpenROAD stage output that back the flow-instance records in
[`dataset/flow_instances.jsonl`](../dataset/flow_instances.jsonl).

Read this before adding to, trusting, or deleting anything here. Several files
are not what their directory name implies, and the reasons are recorded below
rather than left for the next person to rediscover.

## Contents

`phase1-20260817-214624/` is the **only** snapshot currently in the repository.
All 141 of its files are tracked: 6 at the top level (2.0 MB) and 135 under
three `orfs_*/base/` directories (252.3 MB).

## Keep: top-level Yosys evidence

These four files are the **only copies in git** of the evidence behind all 77
synthesis rows. Their working-tree twins (`generated/`, and `yosys_output.log`
at the repo root) are gitignored, so if these are removed the dataset loses its
supporting evidence entirely.

| File | Supports | Twin (gitignored) |
| --- | --- | --- |
| `cell_count_report.xml` | generic cell counts for all 77 rows | `generated/cell_count_report.xml` |
| `yosys_output.log` | the synthesis run that produced them | `yosys_output.log` (repo root) |
| `ppa_report.html` | the rendered Phase-1 PPA report | `generated/ppa_report.html` |
| `environment.txt` | host, git HEAD, submodules, tool versions | none — unique to this snapshot |

Two of them (`cell_count_report.xml`, `yosys_output.log`) were only rescued from
`.gitignore` on 2026-08-31 in `43bf9a4`; before that the dataset had no tracked
evidence at all.

## Historical copies: `flow_instances.jsonl` / `.json`

**`dataset/flow_instances.jsonl` is the canonical tracked dataset.** The copies
here are historical and should not be edited, parsed, or cited.

They are byte-identical to the canonical file today
(`md5 d00fcaa0052bbdb43fe24aa6843cdacc`), but nothing keeps them in step — the
one writer that updated them was changed in `04987bd` — so they will drift as
soon as the dataset grows.

### These are not a frozen 2026-08-17 archive

The directory name and `environment.txt` both say 2026-08-17, but the record
files were rewritten twice afterwards:

| Commit | Date | Rows | Implementation rows | md5 |
| --- | --- | ---: | ---: | --- |
| `c5f66c1` | 2026-08-18 | 71 | 0 | `07ec50d05c5d` |
| `83c5f18` | 2026-08-25 | 77 | 12 | `d00fcaa0052b` |

`flow_instances.jsonl` has an mtime of 2026-08-20, three days after the
timestamp it is filed under. The cause was a hardcoded
`--also-jsonl saved_results/phase1-20260817-214624/flow_instances.jsonl` in
`scripts/run_orfs_sweep.sh`, which merged every sweep result back into the
archive. That is fixed: the sweep now writes only to `dataset/`, and merging
into a second file requires opting in via `SWEEP_ALSO_JSONL`.

The genuine 2026-08-18 state is preserved in history and can be recovered at any
time:

```bash
git show c5f66c1:saved_results/phase1-20260817-214624/flow_instances.jsonl
```

## Archived ORFS directories: stage output, not reports

`orfs_hardfloat_FPADD_8_24/`, `orfs_openfloat_FP_32_1/`, and
`orfs_rial_RialAddFP32/` each hold 45 files of OpenROAD `results/` stage output:

```
.odb  .gds  .def  .spef  .v  .rtlil  .guide  .sdc  .tcl  .txt  .json
```

They contain **no `.rpt` and no `.log` files**. None of the numbers in the
dataset were parsed from anything in these directories — the metrics come from
`report_design_area`, `report_worst_slack`, and `report_tns` output that lives
elsewhere (see the next section).

Coverage is also partial. Three nicknames are archived; the dataset has twelve:

| Archived nickname | In dataset? |
| --- | --- |
| `hardfloat_FPADD_8_24` | yes — 2000 ps, `fail` |
| `rial_RialAddFP32` | yes — 2000 ps, `fail` |
| `openfloat_FP_32_1` | **no** — orphan, matches no row, and no longer exists under `openroad_results/` either |

So 252.3 MB of tracked binaries (37.2 MB of unique blobs, ~95% of this
repository's object store) supports 2 of 12 implementation rows and includes one
run that is not in the dataset at all.

## Known gap: implementation evidence is not tracked

All 12 implementation records cite `report_dir` paths under
`openroad_results/reports/asap7/<nickname>/base`. `openroad_results/` is
gitignored and has **zero tracked files**, so those paths resolve only on the
machine that ran the flow. `scripts/validate_dataset.py` reports this as a
standing warning.

On the original host the evidence does exist — 98 `.rpt` and 240 `.log` files —
and it is small. Tracking that set, rather than the 252 MB of layout binaries,
is what would actually close the evidence chain.

## Planned cleanup — separate commits, in this order

None of these are done yet. Each is deliberately scoped to one commit so a
mistake in one does not compromise the others.

1. **Remove or de-emphasize the duplicate record files here.** `dataset/` is now
   canonical and tracked, and the true 08-18 state is in history at `c5f66c1`,
   so nothing is lost.
2. **Decide what to do with the large ORFS binary stage outputs.** Note that
   `git rm` frees the working tree but reclaims *nothing* from clone size —
   the blobs stay in history. Actually shrinking the repository needs
   `git filter-repo`/BFG or an LFS migration, i.e. a history rewrite.
3. **Track the small OpenROAD reports and logs that support the 12
   implementation rows.** This closes the gap above and clears the validator
   warning.

## Adding a new snapshot

`scripts/archive_phase1.sh` writes to `results/phase1-<timestamp>/`, which is
gitignored. Promoting a run into `saved_results/` is a deliberate act: copy it
in, and check `git check-ignore -v` on each file first. Broad rules elsewhere in
`.gitignore` (`*.log`, `*.xml`, `*.json`, …) match these paths, and only the
negation block at the very bottom of `.gitignore` keeps them visible. Extensions
not listed there — including new ones — are silently ignored.

Treat a snapshot as immutable once committed. If a later run produces better
data, add a new snapshot directory; do not edit an existing one.
