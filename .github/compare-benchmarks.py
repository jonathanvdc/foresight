#!/usr/bin/env python3
"""Compare two JMH JSON results and emit a Markdown table.

Usage:
  python compare-benchmarks.py BASELINE.json CURRENT.json [--title "..."] [--note "..."]

Behavior:
  • Reads the two JSON files produced by JMH (arrays of benchmark entries).
  • Matches rows by (benchmark name, sorted params).
  • Emits a Markdown table with Baseline, PR, Δ (PR/Base), and Unit.
  • If $GITHUB_OUTPUT is set, appends a multi-line output block suitable for
    GitHub Actions; otherwise, prints the Markdown to stdout.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from typing import Dict, Iterable, List, Tuple, Any

Key = Tuple[str, Tuple[Tuple[str, Any], ...]]  # (benchmark, sorted(params.items()))
Entry = Dict[str, Any]

EM_DASH = "—"


def load(path: str) -> Dict[Key, Entry]:
    """Load a JMH JSON result file into a dict keyed by (benchmark, params)."""
    try:
        with open(path, "r", encoding="utf-8") as f:
            data = json.load(f)
    except FileNotFoundError:
        return {}
    except json.JSONDecodeError as e:
        print(f"error: failed to parse JSON from {path}: {e}", file=sys.stderr)
        sys.exit(2)

    result: Dict[Key, Entry] = {}
    if not isinstance(data, list):
        print(f"warning: {path} did not contain a list; ignoring.", file=sys.stderr)
        return result

    for e in data:
        if not isinstance(e, dict):
            continue
        bench = e.get("benchmark", "")
        params = e.get("params") or {}
        if not isinstance(params, dict):
            params = {}
        key: Key = (bench, tuple(sorted(params.items())))
        result[key] = e
    return result


essential_num_types = (int, float)


def _score_and_unit(e: Entry | None) -> Tuple[str, str]:
    if not e:
        return EM_DASH, ""
    m = e.get("primaryMetric", {}) if isinstance(e, dict) else {}
    score = m.get("score")
    unit = m.get("scoreUnit", "")
    if isinstance(score, essential_num_types):
        return f"{float(score):.6g}", str(unit)
    return str(score if score is not None else EM_DASH), str(unit)


def _numeric_score(e: Entry | None) -> float | None:
    if not e or not isinstance(e, dict):
        return None
    m = e.get("primaryMetric", {})
    score = m.get("score")
    return float(score) if isinstance(score, essential_num_types) else None


def compare_to_markdown(base: Dict[Key, Entry], cur: Dict[Key, Entry], *, title: str, note: str | None) -> str:
    keys = sorted(set(base.keys()) | set(cur.keys()))

    lines: List[str] = []
    lines.append(f"### {title}")
    lines.append("")
    lines.append("| Benchmark | Params | Baseline | PR | Δ (PR/Base) | Unit |")
    lines.append("|---|---|---:|---:|---:|---|")

    for bench, params in keys:
        b = base.get((bench, params))
        c = cur.get((bench, params))

        bscore_s, bunit = _score_and_unit(b)
        cscore_s, cunit = _score_and_unit(c)

        bscore = _numeric_score(b)
        cscore = _numeric_score(c)

        if bscore is not None and cscore is not None:
            if bscore == 0:
                delta_s = "n/a"
            else:
                delta = cscore / bscore
                delta_s = f"{delta:.3f}× ({(delta - 1.0) * 100:+.1f}%)"
            unit = c.get("primaryMetric", {}).get("scoreUnit", bunit or cunit) if isinstance(c, dict) else (bunit or cunit)
        else:
            delta_s = "n/a"
            unit = cunit or bunit or ""

        pstr = ", ".join(f"{k}={v}" for k, v in params) if params else EM_DASH
        lines.append(f"| `{bench}` | {pstr} | {bscore_s} | {cscore_s} | {delta_s} | {unit} |")

    lines.append("")
    if note:
        lines.append(note)
        lines.append("")

    return "\n".join(lines)


def parse_args(argv: List[str]) -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Compare two JMH JSON files and emit Markdown.")
    p.add_argument("baseline", help="Path to baseline JMH JSON (e.g., from main)")
    p.add_argument("current", help="Path to current JMH JSON (e.g., from PR)")
    p.add_argument("--title", default="JMH comparison (baseline vs PR)", help="Header title to use above the table")
    p.add_argument(
        "--note",
        default="_Note: < 1.0× means faster on PR; > 1.0× means slower._",
        help="Optional note to append under the table (Markdown)",
    )
    return p.parse_args(argv)


def main(argv: List[str]) -> int:
    args = parse_args(argv)

    base = load(args.baseline)
    cur = load(args.current)

    if not base and not cur:
        print("error: neither baseline nor current results could be loaded.", file=sys.stderr)
        return 1

    md = compare_to_markdown(base, cur, title=args.title, note=args.note)

    print(md)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
