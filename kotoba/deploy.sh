#!/usr/bin/env bash
# hydrogen_electrolysis — kotoba deploy
#
# Dry run:
#   ./deploy.sh
#
# Live node check + operator-gated ingest:
#   KOTOBA_URL=http://127.0.0.1:8077 KOTOBA_TOKEN=<at-session-jwt> ./deploy.sh
set -euo pipefail

KOTOBA_URL="${KOTOBA_URL:-http://127.0.0.1:8077}"
GRAPH="${HYDROGEN_ELECTROLYSIS_GRAPH:-com.etzhayyim.hydrogen-electrolysis}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "==> hydrogen_electrolysis kotoba deploy -> ${KOTOBA_URL} (graph ${GRAPH})"

if ! curl -fsS -m 5 "${KOTOBA_URL}/health" >/dev/null 2>&1; then
  echo "!! kotoba node not reachable at ${KOTOBA_URL} - start it with: kotoba serve" >&2
  exit 1
fi

bb "${SCRIPT_DIR}/ingest_efficiency.cljc" --url "${KOTOBA_URL}" --graph "${GRAPH}"
echo "==> done"
