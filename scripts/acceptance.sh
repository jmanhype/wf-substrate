#!/bin/bash
# Convenience wrapper for acceptance tests
set -e

cd "$(dirname "$0")/.."
escript src/wf_acceptance.erl "$@"
