#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$root_dir"

bash ./contest.sh test/examples/dispatch/basic.json
bash ./contest.sh test/examples/dispatch/neg.json
bash ./contest.sh test/examples/dispatch/miniERC20.json
bash ./contest.sh test/examples/dispatch/Revert.json
bash ./contest.sh test/examples/dispatch/ownable.json
bash ./contest.sh test/examples/dispatch/hashes.json
bash ./contest.sh test/examples/dispatch/payable.json
bash ./contest.sh test/examples/dispatch/payable_ctor.json
bash ./contest.sh test/examples/dispatch/nonpayable_ctor.json
bash ./contest.sh test/examples/dispatch/concat.json
bash ./contest.sh test/examples/dispatch/slices.json
bash ./contest.sh test/examples/dispatch/fallback.json
bash ./contest.sh test/examples/dispatch/ecrecover.json
bash ./contest.sh test/examples/dispatch/memory.json
# bytecode.json exercises extcodecopy. The selfCodeCopyWorks() oracle passes
# deterministically, but the literal copySelfCode() test ships with an empty
# placeholder returndata (this contract's runtime code is not known ahead of
# time). Run it once, paste the hex the testrunner prints into bytecode.json,
# then enable this line (set -e + testrunner EXIT_FAILURE would abort the suite
# while the golden is unfilled):
# bash ./contest.sh test/examples/dispatch/bytecode.json
