# multsig — basic Multisig test suite

A very basic dispatch test scaffold for `test/examples/dispatch/multisig.sol`
(symlinked here as `multisig.solc` so `contest.sh`'s base-name convention works).

The suite walks through the core state machine on both the **on-chain**
(direct signer call) and **off-chain** (signature-relayed) surfaces:

1. `constructor()` — deploying account becomes signer 0; `signers_count = 1`,
   `signers_required = 1`.
2. **On-chain add signer** — `queue(AddSigner(0x…dead))` → `approve(0)` →
   `execute(0, "")`. After this the new address is signer 1.
3. **On-chain reject** of an `AddSigner` — `queue(AddSigner(0x…cafe))` →
   `reject(1)` → `execute(1, "")` (rejected ops are no-op'd and skipped, so
   `nonce` still advances).
4. **Off-chain add signer** — `queueWithSignature(AddSigner(0x…beef), …)`
   → `approveWithSignature(2, …)`.
5. **Off-chain reject** of an `AddSigner` — `queueWithSignature(AddSigner(0x…f00d), …)`
   → `rejectWithSignature(3, …)`.

Each step also has at least one adjacent failure case (double-approve,
approve-after-reject, reject-after-approve) so the OperationStatus state
machine actually gets exercised, not just the happy path.

## Running

```bash
bash contest.sh multsig/multisig.json
```

…and the suite shows up alongside the others if you append it to
`run_contests.sh`.

## ⚠️ Known compiler gap

The contract takes sum-type arguments (`Operation`, `Signature`,
`BatchOperation`). solcore's dispatch generator (`std/dispatch.solc`) only
ships `SigString` / `ABIEncode` / `ABIDecode` instances for `uint256`,
`address`, `bytes32`, `bool`, `string`, `bytes`, `()` and 2-tuples — there
is no instance for arbitrary sum types yet, so the methods that take
`Operation` / `Signature` (`queue`, `queueWithSignature`,
`approveWithSignature`, `rejectWithSignature`, `batch`) currently can't be
routed through the generated entrypoint at all.

For the steps that take a sum-type argument the JSON therefore uses a
**placeholder calldata** (just the 4-byte selector, plus the static head
of any `uint256` arg). These cases are tagged in their `comment` field;
once the dispatcher learns to encode ADTs, fill in the real tail bytes
(tag word + variant payload) and the cases become real.

The pure-`uint256` / `(uint256, bytes)` methods (`approve`, `reject`,
`execute`) use the canonical Solidity encoding and their selectors —

| function                 | selector   |
| ------------------------ | ---------- |
| `approve(uint256)`       | `b759f954` |
| `reject(uint256)`        | `b8adaa11` |
| `execute(uint256,bytes)` | `59efcb15` |
| `queue(Operation)`       | `4176b43a` |
| `queueWithSignature(Operation,Signature)`     | `9b9bf4ec` |
| `approveWithSignature(uint256,Signature)`     | `0e8d95f3` |
| `rejectWithSignature(uint256,Signature)`      | `175f6f47` |

— so those cases should work end-to-end once the contract itself compiles.

## Caller convention

The C++ testrunner always sends from
`0x1212121212121212121212121212120000000012` (see the existing
`ownable.json` returndata for the canonical padded form). The constructor
records that address as signer 0, so every on-chain `approve` / `reject`
in this suite implicitly comes from it.
