#!/usr/bin/env python3
"""Generate std/opcodes.solc from a declarative list of EVM opcodes."""

import os
from string import ascii_lowercase

opcodes = [
    {"name": "stop",      "inputs": 0, "output": 0},
    {"name": "add",       "inputs": 2, "output": 1},
    {"name": "mstore",    "inputs": 2, "output": 0},
    {"name": "mload",     "inputs": 1, "output": 1},
    {"name": "mcopy",     "inputs": 3, "output": 0},
    {"name": "sstore",    "inputs": 2, "output": 0},
    {"name": "sload",     "inputs": 1, "output": 1},
    {"name": "keccak256", "inputs": 2, "output": 1},
    {"name": "callvalue", "inputs": 0, "output": 1},
    {"name": "revert",    "inputs": 2, "output": 0},
]


def arg_names(n):
    return [ascii_lowercase[i] for i in range(n)]


def gen_export(ops):
    lines = ["export {"]
    for i, op in enumerate(ops):
        sep = "," if i < len(ops) - 1 else ""
        lines.append(f"    {op['name']}{sep}")
    lines.append("};")
    return "\n".join(lines)


def gen_function(op):
    name = op["name"]
    args = arg_names(op["inputs"])
    params = ", ".join(f"{a}: word" for a in args)
    call_args = ", ".join(args)
    ret_type = "word" if op["output"] == 1 else "()"

    lines = [f"function {name}({params}) -> {ret_type} {{"]
    if op["output"] == 1:
        lines.append("    let res;")
        lines.append("    assembly {")
        lines.append(f"        res := {name}({call_args})")
        lines.append("    }")
        lines.append("    return res;")
    else:
        lines.append("    assembly {")
        lines.append(f"        {name}({call_args})")
        lines.append("    }")
    lines.append("}")
    return "\n".join(lines)


def render(ops):
    parts = [gen_export(ops)]
    for op in ops:
        parts.append("")
        parts.append(gen_function(op))
    return "\n".join(parts) + "\n"


def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    out_path = os.path.join(script_dir, os.pardir, "std", "opcodes.solc")
    with open(out_path, "w") as f:
        f.write(render(opcodes))


if __name__ == "__main__":
    main()
