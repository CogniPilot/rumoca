"""Exact RL guide source spans and passive authored CLI parser contexts."""
import argparse
import ast
import copy
import hashlib
import json
import os
from pathlib import Path
import shlex

ROOT = Path(__file__).resolve().parents[3]
FIXTURES = ROOT / 'crates/rumoca-bind-wasm/src/tests/fixtures/user_guide'


def digest(source):
    return hashlib.sha256(source.encode()).hexdigest()


def inventory():
    data = json.loads((FIXTURES / 'non-modelica-inventory.json').read_text())
    return data['entries']


def span(entry):
    page = ROOT / 'docs/user-guide/src' / entry['page']
    lines = page.read_text().splitlines()
    start = entry['line'] - 1
    count = entry['source_line_count']
    assert start >= 0 and count > 0 and start + count <= len(lines), entry
    source = '\n'.join(lines[start:start + count]) + '\n'
    assert digest(source) == entry['source_sha256'], entry
    return source


def included_source(entry):
    raw = span(entry).strip()
    assert raw.startswith('{{#include ') and raw.endswith('}}'), entry
    page = ROOT / 'docs/user-guide/src' / entry['page']
    target = (page.parent / raw[len('{{#include '):-2]).resolve()
    assert target.is_relative_to(ROOT / 'examples/reinforcement_learning'), target
    records = json.loads((FIXTURES / 'included-rl-sources.json').read_text())
    record = next(r for r in records if r['page'] == entry['page'] and r['line'] == entry['line'])
    assert record['language'] == entry['language']
    assert record['directive_sha256'] == digest(span(entry))
    assert target.relative_to(ROOT).as_posix() == record['path']
    source = target.read_text()
    assert digest(source) == record['source_sha256'], target
    compile(source, str(target), 'exec')
    return target, source


def python_commands(entries):
    commands = []
    for entry in entries:
        if entry['language'] != 'bash':
            continue
        selected = entry['profile'] == 'executable-rl-command-bash'
        setup = entry['page'] == 'simulation/canyon-run.md' and entry['profile'] == 'environment-setup-bash'
        if not (selected or setup):
            continue
        for line in span(entry).replace('\\\n', '').splitlines():
            argv = shlex.split(line, comments=True)
            if argv[:1] == ['python3'] and len(argv) > 1 and argv[1].startswith('examples/reinforcement_learning/'):
                commands.append(argv[1:])
    assert len(commands) == 4, commands
    return commands


def parser_statements(source):
    tree = ast.parse(source)
    main = next((node for node in tree.body if isinstance(node, ast.FunctionDef) and node.name == 'main'), None)
    body = main.body if main else next(node.body for node in tree.body
        if isinstance(node, ast.If) and ast.unparse(node.test) == "__name__ == '__main__'")
    result = []
    for statement in body:
        if isinstance(statement, ast.Import):
            continue
        statement = copy.deepcopy(statement)
        result.append(statement)
        if isinstance(statement, ast.Assign) and isinstance(statement.value, ast.Call):
            call = statement.value
            if isinstance(call.func, ast.Attribute) and call.func.attr == 'parse_args':
                assert ast.unparse(call.func.value) == 'parser'
                call.args = [ast.Name(id='_argv', ctx=ast.Load())]
                return ast.fix_missing_locations(ast.Module(body=result, type_ignores=[]))
        assert (isinstance(statement, ast.Assign) and ast.unparse(statement.targets[0]) == 'parser') or (
            isinstance(statement, ast.Expr) and isinstance(statement.value, ast.Call)
            and ast.unparse(statement.value.func) == 'parser.add_argument'), ast.dump(statement)
    raise AssertionError('authored parser prefix not found')


def parse_authored_arguments(path, argv):
    source = path.read_text()
    namespace = {'argparse': argparse, 'Path': Path, 'os': os, 'REPO': ROOT,
                 '__doc__': ast.get_docstring(ast.parse(source)), '_argv': argv}
    exec(compile(parser_statements(source), str(path) + ':parser-only', 'exec'), namespace)
    return namespace['args']


def authored_functions(source, names, namespace):
    tree = ast.parse(source)
    selected = [node for node in tree.body if isinstance(node, ast.FunctionDef) and node.name in names]
    assert {node.name for node in selected} == set(names)
    exec(compile(ast.Module(body=selected, type_ignores=[]), '<authored-guide-functions>', 'exec'), namespace)
    return namespace
