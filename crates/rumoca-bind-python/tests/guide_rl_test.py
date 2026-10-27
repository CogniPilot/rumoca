"""Required wheel-guide ownership for displayed RL code; no native training."""
import contextlib
import io
import json
import math
import os
import statistics
import numpy as np

from guide_rl_fixture import ROOT, inventory, span, included_source
from guide_rl_fixture import python_commands, parse_authored_arguments, authored_functions


def check_python_contexts(entries):
    selected = [entry for entry in entries if entry['profile'] == 'executable-rl-context-python']
    assert len(selected) == 4
    included = {}
    formula = None
    for entry in selected:
        if span(entry).startswith('{{#include '):
            path, source = included_source(entry)
            included[path.relative_to(ROOT).as_posix()] = source
        else:
            assert entry['page'] == 'simulation/cartpole.md'
            formula = span(entry)
    assert len(included) == 3 and formula is not None
    return included, formula


def check_cartpole(source, formula):
    policy = authored_functions(source, ['force_for'], {'math': math})['force_for']
    for weights, state in [([0, 0, 0, 0], [1, 2, 3, 4]),
                           ([.3, -.5, .2, -.7], [.1, -.2, .03, .4]),
                           ([-.3, .5, -.2, .7], [-.1, .2, -.03, -.4])]:
        context = {'math': math, 'weights': weights, 'state': state}
        exec(compile(formula, '<exact-cartpole-guide-fence>', 'exec'), context)
        assert math.isfinite(context['force'])
        assert context['force'] == policy(weights, state)
        assert abs(context['force']) <= 10


def check_ppo(source):
    context = {'__name__': 'guide_rl_context'}
    exec(compile(source, '<exact-included-ppo>', 'exec'), context)
    model = context['MLP'](2, 1, np.random.default_rng(1), hidden=(2,))
    model.params['W0'][:] = [[1, 0], [0, -1]]
    model.params['b0'][:] = 0
    model.params['W1'][:] = [[2], [3]]
    model.params['b1'][:] = .5
    x = np.array([[.1, .2], [-.4, .3]])
    expected = [[2 * math.tanh(a) + 3 * math.tanh(-b) + .5] for a, b in x]
    np.testing.assert_allclose(model.forward(x), expected, atol=1e-14, rtol=0)
    parameter = np.array([2., -3.])
    initial = np.sum(parameter ** 2)
    context['Adam']({'x': parameter}).step({'x': parameter.copy()})
    assert np.sum(parameter ** 2) < initial


def check_trainer(source):
    context = authored_functions(source, ['preprocess', 'episode_summary'], {'np': np, 'statistics': statistics})
    np.testing.assert_array_equal(context['preprocess']([-10., 1., 10.]), [-5, 1, 5])
    summary = context['episode_summary']([
        {'cause': 'finish', 'distance': 600, 'return': 20},
        {'cause': 'floor', 'distance': 100, 'return': -2}])
    assert summary['finishes'] == 1 and summary['max_distance'] == 600
    assert summary['last50_mean_distance'] == 350 and summary['last50_mean_return'] == 9


def check_command_parsers(entries):
    results = []
    for relative, *argv in python_commands(entries):
        path = ROOT / relative
        if 'canyon_run/' in relative and not hasattr(os, 'sched_getaffinity'):
            results.append({'program': relative, 'status': 'NOT_RUN_PLATFORM',
                            'reason': 'Documented native Canyon companion/parser defaults are Linux-owned'})
            continue
        args = parse_authored_arguments(path, argv)
        assert args.rumoca == 'rumoca'
        with contextlib.redirect_stderr(io.StringIO()):
            try:
                parse_authored_arguments(path, [*argv, '--not-a-guide-option'])
            except SystemExit as error:
                assert error.code == 2
            else:
                raise AssertionError('unknown authored CLI option accepted')
        results.append({'program': relative, 'status': 'PARSER_PASS_NO_LAUNCH'})
    return results


def test_rl_guide_fences():
    entries = inventory()
    included, formula = check_python_contexts(entries)
    prefix = 'examples/reinforcement_learning/'
    check_cartpole(included[prefix + 'cartpole/train.py'], formula)
    check_ppo(included[prefix + 'canyon_run/ppo.py'])
    check_trainer(included[prefix + 'canyon_run/train.py'])
    print(json.dumps({'guide_rl': 'source/function contexts PASS',
                      'commands': check_command_parsers(entries),
                      'native_training': 'NOT_RUN', 'server_launch': 'NOT_RUN'}))


if __name__ == '__main__':
    test_rl_guide_fences()
