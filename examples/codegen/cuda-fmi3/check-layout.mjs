// Run with node --test; CLANG may point to the device compiler used by build.mjs.
import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {test} from 'node:test';
import {checkNumericInstance} from './layout.mjs';

function inspect(member, relocated = false) {
  const source = `
    typedef void *fmi3InstanceEnvironment;
    typedef void (*fmi3LogMessageCallback)(void);
    typedef double *HostPointer;
    typedef struct {
      fmi3InstanceEnvironment environment; fmi3LogMessageCallback logger;
      ${member}
    } ModelInstance;
  `;
  const flags = relocated ? ['-DRMC_FMI3_INLINE_FACTORS'] : [];
  const ast = execFileSync(process.env.CLANG ?? 'clang',
    [...flags, '-x', 'c', '-std=c11', '-Xclang', '-ast-dump=json', '-fsyntax-only', '-'],
    {input: source, encoding: 'utf8'});
  checkNumericInstance(JSON.parse(ast));
}

test('the selected device storage is inline', () => {
  const fields = `
    #ifdef RMC_FMI3_INLINE_FACTORS
      double factors[2];
    #else
      double *factors;
    #endif
  `;
  assert.throws(() => inspect(fields), /ModelInstance.factors/);
  inspect(fields, true);
});
test('nested numeric fields and callback typedefs are supported', () => {
  inspect('struct Counts { unsigned long epochs[4]; } counters;');
});
test('aliased pointers and nested pointer arrays prevent transfer', () => {
  assert.throws(() => inspect('HostPointer addresses[4];'), /ModelInstance.addresses/);
  assert.throws(() => inspect('struct Values { HostPointer live; } values;'), /ModelInstance.values.live/);
  assert.throws(() => inspect('fmi3LogMessageCallback leaked;'), /ModelInstance.leaked/);
});
