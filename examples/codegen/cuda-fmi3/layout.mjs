// Inspect field types after the exact device compiler flags and includes are applied.
export function checkNumericInstance(ast) {
  const declarations = ast.inner;
  const instance = declarations.find(node => node.kind === 'TypedefDecl' && node.name === 'ModelInstance');
  const owner = instance?.inner?.find(type => type.ownedTagDecl)?.ownedTagDecl.id;
  const record = declarations.find(node => node.kind === 'RecordDecl' && node.id === owner);
  if (!record?.completeDefinition) throw Error('CUDA export requires a complete ModelInstance.');

  const numeric = /^(?:_Bool|bool|double|float|(?:unsigned |signed )?(?:char|short|int|long(?: long)?))$/;
  const aliases = new Map(declarations.filter(node => node.kind === 'TypedefDecl')
    .map(node => [node.name, node.type.desugaredQualType ?? node.type.qualType]));
  function checkFields(struct, path) {
    for (const field of struct.inner.filter(node => node.kind === 'FieldDecl')) {
      const name = path + '.' + field.name;
      const type = field.type.qualType;
      // The generated entry checks both host callbacks for null before stepping.
      if ((name === 'ModelInstance.environment' && type === 'fmi3InstanceEnvironment') ||
          (name === 'ModelInstance.logger' && type === 'fmi3LogMessageCallback')) continue;
      const base = (field.type.desugaredQualType ?? type).replace(/(?:\[\d+\])+$/, '');
      if (numeric.test(aliases.get(base) ?? base) || /^enum \w+$/.test(base)) continue;
      const nested = struct.inner.find(node => node.kind === 'RecordDecl' &&
        node.completeDefinition && base === 'struct ' + node.name);
      if (nested) checkFields(nested, name);
      else throw Error('CUDA export requires inline numeric state; unsupported ' + name + ' (' + type + ').');
    }
  }
  checkFields(record, 'ModelInstance');
}
