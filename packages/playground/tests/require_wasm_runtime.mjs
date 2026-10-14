// A valid module with two memories checks the engine feature without allocating
// guest memory or compiling a Modelica model.
const twoMemoryModule = Uint8Array.of(
  0, 97, 115, 109, 1, 0, 0, 0,
  5, 5, 2, 0, 1, 0, 1,
);

if (!WebAssembly.validate(twoMemoryModule)) {
  console.error(
    `Rumoca compiled WASM simulation requires WebAssembly multi-memory support. ` +
      `Use Node.js 22 or newer; running ${process.version}.`,
  );
  process.exitCode = 1;
}
