"""Installed-wheel FMI source rendering through the public Model.codegen API.

These checks require the wheel's FMI feature. They do not build a native FMU
or a WebAssembly component; packaged-target runtime suites own those boundaries.
"""

from pathlib import Path, PurePosixPath
import re
import tempfile
import uuid
import xml.etree.ElementTree as ET

import rumoca as rm

FIXTURE_ROOT = Path(__file__).resolve().parent / "fixtures"
TRANSLATION_UNITS = {
    "model.c", "rmc_rows.c", "rmc_isolators.c", "rmc_functions.c",
    "rmc_assign.c", "rmc_jacobian.c",
}


def _render(target: str, expected: set[str]) -> dict[str, str]:
    advertised = next(item for item in rm.targets() if item.id == target)
    assert advertised.ir == "fmi"
    model = rm.Session(roots=[str(FIXTURE_ROOT / "Lib")]).load(
        FIXTURE_ROOT / "UsesLib.mo"
    )
    result = model.codegen(target)
    assert isinstance(result, rm.CodegenResult)
    assert result.target == target
    pairs = list(result)
    files = dict(pairs)
    assert len(files) == len(pairs) == len(result), "paths must be unique"
    assert result.paths == [path for path, _ in pairs]
    assert set(files) == expected, (
        f"{target}: rendered source set {sorted(files)} does not match {sorted(expected)}"
    )
    assert [(file.path, file.content) for file in result.files] == pairs
    for path, content in pairs:
        relative = PurePosixPath(path)
        assert not relative.is_absolute() and ".." not in relative.parts
        assert isinstance(content, str) and content.strip(), path
    with tempfile.TemporaryDirectory(prefix="rumoca-python-fmi-") as temporary:
        root = Path(temporary)
        written = result.save_all(root)
        assert {Path(path) for path in written} == {root / path for path in files}
        for path, content in pairs:
            assert (root / path).read_bytes() == content.encode("utf-8"), path
    return files


def _description(files: dict[str, str], version: str) -> ET.Element:
    root = ET.fromstring(files["modelDescription.xml"])
    assert root.tag == "fmiModelDescription"
    assert root.attrib["fmiVersion"] == version
    assert root.attrib["modelName"] == "UsesLib"
    identity = root.attrib["guid" if version == "2.0" else "instantiationToken"]
    issued = str(uuid.UUID(identity.strip("{}")))
    identifier = "Rumoca_" + issued.replace("-", "_")
    for interface in ["ModelExchange", "CoSimulation"]:
        node = root.find(interface)
        assert node is not None and node.attrib["modelIdentifier"] == identifier
    assert identifier + "_" in files["sources/model.h"]
    variables = root.find("ModelVariables")
    assert variables is not None
    assert {node.attrib["name"] for node in variables} == {
        "time", "gain", "x", "der(x)"
    }
    references = [node.attrib["valueReference"] for node in variables]
    assert len(set(references)) == len(references)
    return root


def test_codegen_fmi2_sources_and_metadata() -> None:
    expected = {"modelDescription.xml", "sources/model.h"}
    expected.update("sources/" + name for name in TRANSLATION_UNITS)
    files = _render("fmi2", expected)
    root = _description(files, "2.0")
    variables = list(root.find("ModelVariables"))
    by_name = {node.attrib["name"]: node for node in variables}
    assert float(by_name["gain"].find("Real").attrib["start"]) == 2.0
    assert float(by_name["x"].find("Real").attrib["start"]) == 1.0
    derivative = by_name["der(x)"].find("Real")
    state_index = variables.index(by_name["x"]) + 1
    assert int(derivative.attrib["derivative"]) == state_index
    for interface in ["ModelExchange", "CoSimulation"]:
        sources = root.findall(interface + "/SourceFiles/File")
        assert {node.attrib["name"] for node in sources} == TRANSLATION_UNITS
    assert "fmi2Instantiate(" in files["sources/model.c"]


def test_codegen_fmi3_sources_and_metadata() -> None:
    expected = {
        "modelDescription.xml", "sources/buildDescription.xml", "sources/model.h"
    }
    expected.update("sources/" + name for name in TRANSLATION_UNITS)
    files = _render("fmi3", expected)
    root = _description(files, "3.0")
    by_name = {node.attrib["name"]: node for node in root.find("ModelVariables")}
    assert by_name["x"].tag == by_name["gain"].tag == "Float64"
    assert float(by_name["gain"].attrib["start"]) == 2.0
    assert float(by_name["x"].attrib["start"]) == 1.0
    derivative_reference = by_name["der(x)"].attrib["valueReference"]
    state_reference = by_name["x"].attrib["valueReference"]
    assert by_name["der(x)"].attrib["derivative"] == state_reference
    derivative = root.find("ModelStructure/ContinuousStateDerivative")
    assert derivative is not None
    assert derivative.attrib["valueReference"] == derivative_reference
    build = ET.fromstring(files["sources/buildDescription.xml"])
    assert build.tag == "fmiBuildDescription" and build.attrib["fmiVersion"] == "3.0"
    configuration = build.find("BuildConfiguration")
    identifier = root.find("ModelExchange").attrib["modelIdentifier"]
    assert configuration.attrib["modelIdentifier"] == identifier
    sources = configuration.findall("SourceFileSet/SourceFile")
    assert {node.attrib["name"] for node in sources} == TRANSLATION_UNITS
    assert "fmi3InstantiateCoSimulation(" in files["sources/model.c"]


def test_codegen_fmi_ls_wasm_rendered_component_sources() -> None:
    expected = {
        "Cargo.toml",
        "build.rs",
        "src/lib.rs",
        "UPSTREAM.md",
        "csrc/model.h",
        "modelDescription.xml",
        "extra/org.modelica.fmi-ls-wasm/manifest.xml",
    }
    expected.update("csrc/" + name for name in TRANSLATION_UNITS)
    files = _render("fmi-ls-wasm", expected)
    assert 'crate-type = ["cdylib"]' in files["Cargo.toml"]
    assert 'world: "co-simulation-fmu"' in files["src/lib.rs"]
    assert "c1aac17d392bec989fe2d059db3cc57bb7a0fff5" in files["UPSTREAM.md"]
    sources = re.findall(r'"(csrc/[^\"]+\.c)"', files["build.rs"])
    assert {path.removeprefix("csrc/") for path in sources} == TRANSLATION_UNITS
    prefix = re.search(r"#define FMI3_FUNCTION_PREFIX (\w+)", files["csrc/model.h"])
    assert prefix is not None
    assert prefix.group(1) + "fmi3InstantiateCoSimulation" in files["src/lib.rs"]
    assert "fmi3InstantiateCoSimulation(" in files["csrc/model.c"]


def main() -> None:
    test_codegen_fmi2_sources_and_metadata()
    test_codegen_fmi3_sources_and_metadata()
    test_codegen_fmi_ls_wasm_rendered_component_sources()
    print("fmi_codegen_test: OK (3 required FMI source targets)")


if __name__ == "__main__":
    main()
