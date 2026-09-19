import {
    availableOutputNames,
    availableStateNames,
    buildVisualizationModel,
    expandRequestedSeries,
    toggleSeriesName,
} from "../../rumoca-web/viz/visualization_shared.js";

function assert(condition, message) {
    if (!condition) {
        throw new Error(message);
    }
}

function assertDeepEqual(actual, expected, message) {
    const actualJson = JSON.stringify(actual);
    const expectedJson = JSON.stringify(expected);
    if (actualJson !== expectedJson) {
        throw new Error(`${message || "mismatch"}: expected ${expectedJson}, got ${actualJson}`);
    }
}

// A result with two states (x, v) and two outputs (energy, power).
function sampleResult() {
    return {
        names: ["x", "v", "energy", "power"],
        nStates: 2,
        allData: [
            [0, 1, 2],
            [0.0, 0.1, 0.2],
            [1.0, 0.9, 0.8],
            [10.0, 11.0, 12.0],
            [100.0, 90.0, 80.0],
        ],
    };
}

function statesAndOutputsPartitionByStateCount() {
    const result = sampleResult();
    assertDeepEqual(availableStateNames(result), ["x", "v"], "states are names[0..nStates]");
    assertDeepEqual(availableOutputNames(result), ["energy", "power"], "outputs are names[nStates..]");
}

function statesWildcardStaysIdentical() {
    const result = sampleResult();
    assertDeepEqual(
        expandRequestedSeries(result, ["*states"]),
        ["x", "v"],
        "*states expands to states only",
    );
}

function outputsWildcardExpandsToOutputs() {
    const result = sampleResult();
    assertDeepEqual(
        expandRequestedSeries(result, ["*outputs"]),
        ["energy", "power"],
        "*outputs expands to output channels",
    );
}

function allWildcardExpandsToEveryChannel() {
    const result = sampleResult();
    assertDeepEqual(
        expandRequestedSeries(result, ["*all"]),
        ["x", "v", "energy", "power"],
        "*all expands to every channel",
    );
}

function mixedWildcardsAndNamesDeduplicate() {
    const result = sampleResult();
    assertDeepEqual(
        expandRequestedSeries(result, ["*states", "*outputs", "power", "unknown"]),
        ["x", "v", "energy", "power"],
        "mixed wildcards plus explicit names dedupe and drop unknowns",
    );
}

function buildModelResolvesSelectedOutputs() {
    const result = sampleResult();
    const model = buildVisualizationModel(result, {
        id: "mixed",
        title: "Mixed",
        type: "timeseries",
        x: "time",
        y: ["*states", "*outputs"],
    });
    assertDeepEqual(
        model.y.map((series) => series.name),
        ["x", "v", "energy", "power"],
        "timeseries model resolves states and outputs together",
    );
    // The output "power" data column must be carried through, proving outputs
    // are selectable and not dropped anywhere in the plot-data path.
    const power = model.y.find((series) => series.name === "power");
    assert(power, "expected power series to be present");
    assertDeepEqual(power.values, [100, 90, 80], "power output values plot from the payload");
}

function pickerToggleAddsAndRemovesExactNames() {
    // Start with a wildcard entry that the picker leaves untouched.
    let yText = "*states";
    yText = toggleSeriesName(yText, "energy", true);
    assertDeepEqual(
        yText.split(", "),
        ["*states", "energy"],
        "toggling on adds the exact channel name alongside a wildcard",
    );
    yText = toggleSeriesName(yText, "power", true);
    assertDeepEqual(
        yText.split(", "),
        ["*states", "energy", "power"],
        "toggling a second channel appends it",
    );
    yText = toggleSeriesName(yText, "energy", false);
    assertDeepEqual(
        yText.split(", "),
        ["*states", "power"],
        "toggling off removes only the exact channel name",
    );
}

function pickerToggleIsIdempotentForDuplicates() {
    const yText = toggleSeriesName("x, x, v", "x", true);
    assertDeepEqual(
        yText.split(", "),
        ["v", "x"],
        "toggling on collapses duplicates to a single entry",
    );
}

function main() {
    statesAndOutputsPartitionByStateCount();
    statesWildcardStaysIdentical();
    outputsWildcardExpandsToOutputs();
    allWildcardExpandsToEveryChannel();
    mixedWildcardsAndNamesDeduplicate();
    buildModelResolvesSelectedOutputs();
    pickerToggleAddsAndRemovesExactNames();
    pickerToggleIsIdempotentForDuplicates();
}

main();
