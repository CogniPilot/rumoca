//! Checked public Integer publication validates the whole request against the
//! variant's Integer profile (FMI 2 Int32, FMI 3 Int64) before casting any value,
//! even when the internal MLS §3.7.2 quotient is an exact, wider signed Integer.

use super::*;

const ORIGINAL: &str = include_str!(
    "../../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_411.mo"
);
const BOUNDS: &str = r#"model IntegerPrecision
  input Integer n(start=0);
  output Integer y;
equation
  y=if n < 0 then -2147483648+n+1 else 2147483647+n;
end IntegerPrecision;
"#;

#[test]
fn packaged_fmi_integer_publication_checks_the_whole_request_before_casting() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("Integer publication work directory");
    let division = ORIGINAL.replace("mod(", "div(");
    let cases = [
        (
            "mod",
            ORIGINAL,
            "{-7,5},{-2,3},{-1,4},{0,5},{1,6},{2,0},{7,5}",
        ),
        ("div", division.as_str(), "{2,1286742750677285.0}"),
        (
            "bounds",
            BOUNDS,
            "{-2,-2147483649.0},{-1,-2147483648.0},{0,2147483647.0},{1,2147483648.0}",
        ),
    ];
    for (label, source, cases) in cases {
        let compiled = rumoca::Compiler::new()
            .model("IntegerPrecision")
            .compile_str(source, "IntegerPrecision.mo")
            .expect("compile reported Integer publication source");
        let directory = work.path().join(label);
        for (version, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
            let fmu = build_named_fmu(&directory, &compiled, version, "IntegerPrecision");
            validate_source_package(&fmu, standard);
            run_harness(&fmu, standard, cases);
        }
    }
}

fn run_harness(fmu: &BuiltFmu, standard: &FmiStandard, cases: &str) {
    let xml = fs::read_to_string(fmu.root.join("modelDescription.xml")).unwrap();
    let (adapter, key) = match fmu.version {
        "fmi2" => (FMI2, "guid"),
        "fmi3" => (FMI3, "instantiationToken"),
        other => panic!("unexpected FMI version {other}"),
    };
    let driver = HARNESS
        .replace("@ADAPTER@", adapter)
        .replace("@TOKEN@", lifecycle::attribute(&xml, key))
        .replace("@CASES@", cases);
    let source = fmu.root.join("integer_publication.c");
    let binary = fmu.root.join("integer_publication");
    fs::write(&source, driver).unwrap();
    checked_output(
        Command::new("cc")
            .args([
                "-std=c11",
                "-ffp-contract=off",
                "-Wall",
                "-Wextra",
                "-Werror",
            ])
            .arg(format!("-I{}", fmu.root.join("sources").display()))
            .arg(format!("-I{}", standard.root.join("headers").display()))
            .arg(&source)
            .args(
                lifecycle::source_units(&fmu.root)
                    .into_iter()
                    .filter(|path| path.file_name().is_some_and(|name| name != "model.c")),
            )
            .args(["-lm", "-o"])
            .arg(&binary),
        "compile packaged Integer publication harness",
    );
    checked_output(
        &mut Command::new(binary),
        "execute Integer publication harness",
    );
}

const FMI2: &str = r#"
typedef fmi2Component Instance;
typedef fmi2Integer Integer;
#define PUBLIC_BITS 32
typedef fmi2ValueReference Reference;
#define SUCCESS fmi2OK
#define FAILURE fmi2Error
static Instance start(int cs, Integer input) {
    fmi2CallbackFunctions callbacks={0}; callbacks.allocateMemory=calloc; callbacks.freeMemory=free;
    Instance m=fmi2Instantiate("integer",cs?fmi2CoSimulation:fmi2ModelExchange,"@TOKEN@",NULL,&callbacks,0,0);
    const Reference vr=1;
    if (!m || fmi2SetupExperiment(m,0,0,0,1,0.01)!=SUCCESS || fmi2EnterInitializationMode(m)!=SUCCESS
        || fmi2SetInteger(m,&vr,1,&input)!=SUCCESS || fmi2ExitInitializationMode(m)!=SUCCESS) return NULL;
    if (!cs) {
        fmi2EventInfo info;
        if (fmi2NewDiscreteStates(m,&info)!=SUCCESS || info.newDiscreteStatesNeeded || info.terminateSimulation
            || fmi2EnterContinuousTimeMode(m)!=SUCCESS) return NULL;
    }
    return m;
}
static int get(Instance m, const Reference* vr, Integer* out) { return fmi2GetInteger(m,vr,2,out); }
static int finish(Instance m) { int status=fmi2Terminate(m); fmi2FreeInstance(m); return status; }
static double settled(Instance m) { return ((ModelInstance*)m)->p[rmc_value_references[2].index]; }
"#;

const FMI3: &str = r#"
typedef fmi3Instance Instance;
typedef fmi3Int64 Integer;
#define PUBLIC_BITS 64
typedef fmi3ValueReference Reference;
#define SUCCESS fmi3OK
#define FAILURE fmi3Error
static Instance start(int cs, Integer input) {
    Instance m=cs?fmi3InstantiateCoSimulation("integer","@TOKEN@",NULL,0,0,0,0,NULL,0,NULL,NULL,NULL)
        :fmi3InstantiateModelExchange("integer","@TOKEN@",NULL,0,0,NULL,NULL);
    const Reference vr=1;
    if (!m || fmi3EnterInitializationMode(m,0,0,0,1,0.01)!=SUCCESS
        || fmi3SetInt64(m,&vr,1,&input,1)!=SUCCESS || fmi3ExitInitializationMode(m)!=SUCCESS) return NULL;
    if (!cs) {
        fmi3Boolean need,terminate,nominals,values,next; fmi3Float64 next_time;
        if (fmi3UpdateDiscreteStates(m,&need,&terminate,&nominals,&values,&next,&next_time)!=SUCCESS
            || need || terminate || fmi3EnterContinuousTimeMode(m)!=SUCCESS) return NULL;
    }
    return m;
}
static int get(Instance m, const Reference* vr, Integer* out) { return fmi3GetInt64(m,vr,2,out,2); }
static int finish(Instance m) { int status=fmi3Terminate(m); fmi3FreeInstance(m); return status; }
static double settled(Instance m) { return ((ModelInstance*)m)->p[rmc_value_references[2].base]; }
"#;

const HARNESS: &str = r#"#include "model.c"
#include <stdio.h>
#define CHECK(test) do { if (!(test)) { fprintf(stderr,"failed line %d: %s\n",__LINE__,#test); return 1; } } while(0)
@ADAPTER@
static int integer_domains(void) {
    const double valid32[]={-2147483648.0,2147483647.0,0.0};
    const double invalid32[]={-2147483649.0,2147483648.0,NAN,INFINITY,-INFINITY,0.5};
    for (size_t i=0;i<sizeof valid32/sizeof valid32[0];++i) CHECK(rmc_integer_public_value(valid32[i],32,0));
    for (size_t i=0;i<sizeof invalid32/sizeof invalid32[0];++i) CHECK(!rmc_integer_public_value(invalid32[i],32,0));
    CHECK(rmc_integer_public_value(-0x1p63,64,0));
    CHECK(rmc_integer_public_value(nextafter(0x1p63,0.0),64,0));
    CHECK(!rmc_integer_public_value(0x1p63,64,0));
    CHECK(!rmc_integer_public_value(nextafter(-0x1p63,-INFINITY),64,0));
    CHECK(rmc_integer_public_value(1,64,3) && rmc_integer_public_value(3,64,3));
    CHECK(!rmc_integer_public_value(0,64,3) && !rmc_integer_public_value(4,64,3));
    return 0;
}
int main(void) {
    CHECK(integer_domains()==0);
    const struct { Integer input; double raw; } cases[]={@CASES@};
    const Reference vr[2]={1,2}, wrong[2]={1,0};
    for (int cs=0;cs<2;++cs) for (size_t k=0;k<sizeof cases/sizeof cases[0];++k) {
        Instance m=start(cs,cases[k].input); CHECK(m);
        Integer values[2]={12345,6789};
        int status=get(m,vr,values);
        CHECK(settled(m)==cases[k].raw);
        const int error=!rmc_integer_public_value(cases[k].raw,PUBLIC_BITS,0);
        if (error) CHECK(status==FAILURE && values[0]==12345 && values[1]==6789);
        else CHECK(status==SUCCESS && values[0]==cases[k].input && (double)values[1]==cases[k].raw);
        values[0]=12345; values[1]=6789;
        CHECK(get(m,wrong,values)==FAILURE && values[0]==12345 && values[1]==6789);
        printf("%s n=%d raw=%.17g status=%d atomic=ok\n",cs?"CS":"ME",(int)cases[k].input,cases[k].raw,status);
        CHECK(finish(m)==SUCCESS);
    }
    return 0;
}
"#;
