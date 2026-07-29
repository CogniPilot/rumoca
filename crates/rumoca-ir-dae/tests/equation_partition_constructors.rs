use rumoca_core::{SourceMap, Span};
use rumoca_ir_dae::{Dae, DaeLiteral, DaeProvenance};

#[test]
fn remaining_scalar_partition_methods_construct_and_round_trip() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("scalar_partitions.mo", "initial equation 0; equation 0;");
    let owner = DaeProvenance::source(Span::from_offsets(source, 0, 18)).expect("real source span");
    let dae = Dae::construct(source_map, |dae| {
        let residual =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
        dae.initialization(|initialization| initialization.value_equation(owner, residual))?;
        dae.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .expect("initialization value and discrete-real residual partitions construct");

    dae.inspect(|view| {
        assert_eq!(view.initialization_equation_count(), 1);
        assert_eq!(view.discrete_real_equation_count(), 1);
    });
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.initialization_equation_count(), 1);
        assert_eq!(view.discrete_real_equation_count(), 1);
    });
}
