//! Analytic matched geometry; no Horn/quaternion fitting in this oracle.
pub(super) const N: usize = 14400;
type Point = [f64; 3];
const IDENTITY: [f64; 9] = [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0];

pub(super) struct Case {
    pub name: &'static str,
    pub source: Vec<Point>,
    pub target: Vec<Point>,
    pub enabled: Vec<f64>,
    pub count: f64,
    pub maximum_rms: f64,
    pub rotation: [f64; 9],
    pub translation: Point,
    pub reason: u32,
    pub valid: f64,
    pub invalid: f64,
    pub rank: Option<f64>,
}

fn rotation(axis: Point, angle: f64) -> [f64; 9] {
    let norm = axis.iter().map(|v| v * v).sum::<f64>().sqrt();
    let [x, y, z] = axis.map(|v| v / norm);
    let c = angle.cos();
    let s = angle.sin();
    let d = 1.0 - c;
    [
        c + x * x * d,
        x * y * d - z * s,
        x * z * d + y * s,
        y * x * d + z * s,
        c + y * y * d,
        y * z * d - x * s,
        z * x * d - y * s,
        z * y * d + x * s,
        c + z * z * d,
    ]
}

fn matched(name: &'static str, source: Vec<Point>, rotation: [f64; 9]) -> Case {
    let translation = [0.2, -0.35, 0.09];
    let target = source
        .iter()
        .map(|point| {
            std::array::from_fn(|axis| {
                translation[axis]
                    + (0..3)
                        .map(|k| rotation[3 * axis + k] * point[k])
                        .sum::<f64>()
            })
        })
        .collect();
    Case {
        name,
        source,
        target,
        enabled: vec![1.0; N],
        count: N as f64,
        maximum_rms: 0.02,
        rotation,
        translation,
        reason: 0,
        valid: N as f64,
        invalid: 0.0,
        rank: Some(3.0),
    }
}

fn rejected(mut case: Case, reason: u32) -> Case {
    case.reason = reason;
    case.rotation = IDENTITY;
    case.translation = [0.0; 3];
    case
}

fn points() -> Vec<Point> {
    (0..N)
        .map(|i| {
            [
                (i % 120) as f64 / 120.0 - 59.5 / 120.0,
                (i / 120) as f64 / 240.0 - 59.5 / 240.0,
                if (i % 2 == 0) == ((i / 120) % 2 == 0) {
                    0.13
                } else {
                    -0.13
                },
            ]
        })
        .collect()
}

pub(super) fn cases() -> Vec<Case> {
    let points = points();
    let r = rotation([0.3, -0.7, 0.5], 0.81);
    let mut result = vec![matched("full non-axis rigid transform", points.clone(), r)];
    let mut planar = matched(
        "full planar non-collinear",
        points.iter().map(|p| [p[0], p[1], 0.0]).collect(),
        r,
    );
    planar.rank = Some(2.0);
    result.push(planar);
    result.push(matched(
        "near half turn",
        points.clone(),
        rotation([1.0, 2.0, -3.0], std::f64::consts::PI - 1e-8),
    ));
    result.extend(noisy_cases(&points, r));
    result.extend(degenerate_cases(&points, r));
    result.extend(domain_cases(&points, r));
    result.extend(mask_cases(&points, r));
    let mut reflection = matched("proper reflection RMS refusal", points.clone(), r);
    reflection.target = points.iter().map(|p| [-p[0], p[1], p[2]]).collect();
    result.push(rejected(reflection, 6));
    let mut permissive = matched(
        "runtime RMS parameter admits proper reflection",
        points.clone(),
        [-1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, -1.0],
    );
    permissive.target = points.iter().map(|p| [-p[0], p[1], p[2]]).collect();
    permissive.translation = [0.0; 3];
    permissive.maximum_rms = 0.5;
    result.push(permissive);
    result.push(matched("recovery and repeat", points, r));
    result
}

fn noisy_cases(points: &[Point], rotation: [f64; 9]) -> Vec<Case> {
    [0.01, 0.03]
        .into_iter()
        .map(|magnitude| {
            let mut case = matched(
                if magnitude < 0.02 {
                    "known orthogonal residual"
                } else {
                    "finite RMS gate refusal"
                },
                points.to_vec(),
                rotation,
            );
            for (i, point) in case.target.iter_mut().enumerate() {
                point[2] += magnitude
                    * [1.0, -1.0, -1.0, 1.0][i % 4]
                    * if (i / 120) % 2 == 0 { 1.0 } else { -1.0 };
            }
            if magnitude < 0.02 {
                case
            } else {
                rejected(case, 6)
            }
        })
        .collect()
}

fn degenerate_cases(points: &[Point], rotation: [f64; 9]) -> Vec<Case> {
    let mut line = matched(
        "collinear refusal",
        points.iter().map(|p| [p[0], 0.0, 0.0]).collect(),
        rotation,
    );
    line.rank = Some(1.0);
    let mut near_line = matched(
        "relative rank nearly collinear refusal",
        points.iter().map(|p| [p[0], p[1] * 1e-6, 0.0]).collect(),
        rotation,
    );
    near_line.rank = Some(1.0);
    let mut coincident = matched("coincident refusal", vec![[0.1, 0.2, 0.3]; N], rotation);
    coincident.rank = None;
    let mut collapsed = matched(
        "collapsed target eigen-gap refusal",
        points.to_vec(),
        rotation,
    );
    collapsed.target.fill([0.1, 0.2, 0.3]);
    vec![
        rejected(line, 4),
        rejected(near_line, 4),
        rejected(coincident, 4),
        rejected(collapsed, 4),
    ]
}

fn domain_cases(points: &[Point], rotation: [f64; 9]) -> Vec<Case> {
    [
        ("empty observation", 0.0, 3),
        ("fractional count", N as f64 - 0.5, 1),
        ("oversized count", N as f64 + 1.0, 1),
        ("negative count", -1.0, 1),
        ("insufficient pairs", 2.0, 3),
    ]
    .into_iter()
    .map(|(name, count, reason)| {
        let mut case = matched(name, points.to_vec(), rotation);
        case.count = count;
        case.valid = if count == 2.0 { 2.0 } else { 0.0 };
        case.rank = None;
        rejected(case, reason)
    })
    .collect()
}

fn mask_cases(points: &[Point], rotation: [f64; 9]) -> Vec<Case> {
    let mut huge = matched(
        "finite huge coordinate refuses before covariance",
        points.to_vec(),
        rotation,
    );
    huge.source[N - 1][0] = 1e150;
    huge.valid -= 1.0;
    huge.invalid = 1.0;
    let mut masked = matched(
        "disabled NaN does not poison arithmetic",
        points.to_vec(),
        rotation,
    );
    masked.source[N - 1].fill(f64::NAN);
    masked.target[N - 1].fill(f64::NAN);
    masked.enabled[N - 1] = 0.0;
    masked.valid -= 1.0;
    let mut invalid = matched("invalid active pair flag", points.to_vec(), rotation);
    invalid.enabled[N - 1] = 0.5;
    invalid.valid -= 1.0;
    invalid.invalid = 1.0;
    vec![rejected(huge, 2), masked, rejected(invalid, 2)]
}
