#[test]
fn points_reading() {
    use crate::parsing::into_points;
    use crate::solver::Point2D;
    use crate::solver::SolverID;
    use std::collections::HashMap;
    use toml::Table;

    let file = r#"points = [
        ["A", "Origin"],
        ["B", "Cartesian", 2, 0],
        ["C", "Polar", 2, 60],
        ["D", "Origin"],
        ["E", "Cartesian", 1, 1],
        ["the tall tailed fox", "Polar", 1.7556, 37.113],
        ["Origin", "Origin"],
        ["Polar", "Cartesian", 2.4, 3.01],
    ]"#;
    let table = file.parse::<Table>().unwrap();
    let mut names = HashMap::new();
    let result = into_points(table.get("points").unwrap(), &mut names).unwrap();
    let expected = [
        Point2D::origin(SolverID::new("A")),
        Point2D::cartesian(SolverID::new("B"), 2.0, 0.0),
        Point2D::polar(SolverID::new("C"), 2.0, 60.0),
        Point2D::origin(SolverID::new("D")),
        Point2D::cartesian(SolverID::new("E"), 1.0, 1.0),
        Point2D::polar(SolverID::new("the tall tailed fox"), 1.7556, 37.113),
        Point2D::origin(SolverID::new("Origin")),
        Point2D::cartesian(SolverID::new("Polar"), 2.4, 3.01),
    ];

    assert_eq!(
        result.len(),
        expected.len(),
        "Mismatched number of expected vs result points!"
    );

    const THRESHOLD: f64 = 0.000001_f64;
    for point in expected {
        let (id, expected_pos) = (point.id(), point.pos());
        let name = names.get(&id).unwrap();
        let result_pos = result.get(&id).unwrap().pos();

        assert!(
            (expected_pos.0 - result_pos.0).abs() <= THRESHOLD,
            "{name} had different values: {expected_pos:?} vs {result_pos:?}"
        );
        assert!(
            (expected_pos.1 - result_pos.1).abs() <= THRESHOLD,
            "{name} had different values: {expected_pos:?} vs {result_pos:?}"
        );
    }
}

#[test]
fn structural_member_reading() {
    use crate::parsing::into_connections;
    use crate::solver::SolverID;
    use std::cmp::Ordering;
    use std::collections::HashMap;
    use std::collections::HashSet;
    use toml::Table;

    let file = r#"members = [
        ["A", "B"],
        ["A", "C"],
        ["B", "C"],
        ["D", "E"],
        ["G", "H"],
        ["Ins", "12"],
    ]"#;
    let table = file.parse::<Table>().unwrap();
    let mut n = HashMap::new();
    let result = into_connections(table.get("members").unwrap(), &mut n)
        .unwrap()
        .into_iter()
        .collect::<HashSet<_>>();
    let expected = [
        ("A", "B"),
        ("A", "C"),
        ("B", "C"),
        ("D", "E"),
        ("G", "H"),
        ("Ins", "12"),
    ]
    .into_iter()
    .map(|(a, b)| match a.cmp(b) {
        Ordering::Greater => (SolverID::new(a), SolverID::new(b)),
        Ordering::Equal => panic!("Declared zero-width member in expected results!"),
        Ordering::Less => (SolverID::new(b), SolverID::new(a)),
    })
    .collect::<Vec<_>>();

    assert_eq!(
        result.len(),
        expected.len(),
        "Mismatched number of expected vs result members!"
    );
    for item in expected {
        let message = match n.get(&item.0.concatenate(item.1)) {
            Some(n) => n.as_str(),
            None => "member not found in names",
        };
        assert!(
            result.contains(&item),
            "Expected item: {message}, but it was not found!"
        );
    }
}

#[test]
fn applied_load_reading() {
    use crate::parsing::into_loads;
    use crate::parsing::AppliedLoad;
    use crate::solver::{Direction2D, SolverID, VectorComponent};
    use std::collections::HashMap;
    use toml::Table;

    let file = r#"loads = [
        ["A", 25, "Polar", 90],
        ["B", 25, "Up"],
        ["C", 50, "Down"],
        ["FG", 1754, "Left"],
        ["xc", 0.0000571, "Polar", 13],
    ]"#;
    let table = file.parse::<Table>().unwrap();
    let mut n = HashMap::new();
    let results = into_loads(table.get("loads").unwrap(), &mut n)
        .unwrap()
        .into_values()
        .collect::<Vec<_>>();
    let expected = [
        AppliedLoad {
            id: SolverID::default(),
            at: SolverID::new("A"),
            dir: Direction2D::from_degrees(90.0),
            mag: VectorComponent::KnownExactly(25.0),
        },
        AppliedLoad {
            id: SolverID::default(),
            at: SolverID::new("B"),
            dir: Direction2D::up(),
            mag: VectorComponent::KnownExactly(25.0),
        },
        AppliedLoad {
            id: SolverID::default(),
            at: SolverID::new("C"),
            dir: Direction2D::down(),
            mag: VectorComponent::KnownExactly(50.0),
        },
        AppliedLoad {
            id: SolverID::default(),
            at: SolverID::new("FG"),
            dir: Direction2D::left(),
            mag: VectorComponent::KnownExactly(1754.0),
        },
        AppliedLoad {
            id: SolverID::default(),
            at: SolverID::new("xc"),
            dir: Direction2D::from_degrees(13.0),
            mag: VectorComponent::KnownExactly(0.0000571),
        },
    ];

    assert_eq!(
        results.len(),
        expected.len(),
        "Mismatched number of expected vs result loads!"
    );
    for item in expected {
        let cmp = results
            .iter()
            .any(|res| res.at == item.at && res.dir == item.dir && res.mag == item.mag);
        assert!(cmp, "Expected: {item:?} to appear in results: {results:#?}");
    }
}

#[test]
fn supports_reading() {
    use crate::parsing::into_supports;
    use crate::parsing::Support;
    use crate::solver::{Direction2D, SolverID};
    use std::collections::HashMap;
    use toml::Table;

    let file = r#"supports = [
        ["C", "Pin"],
        ["A", "Roller", "Down"],
        ["aosyb", "Pin"],
        ["qq", "Roller", "Right"],
    ]"#;
    let table = file.parse::<Table>().unwrap();
    let mut n = HashMap::new();
    let results = into_supports(table.get("supports").unwrap(), &mut n)
        .unwrap()
        .into_values()
        .collect::<Vec<_>>();
    let expected = [
        Support::Pin {
            at: SolverID::new("C"),
        },
        Support::Roller {
            at: SolverID::new("A"),
            dir: Direction2D::down(),
        },
        Support::Pin {
            at: SolverID::new("aosyb"),
        },
        Support::Roller {
            at: SolverID::new("qq"),
            dir: Direction2D::right(),
        },
    ];

    assert_eq!(
        results.len(),
        expected.len(),
        "Mismatch number of expected vs result supports!"
    );
    for item in expected {
        assert!(
            results.contains(&item),
            "Item {item:?} was not found in {results:#?}"
        );
    }
}
