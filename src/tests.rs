#[cfg(test)]
use std::collections::BTreeMap;

#[cfg(test)]
use toml::Table;

#[cfg(test)]
use crate::parsing;
#[cfg(test)]
use crate::parsing::PointValidationError;
#[cfg(test)]
use crate::solver;

#[test]
#[allow(non_snake_case)]
fn check_nalgebra() -> Result<(), ()> {
    use nalgebra as na;

    const SIN_60: f64 = 0.8660254038;
    let answer = na::DMatrix::from_row_slice(
        3,
        1,
        &[28.867513458962573, 28.867513458962573, -14.433756729481287],
    );

    let A = na::DMatrix::from_row_slice(
        3,
        3,
        &[SIN_60, SIN_60, 0f64, -SIN_60, 0f64, 0f64, 0f64, 0.5, 1f64],
    );
    let C = na::DMatrix::from_column_slice(3, 1, &[50.0, -25.0, 0f64]);

    let result = A.try_inverse().unwrap() * C;
    assert_eq!(result, answer);
    Ok(())
}

#[test]
fn read_points_from_file() -> Result<(), ()> {
    let file = std::fs::read_to_string(r"sample-problems\points-test.toml").unwrap();
    let data = file.parse::<Table>().unwrap();
    let points = {
        let a = data.get("points").unwrap();
        parsing::parse_points(a, &mut BTreeMap::new(), false)
    };
    panic!("{:?}", points);
}

#[test]
fn check_point_reading() -> Result<(), ()> {
    let file = std::fs::read_to_string(r"sample-problems\points-test.toml").unwrap();
    let toml_data = file.parse::<Table>().unwrap();

    let mut points = parsing::parse_points(
        toml_data.get("points").unwrap(),
        &mut BTreeMap::new(),
        false,
    )
    .into_values()
    .collect::<Vec<_>>();
    points.sort_by_key(|a| a.id());

    let mut answers: Vec<solver::Point2D> = vec![
        solver::Point2D::origin("A".into()),
        solver::Point2D::cartesian("B".into(), 2f64, 0f64),
        solver::Point2D::polar("C".into(), 2f64, 60f64),
        solver::Point2D::cartesian("D".into(), 1f64, 1f64),
        solver::Point2D::origin("E".into()),
        solver::Point2D::polar("F".into(), 1f64, 90f64),
    ];
    answers.sort_by_key(|a| a.id());

    for (calculated, ans) in points.iter().zip(answers.iter()) {
        if calculated.id() != ans.id() {
            eprintln!(
                "Parsed id {} does not match answer id {}. ",
                calculated.id(),
                ans.id()
            );
            eprintln!("Point {:?} should've been {:?}", calculated, ans);
            return Err(());
        }
        if calculated.coords() != ans.coords() {
            eprintln!(
                "Parsed location {:?} does not match answer location {:?}. ",
                calculated.coords(),
                ans.coords()
            );
            eprintln!("Point {:?} should've been {:?}", calculated, ans);
            return Err(());
        }
    }
    Ok(())
}

#[test]
fn check_point_validation() -> Result<(), ()> {
    let file = std::fs::read_to_string(r"sample-problems/error_points.toml").unwrap();
    let toml_data = file.parse::<Table>().unwrap();

    let mut points = parsing::parse_points(
        toml_data.get("points").unwrap(),
        &mut BTreeMap::new(),
        false,
    );

    if parsing::validate_points(&points).is_ok() {
        eprintln!("Point validation did not error!");
        return Err(());
    }
    points.remove(&solver::SolverID::new("bet"));

    if parsing::validate_points(&points).unwrap_err() != PointValidationError::DuplicatePosition {
        eprintln!("Point validation did not error when it should've!");
        return Err(());
    }
    Ok(())
}

#[test]
fn check_load_parsing() -> Result<(), ()> {
    // we currently generate too many unknown forces is the solver somehow
    todo!()
}
