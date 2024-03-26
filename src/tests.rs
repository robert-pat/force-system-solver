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
fn build_equations_test_one() -> Result<(), ()> {
    Ok(())
}
