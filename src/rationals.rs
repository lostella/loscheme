fn gcd(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let t = b;
        b = (a.abs() % b.abs()) * b.signum();
        a = t;
    }
    a
}

pub fn simplify(numerator: i64, denominator: i64) -> (i64, i64) {
    let d = gcd(numerator, denominator);
    (numerator / d, denominator / d)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gcd() {
        let cases = vec![
            (7, 3, 1),
            (7, -3, -1),
            (-7, 3, 1),
            (-7, -3, -1),
            (6, 4, 2),
            (6, -4, -2),
            (-6, 4, 2),
            (-6, -4, -2),
            (5, 5, 5),
            (5, -5, -5),
            (-5, 5, 5),
            (-5, -5, -5),
        ];
        for (a, b, c) in cases {
            assert_eq!(gcd(a, b), c)
        }
    }

    #[test]
    fn test_simplify() {
        let cases = vec![
            ((3, 2), (3, 2)),
            ((3, -2), (-3, 2)),
            ((-3, 2), (-3, 2)),
            ((-3, -2), (3, 2)),
            ((6, 4), (3, 2)),
            ((6, -4), (-3, 2)),
            ((-6, 4), (-3, 2)),
            ((-6, -4), (3, 2)),
        ];
        for ((a, b), (a1, b1)) in cases {
            assert_eq!(simplify(a, b), (a1, b1))
        }
    }
}
