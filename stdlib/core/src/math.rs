pub fn clamp<T: Ord>(value: T, min: T, max: T) -> T {
    if value < min {
        min
    } else if value > max {
        max
    } else {
        value
    }
}

pub fn lerp(start: f64, end: f64, t: f64) -> f64 {
    start + (end - start) * t
}

pub fn approx_eq(a: f64, b: f64, epsilon: f64) -> bool {
    (a - b).abs() <= epsilon
}
